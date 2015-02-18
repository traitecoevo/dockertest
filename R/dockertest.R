##' Write a dockerfile.  Currently system requirements are not at all
##' dealt with, which is sad.
##' @title Write Dockerfile
##' @param filename filename to write to, or \code{NULL} to return the
##' contents, invisibly.
##' @param path_package Path to the package root.  We'll try and
##' locate it if not specificied.
##' @export
##' @importFrom devtools as.package
##' @importFrom whisker whisker.render
dockerfile <- function(filename=NULL, path_package=NULL) {
  deps <- dependencies(path_package)
  p <- function(x) {
    if (length(x) <= 1) x else paste(c("", sort(x)), collapse="  \\\n    ")
  }

  template <- system.file("Dockerfile.whisker", package="dockertest",
                          mustWork=TRUE)
  str <- whisker.render(readLines(template),
                        list(dependencies=lapply(deps, p)))
  if (!is.null(filename)) {
    writeLines(str, filename)
  }
  invisible(str)
}

##' Prepare for a build by writing a dockerfile and copying scripts
##' into the build directory.
##' @title Prepare for docker build
##' @param path_build Path to the Dockerfile
##' @param path_package Path to the package
##' @export
prepare <- function(path_build=".", path_package=NULL) {
  str <- dockerfile(file.path(path_build, "Dockerfile"), path_package)
  write_scripts(path_build, path_package)
  copy_scripts_dir(path_build)
}

##' Build a docker container
##' @title Build a docker container
##' @param path_build Path to the Dockerfile
##' @param path_package Path to the package
##' @param use_cache Set to FALSE to skip docker's cache
##' @param prepare Rerun \code{\link{prepare}} before building the
##' image?
##' @export
build <- function(path_build=".", path_package=NULL,
                  use_cache=TRUE, prepare=TRUE) {
  if (prepare) {
    prepare(path_build, path_package)
  }
  tag <- docker_tagname(path_package)
  if (Sys.info()[["sysname"]] == "Darwin") {
    boot2docker_shellinit()
  }
  args <- c("build", "-t", tag,
            if (!use_cache) "--no-cache",
            path_build)
  system2("docker", args)
  unlink("scripts", recursive=TRUE)
}

write_scripts <- function(path_build=".", path_package=NULL) {
  boot2docker_str <-
    if (is_mac()) "$(boot2docker shellinit 2> /dev/null)" else ""
  path_package <- find_package_root(path_package)
  data <- list(tag=docker_tagname(path_package),
               boot2docker=boot2docker_str,
               source_dir=path_package)
  scripts <- c("build.sh", "launch.sh")
  for (s in scripts) {
    template <- system.file(paste0(s, ".whisker"),
                            package="dockertest", mustWork=TRUE)
    writeLines(whisker.render(readLines(template), data),
               file.path(path_build, s))
  }
  Sys.chmod(file.path(path_build, scripts), "0755")
  invisible(NULL)
}

copy_scripts_dir <- function(path_build=".") {
  file.copy(system.file("scripts", package="dockertest", mustWork=TRUE),
            path_build, recursive=TRUE)
  invisible(NULL)
}

boot2docker_shellinit <- function() {
  if (Sys.getenv("DOCKER_HOST") == "") {
    tmp <- system2("boot2docker", "shellinit", stdout=TRUE, stderr=FALSE)
    vars <- strsplit(sub("^\\s*export\\s+", "", tmp), "=", fixed=TRUE)
    if (!all(vapply(vars, length, integer(1)) == 2L)) {
      stop("Unexpected output from boot2docker shellinit")
    }

    var_name <- vapply(vars, function(x) x[[1]], character(1))
    var_val  <- as.list(vapply(vars, function(x) x[[2]], character(1)))
    names(var_val) <- var_name
    do.call("Sys.setenv", var_val)

    if (Sys.getenv("DOCKER_HOST") == "") {
      stop("Failed to set boot2docker variables")
    }
  }
}

docker_tagname <- function(path_package) {
  paste0("dockertest/",
         tolower(package_name(path_package)))
}

dependencies <- function(path_package=NULL) {
  config <- load_config(path_package)

  ## *Names* of packages that we depend on.  This is just the
  ## immediate set.
  ## NOTE: Being sneaky, using hidden function.
  pkg <- as_package(path_package)
  package_names <- devtools:::pkg_deps(pkg, dependencies=TRUE)[, "name"]

  ## System dependencies, based on this.  We traverse through packages
  ## that are installed on *this* system and look at the
  ## SystemRequirements field and translate that into a set of apt-get
  ## install targets.  Additional things in config are also added.
  deps_system <- dependencies_system(package_names, config, path_package)

  ## For packages, we take the list of required packages and split
  ## them up into github and non-github packages, based on config.
  deps_packages <- dependencies_packages(package_names, config, path_package)

  ## Some packages come from different places:
  repos <- package_repos(path_package)
  if (!is.null(repos)) {
    repos <- sprintf("--repos=%s", repos)
  }

  c(list(system=deps_system, repos=repos), deps_packages)
}

dependencies_system <- function(package_names, config, path_package=NULL) {
  ## These are required by the bootstrap:
  dockertest_system <- c("curl", "git", "libcurl4-openssl-dev", "ssh")

  ## Starting with our package dependencies we can identify the full
  ## list.
  dat <- package_dependencies_recursive(package_names)
  pkgs <- sort(unique(c(package_names, dat$name)))
  pkgs <- setdiff(pkgs, config[["system_ignore_packages"]])

  dat <- package_descriptions(pkgs)
  sys_reqs <- lapply(dat, description_field, "SystemRequirements")
  reqs <- system_requirements_apt_get(system_requirements_sanitise(sys_reqs))

  if (length(reqs$unresolved) > 0) {
    ## Resolving the name here is *ugly*, because I nuked the name
    ## already.  Most of these are a bit ugly anyway so should match
    ## easily.  For these, the best thing to do is to add something to
    ## .dockertest.yml that says:
    ## system_ignore_packages:
    ##   - <offending_package_name>
    ## system:
    ##   - <manually>
    ##   - <list>
    ##   - <deps>
    unresolved_name <- names(sys_reqs)[match(reqs$unresolved, sys_reqs)]
    unresolved_name[is.na(unresolved_name)] <- "<sorry, don't know>"
    msg <- sprintf("\t- %s: %s", unresolved_name, reqs$unresolved)
    message("Unresolved SystemRequirements:\n",
              paste(msg, collapse="\n"))
  }

  sort(unique(c(dockertest_system,
                reqs$resolved,
                config[["system"]])))
}

dependencies_packages <- function(package_names, config, path_package) {
  dockertest_packages <- c("devtools", "testthat")
  deps <- list(R=setdiff(union(dockertest_packages, package_names),
                 base_packages()))
  if (!is.null(config)) {
    deps$github <- config[["packages"]][["github"]]
    if (!is.null(deps$github)) {
      ## NOTE: don't allow removing devtools here though, because that
      ## will break the bootstrap (CRAN devtools is required for
      ## downloading github devtools if it's needed).
      i <- match(setdiff(sub("^.*/", "", deps$github), "devtools"),
                 deps$R)
      if (length(i) > 0L) {
        deps$R <- deps$R[-i]
      }
    }
  }
  deps
}

## This generates a set of repos, including any listed in the
## packageDescription file
package_repos <- function(path_package) {
  pkg <- as_package(path_package)
  if (is.null(pkg$additional_repositories)) {
    NULL
  } else {
    c("http://cran.rstudio.com",
      strsplit(pkg$additional_repositories, "\\s*,\\s*")[[1]])
  }
}

## No validation here yet.
load_config <- function(path_package=NULL) {
  config_file <- file.path(find_package_root(path_package),
                           ".dockertest.yml")
  defaults <- list(system_ignore_packages=NULL,
                   system=NULL,
                   packages=list(github=NULL))

  if (file.exists(config_file)) {
    ret <- yaml_read(config_file)
    modifyList(defaults, ret)
  } else {
    defaults
  }
}

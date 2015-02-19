##' Write a Dockerfile
##' @title Write Dockerfile
##' @param filename filename to write to, or \code{NULL} to return the
##' contents, invisibly.
##' @export
##' @importFrom devtools as.package
##' @importFrom whisker whisker.render
dockerfile <- function(filename=NULL) {
  path_project <- find_project_root(NULL)
  config <- load_config(path_project)
  deps <- dependencies(path_project, config)
  p <- function(x) {
    if (length(x) <= 1) x else paste(c("", sort(x)), collapse="  \\\n    ")
  }

  ## Repos needs to go in as a long arg:
  if (!is.null(deps$repos)) {
    deps$repos <- sprintf("--repos=%s", deps$repos)
  }

  template <- system.file("Dockerfile.whisker", package="dockertest",
                          mustWork=TRUE)
  str <- whisker.render(readLines(template),
                        list(image=config[["image"]],
                             dependencies=lapply(deps, p)))
  if (!is.null(filename)) {
    writeLines(str, filename)
  }
  invisible(str)
}

##' Prepare for a build by writing a dockerfile and copying scripts
##' into the build directory.
##' @title Prepare for docker build
##' @param path_build Path to the Dockerfile
##' @export
prepare <- function(path_build=".") {
  str <- dockerfile(file.path(path_build, "Dockerfile"))
  write_scripts(path_build)
}

##' Build a docker container
##' @title Build a docker container
##' @param path_build Path to the Dockerfile
##' @param use_cache Set to FALSE to skip docker's cache
##' @param prepare Rerun \code{\link{prepare}} before building the
##' image?
##' @export
build <- function(path_build=".", use_cache=TRUE, prepare=TRUE) {
  if (prepare) {
    prepare(path_build)
  }
  tag <- project_info()$tagname
  if (Sys.info()[["sysname"]] == "Darwin") {
    boot2docker_shellinit()
  }
  args <- c("build", "-t", tag,
            if (!use_cache) "--no-cache",
            path_build)
  system2("docker", args)
}

write_scripts <- function(path_build=".") {
  path_project <- find_project_root(NULL)
  info <- project_info()

  boot2docker_str <-
    if (is_mac()) "$(boot2docker shellinit 2> /dev/null)" else ""
  data <- list(tag=info$tagname,
               boot2docker=boot2docker_str,
               source_dir=info$path_project)
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

copy_scripts_dir <- function(path) {
  path_scripts <- system.file("scripts", package="dockertest", mustWork=TRUE)
  scripts <- dir(path_scripts, full.names=TRUE)
  file.copy(scripts, path)
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

## Things that should be configurable:
##   - name
##   - tagname
project_info <- function(path_project=NULL) {
  path_project <- find_project_root(path_project)
  path_package <- find_package_root(NULL, path_project, error=FALSE)
  is_package <- !is.null(path_package)
  if (is_package) {
    name <- devtools::as.package(path_package)$package
  } else {
    name <- basename(path_project)
  }
  tagname <- paste0("dockertest/", tolower(name))
  list(name=name,
       tagname=tagname,
       path_project=path_project,
       path_package=path_package,
       is_package=is_package)
}

dependencies <- function(path_project=NULL, config) {
  ## Determine the *names* of packages that we depend on
  ## (`package_names`).  This is just the immediate set; not including
  ## their dependencies yet.

  ## NOTE: Being sneaky, using hidden function `pkg_deps` to parse
  ## dependencies; need to write my own for this.
  info <- project_info(path_project)
  path_project <- info$path_project
  if (info$is_package) {
    pkg <- devtools::as.package(info$path_package)
    package_names <- devtools:::pkg_deps(pkg, dependencies=TRUE)[, "name"]
    ## Some packages come from different places:
    if (is.null(pkg$additional_repositories)) {
      repos <- NULL
    } else {
      repos <- c("http://cran.rstudio.com",
                 strsplit(pkg$additional_repositories, "\\s*,\\s*")[[1]])
    }
  } else {
    package_names <- character(0)
    repos <- NULL
  }

  ## For packages, we take the list of required packages and split
  ## them up into github and non-github packages, based on config.
  ## This function may identify additional github packages that are
  ## needed from .travis.yml and from .dockertest.yml
  deps_packages <- dependencies_packages(package_names, config, path_project)

  ## Recompute the list of package names here, including any ones that
  ## we added.
  package_names <- c(deps_packages$R,
                     github_package_name(deps_packages$github))

  ## Gather metadata information for all CRAN packages and all
  ## referenced github packages.  This uses crandb, and I need to work
  ## out a way of gracefully expiring that information.  The github
  ## bits are redownloaded each time.
  package_info <- fetch_PACKAGES(deps_packages$github)

  ## System dependencies, based on this.  We use
  ##   1. github information
  ##   2. CRAN information
  ##   3. local package installation information
  ## in decending order of preference.  We then try to coerce this
  ## into a set of suitable packages for travis.  Hints in
  ## .dockertest.yml and .travis.yml may come in helpful here.
  deps_system <- dependencies_system(package_names, package_info,
                                     config, path_project)

  c(list(system=deps_system, repos=repos), deps_packages)
}

dependencies_system <- function(package_names, package_info,
                                config, path_project=NULL) {
  ## These are required by the bootstrap:
  dockertest_system <- c("curl", "git", "libcurl4-openssl-dev", "ssh")

  ## Starting with our package dependencies we can identify the full
  ## list.

  dat <- package_dependencies_recursive(package_names, package_info)
  pkgs <- sort(unique(c(package_names, dat$name)))
  pkgs <- setdiff(pkgs, config[["system_ignore_packages"]])
  pkgs <- setdiff(pkgs, base_packages())

  ok <- pkgs %in% rownames(package_info)
  sys_reqs <- character(0)
  if (any(ok)) {
    sys_reqs <- c(sys_reqs, package_info[pkgs[ok], "SystemRequirements"])
  }
  if (any(!ok)) {
    ## Failed to find the requirements in the database, so try
    ## locally:
    f <- function(package_description) {
      ret <- description_field(package_description, "SystemRequirements")
      if (is.null(ret)) {
        NA_character_
      } else {
        ret
      }
    }
    pkgs_msg <- lapply(pkgs[!ok], package_descriptions)
    names(pkgs_msg) <- pkgs[!ok]
    sys_reqs <- c(sys_reqs, vapply(pkgs_msg, f, character(1)))
  }

  ## Convert impossible to understand requirements:
  sys_reqs <- system_requirements_sanitise(sys_reqs)
  ## And convert those into apt get requirements:
  sys_reqs <- system_requirements_apt_get(sys_reqs)
  ## Merge in travis' apt get requirements:
  sys_reqs_travis <- system_requirements_travis(path_project)
  if (!is.null(sys_reqs_travis)) {
    sys_reqs$resolved <- sort(unique(c(sys_reqs$resolved, sys_reqs_travis)))
  }

  ## Provide a message (but not a warning) about system requirements
  ## that might be unsatisified.
  if (length(sys_reqs$unresolved) > 0) {
    unresolved_name <- names(sys_reqs$unresolved)
    unresolved_pkg  <- sapply(sys_reqs$unresolved, paste, collapse=", ")
    msg <- paste(sprintf("\t- %s: %s", unresolved_name, unresolved_pkg),
                 collapse="\n")
    msg <- paste("Unresolved SystemRequirements:", msg,
                 "Suppress with .dockertest.yml:system_ignore_packages",
                 sep="\n")
    message(msg)
  }

  sort(unique(c(dockertest_system,
                sys_reqs$resolved,
                config[["system"]])))
}

dependencies_packages <- function(package_names, config, path_project) {
  dockertest_packages <- c("devtools", "testthat")
  ## Start with all the packages:
  deps_R <- setdiff(union(dockertest_packages, package_names),
                    base_packages())
  deps_github <- unique(c("richfitz/dockertest",
                          packages_github_travis(path_project),
                          config[["packages"]][["github"]]))
  deps <- list(R=deps_R, github=deps_github)

  if (length(deps$github) > 0L) {
    ## NOTE: don't allow removing devtools here though, because that
    ## will break the bootstrap (CRAN devtools is required for
    ## downloading github devtools if it's needed).
    i <- deps$R %in% setdiff(github_package_name(deps$github), "devtools")
    if (any(i)) {
      deps$R <- deps$R[!i]
    }
  }
  deps
}

## No validation here yet.
load_config <- function(path_project=NULL) {
  ## We'll look in the local directory and in the package root.
  config_file_local <- ".dockertest.yml"
  config_file_package <- file.path(find_project_root(path_project),
                                   ".dockertest.yml")
  if (file.exists(config_file_local)) {
    ## Ideally here we'd merge them, but that's hard to do.
    if (config_file_local != config_file_package &&
        file.exists(config_file_package)) {
      warning("Ignoring root .dockertest.yml", immediate.=TRUE)
    }
    config_file <- config_file_local
  } else {
    config_file <- config_file_package
  }
  defaults <- list(system_ignore_packages=NULL,
                   system=NULL,
                   packages=list(github=NULL),
                   image="r-base")
  if (file.exists(config_file)) {
    ret <- yaml_read(config_file)
    modifyList(defaults, ret)
  } else {
    defaults
  }
}

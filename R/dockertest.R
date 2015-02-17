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
    if (is.null(x)) x else paste(sort(x), collapse="  \\\n    ")
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
  tag <- tagname(path_package)
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
  if (is.null(path_package)) {
    path_package <- find_package_root()
  }
  data <- list(tag=tagname(path_package),
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

find_package_root <- function() {
  root <- normalizePath("/", mustWork=TRUE)
  f <- function(path) {
    if (file.exists(file.path(path, "DESCRIPTION"))) {
      return(path)
    }
    if (normalizePath(path, mustWork=TRUE) == root) {
      stop("Hit the root without finding a package")
    }
    Recall(file.path("..", path))
  }
  normalizePath(f("."), mustWork=TRUE)
}

package_name <- function(path_package=NULL) {
  if (is.null(path_package)) {
    path_package <- find_package_root()
  }
  devtools::as.package(path_package)$package
}

package_deps <- function(path_package=NULL) {
  if (is.null(path_package)) {
    path_package <- find_package_root()
  }
  pkg <- as.package(path_package)
  ## NOTE: Being sneaky, using hidden function.
  devtools:::pkg_deps(pkg, dependencies=TRUE)[, "name"]
}

tagname <- function(path_package) {
  paste0("dockertest/", package_name(path_package))
}

is_mac <- function() {
  Sys.info()[["sysname"]] == "Darwin"
}

base_packages <- function() {
  rownames(installed.packages(priority=c("base", "recommended")))
}

dependencies <- function(path_package=NULL) {
  config <- load_config(path_package)

  deps_pkg <- package_deps(path_package)
  deps_dockertest_packages <- c("devtools", "testthat")
  deps_dockertest_system <- c("curl", "git", "libcurl4-openssl-dev", "ssh")

  deps <- list(CRAN=setdiff(union(deps_dockertest_packages, deps_pkg),
                 base_packages()),
               system=union(deps_dockertest_system, config$system))

  if (!is.null(config)) {
    deps$github <- config$packages$github
    if (!is.null(deps$github)) {
      ## TODO: don't allow removing devtools here though, because that
      ## will break the bootstrap.
      i <- match(setdiff(sub("^.*/", "", deps$github), "devtools"),
                 deps$CRAN)
      if (length(i) > 0L) {
        deps$CRAN <- deps$CRAN[-i]
      }
    }
  }
  deps
}

load_config <- function(path_package=NULL) {
  if (is.null(path_package)) {
    path_package <- find_package_root()
  }
  config <- file.path(path_package, ".dockertest.yml")
  if (file.exists(config)) {
    yaml_read(config)
  } else {
    NULL
  }
}

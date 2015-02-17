##' Write a dockerfile
##' @title Write Dockerfile
##' @param path_package Path to the package root.  We'll try and
##' locate it if not specificied.
##' @export
##' @importFrom devtools as.package
##' @importFrom whisker whisker.render
dockerfile <- function(path_package=NULL) {
  if (is.null(path_package)) {
    path_package <- find_package_root()
  }

  deps <- paste(package_deps(path_package), collapse=", ")
  template <- system.file("Dockerfile.whisker", package="dockertest",
                          mustWork=TRUE)
  whisker.render(readLines(template),
                 list(dependencies=deps))
}

##' Build a docker container
##' @title Build a docker container
##' @param path_build Path to the Dockerfile
##' @param path_package Path to the package
##' @param use_cache Set to FALSE to skip docker's cache
##' @export
build <- function(path_build=".", path_package=NULL, use_cache=TRUE) {
  tag <- tagname(path_package)
  if (Sys.info()[["sysname"]] == "Darwin") {
    boot2docker_shellinit()
  }
  args <- c("build", "-t", tag,
            if (!use_cache) "--no-cache",
            path_build)
  system2("docker", args)
}

copy_scripts <- function(path_build=".", path_package=NULL) {

  data <- list(tag=tagname(path_package),
               boot2docker=if (is_mac()) "$(boot2docker shellinit)" else "")
  scripts <- c("build.sh", "launch.sh")
  for (s in scripts) {
    template <- system.file(paste0(s, ".whisker"),
                            package="dockertest", mustWork=TRUE)
    writeLines(whisker.render(readLines(template), data),
               file.path(path_build, s))
  }
  Sys.chmod(file.path(path_build, scripts), "0755")
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

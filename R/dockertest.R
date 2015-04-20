##' Build a docker container
##' @title Build a docker container
##' @param type Type of container to build.  Valid options are "test",
##' "run" and "production" (last one only for packages).
##' @param use_cache Set to FALSE to skip docker's cache
##' @param prepare Rerun \code{\link{prepare}} before building the
##' image?
##' @export
build <- function(type="test", prepare=TRUE, use_cache=TRUE) {
  info <- project_info(type)
  if (prepare) {
    prepare(info)
  }
  dockerfile <- file.path(info$path_build, "Dockerfile")
  path <- if (info$inplace) info$path_project else "."
  docker_build(path, dockerfile, info$tagname, use_cache)
}

##' Prepare for a build by writing a dockerfile and copying scripts
##' into the build directory.
##' @title Prepare for docker build
##' @param info Result of \code{project_info}, which is not exported,
##' so that's a bit mean.
##' @export
prepare <- function(info) {
  dir.create(info$path_build, FALSE, TRUE)
  clone_local(info)
  if (!info$inplace) {
    clone_self(info)
  }
  format_docker(dockerfile_dockertest(info),
                file.path(info$path_build, "Dockerfile"))
  write_launch_script(info)
}

## TODO: Need to get the simple hooks in here when info$type is true:
##   R (done)
##   R_test (easy)
##   bash (done)
##   check
##   test
##   devtools_check
write_launch_script <- function(info) {
  if (is_mac()) {
    boot2docker <- "$(boot2docker shellinit 2> /dev/null)"
  } else {
    boot2docker <- NULL
  }
  if (info$local_filesystem) {
    volume_map <- sprintf("-v %s:/src", info$path_project)
  } else {
    volume_map <- ""
  }

  dest <- file.path(info$path_build, "launch.sh")
  str <- c("#!/bin/bash",
           "set -e",
           boot2docker,
           sprintf("docker run %s -it %s $*", volume_map, info$tagname))
  writeLines(str, dest)
  Sys.chmod(dest, "0755")

  invisible(NULL)
}

## This one is used by dockertest to copy the contents of the scripts
## directory over.
copy_scripts_dir <- function(path) {
  path_scripts <- system.file("scripts", package="dockertest", mustWork=TRUE)
  scripts <- dir(path_scripts, full.names=TRUE)
  file.copy(scripts, path)
  invisible(NULL)
}

## This is used to clone local sources into the directory that we
## build from so that can get them into the container nicely.
clone_local <- function(info) {
  local_paths <- info$config$packages$local
  if (length(local_paths) == 0L) {
    return()
  }

  dest_local <- "local"
  if (file.exists(dest_local)) {
    unlink(dest_local, recursive=TRUE)
  }
  dir.create(dest_local, FALSE, TRUE)
  add_to_gitignore(dest_local)

  paths_dest <- file.path(dest_local,
                          local_package_name(local_paths))
  for (i in seq_along(local_paths)) {
    git_clone(local_paths[[i]], paths_dest[[i]])
    unlink(file.path(paths_dest[[i]], ".git"), TRUE)
  }
}

clone_self <- function(info) {
  dest_self <- "self"
  if (file.exists(dest_self)) {
    unlink(dest_self, recursive=TRUE)
  }
  dir.create(dest_self, FALSE, TRUE)
  add_to_gitignore(dest_self)
  git_clone(info$path_project, dest_self)
  if (!isTRUE(info$config$keep_git)) {
    unlink(file.path(dest_self, ".git"), TRUE)
  }
}

## Things that should be configurable:
##   - name
##   - tagname
##   - username (currently dockertest)
## This might also be the place to load the config file?
## TODO: The line here between "config" and "info" is now totally
## stuffed, so just copy all the config stuff directly into list and
## nuke any mentions of info$config.
project_info <- function(type, path_project=NULL) {
  path_project <- find_project_root(path_project)
  path_package <- find_package_root(NULL, path_project, error=FALSE)
  is_package <- !is.null(path_package)
  if (is_package) {
    name <- devtools::as.package(path_package)$package
  } else {
    name <- basename(path_project)
  }

  ret <- list(name=name,
              type=type,
              path_project=path_project,
              path_package=path_package,
              is_package=is_package,
              local_filesystem=type == "test",
              keep_git=FALSE)
  ret$install_package <- is_package && type != "test"

  ret$config <- load_config(ret$path_project)

  if (!is.null(ret$config$packages$local)) {
    ret$config$packages$local <-
      normalizePath(file.path(path_project, ret$config$packages$local),
                    mustWork=TRUE)
  }

  if (!is.null(ret$config[["names"]][[type]])) {
    ret$tagname <- ret$config[["names"]][[type]]
  } else {
    ret$tagname <- sprintf("dockertest/%s-%s", tolower(name), tolower(type))
  }

  ret$inplace <- ret$config$inplace

  ## TODO: All references to the build path are probably wrong now?
  ret$path_build <- sub("^.*/", "", ret$tagname)

  if (ret$inplace) {
    ## Get the path to the local sources *relative to the project*.
    ## TODO: This is a *path difference*, see project_info_remake()
    wd <- getwd()
    rel <- substr(wd, nchar(ret$path_project) + 1L, nchar(wd))
    rel <- sub("^/", "", rel)
    ret$path_local <- file.path(rel, "local")
    ret$path_self <- "."
  } else {
    ret$path_local <- "local"
    ret$path_self  <- "self"
  }

  class(ret) <- "dockertest"

  ret
}

## No validation here yet.
load_config <- function(path_project=NULL) {
  ## We'll look in the local directory and in the package root.
  config_file <- "dockertest.yml"
  defaults <- list(system_ignore_packages=NULL,
                   system=NULL,
                   packages=list(github=NULL, local=NULL),
                   image="r-base",
                   names=NULL,
                   inplace=FALSE,
                   deps_only=TRUE)
  if (file.exists(config_file)) {
    ret <- yaml_read(config_file)
    modifyList(defaults, ret)
  } else {
    defaults
  }
}

add_project_deps <- function(info) {
  info <- add_dockertest_deps(info)
  config <- info$config

  config$packages$github <-
    union(config$packages$github,
          packages_github_travis(info$path_project))
  config$system <-
    union(config$system,
          system_requirements_travis(info$path_project))

  if (info$is_package) {
    pkg <- devtools::as.package(info$path_package)
    package_names <- devtools:::pkg_deps(pkg, dependencies=TRUE)[, "name"]
  } else {
    package_names <- character(0)
  }
  config$packages$R <- union(config$packages$R, package_names)

  info$config <- config
  info
}

## These are the dependencies that dockertest needs to bootstrap
## everything, ad added to the config list
add_dockertest_deps <- function(info) {
  config <- info$config
  config$system <- union(config$system,
                         c("curl", "ca-certificates", "git",
                           "libcurl4-openssl-dev", "ssh"))
  config$packages$github <- union(config$packages$github,
                                  "richfitz/dockertest")
  config$packages$R <- union(config$packages$R, "devtools")
  info$config <- config
  info
}

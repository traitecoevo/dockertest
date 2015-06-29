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
  local_paths <- info$config$r_local_packages
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

  if (!is.null(ret$config$r_local_packages)) {
    ret$config$r_local_packages <-
      normalizePath(file.path(path_project, ret$config$r_local_packages),
                    mustWork=TRUE)
  }

  ret <- add_modules(ret)

  if (!is.null(ret$config[["names"]][[type]])) {
    ret$tagname <- ret$config[["names"]][[type]]
  } else {
    ret$tagname <- sprintf("dockertest/%s-%s", tolower(name), tolower(type))
  }

  ret$inplace <- ret$config$inplace

  ## TODO: All references to the build path are probably wrong now?
  ret$path_build <- sub("^.*/", "", ret$tagname)

  ## TODO: why does inplace affect local as well as self?
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
  defaults <- list(
    ## Image to build from:
    image="r-base",
    ## System dependencies via apt:
    apt_packages=NULL,
    ## Use unstable?
    apt_unstable=FALSE,
    ## R package dependencies from CRAN, github and local sources:
    r_packages=NULL,
    r_github_packages=NULL,
    r_local_packages=NULL,
    ## Packages to ignore system dependencies from:
    system_ignore_packages=NULL,
    ## Tag names for generated images:
    names=NULL,
    ## I'm not really sure anymore
    inplace=FALSE,
    ## Don't include the sources of this package
    deps_only=TRUE)
  if (file.exists(config_file)) {
    ret <- yaml_read(config_file)
    ret <- modifyList(defaults, ret)
  } else {
    ret <- defaults
  }
  ## Always include the "dockertest_bootstrap" module which includes
  ## dependencies needed to get started...
  ret$modules <- union("dockertest_bootstrap", ret$modules)
  ret
}

add_project_deps <- function(info) {
  config <- info$config

  config$r_github_packages <-
    union(config$r_github_packages,
          packages_github_travis(info$path_project))
  config$apt_packages <-
    union(config$apt_packages,
          system_requirements_travis(info$path_project))

  if (info$is_package) {
    pkg <- devtools::as.package(info$path_package)
    package_names <- pkg_deps(pkg, dependencies=TRUE)[, "name"]
  } else {
    package_names <- character(0)
  }
  config$r_packages <- union(config$r_packages, package_names)

  info$config <- config
  info
}

add_modules <- function(info) {
  modules <- info$config$modules
  if (is.null(modules)) {
    return(info)
  }

  modules_files <- paste0(modules, ".yml")
  ok <- file.exists(modules_files)
  if (any(!ok)) {
    modules_files[!ok] <- system.file(file.path("modules", modules_files[!ok]),
                                      package="dockertest",
                                      mustWork=TRUE)
  }

  for (f in modules_files) {
    x <- read_module(f)
    ## load module github packages and commands *first*:
    info$config$apt_packages <- union(x$apt_packages, info$config$apt_packages)
    info$config$r_packages <-
      union(x$r_packages, info$config$r_packages)
    info$config$r_github_packages <-
      union(x$r_github_packages, info$config$r_github_packages)
    info$config$commands <- union(x$commands, info$config$commands)
    info$config$system_ignore_packages <-
      union(x$system_ignore_packages,
            info$config$system_ignore_packages)
  }

  info
}

read_module <- function(filename) {
  dat <- yaml_read(filename)
  ok <- c("apt_packages", "r_packages", "r_github_packages",
          "system_ignore_packages", "commands")
  err <- setdiff(names(dat), ok)
  if (length(err) > 0L) {
    warning(sprintf("Unknown fields in module %s\n\t%s",
                    filename, paste(err, collapse="\n\t")),
            immediate.=TRUE)
  }
  dat
}

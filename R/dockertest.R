##' Prepare for a build by writing a dockerfile and copying scripts
##' into the build directory.
##' @title Prepare for docker build
##' @export
prepare <- function(info) {
  dir.create(info$path_build, FALSE, TRUE)
  clone_local(info, load_config())

  if (info$type == "run") {
    prepare_run(info)
  } else if (info$type == "production") {
    prepare_production(info)
  } else {
    prepare_test(info)
  }
}

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
  docker_build(info$path_build, info$tagname, use_cache)
}

write_scripts <- function(info) {
  boot2docker_str <-
    if (is_mac()) "$(boot2docker shellinit 2> /dev/null)" else ""

  ## TODO: This is temporary for now at least.
  ## TODO: as in build(), assume that we're testing unless explicitly
  ## run.
  ## TODO: Request this in the info.
  if (info$type == "test") {
    volume_map <- sprintf("-v %s:/src", info$path_project)
  } else {
    volume_map <- ""
  }
  data <- list(tagname=info$tagname,
               boot2docker=boot2docker_str,
               volume_map=volume_map)
  scripts <- c("build.sh", "launch.sh")
  for (s in scripts) {
    template <- system.file(paste0(s, ".whisker"),
                            package="dockertest", mustWork=TRUE)
    writeLines(whisker.render(readLines(template), data),
               file.path(info$path_build, s))
  }
  Sys.chmod(file.path(info$path_build, scripts), "0755")
  invisible(NULL)
}

copy_scripts_dir <- function(path) {
  path_scripts <- system.file("scripts", package="dockertest", mustWork=TRUE)
  scripts <- dir(path_scripts, full.names=TRUE)
  file.copy(scripts, path)
  invisible(NULL)
}

clone_local <- function(info, config) {
  local_paths <- config$packages$local
  if (length(local_paths) == 0L) {
    return()
  }

  dest_local <- file.path(info$path_build, ".local")
  if (file.exists(dest_local)) {
    unlink(dest_local, recursive=TRUE)
  }
  dir.create(dest_local, FALSE, TRUE)
  add_to_gitignore(dest_local)

  paths_dest <- file.path(dest_local, local_package_name(local_paths))
  for (i in seq_along(local_paths)) {
    git_clone(local_paths[[i]], paths_dest[[i]])
  }
}

## Things that should be configurable:
##   - name
##   - tagname
##   - username (currently dockertest)
## This might also be the place to load the config file?
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
              is_package=is_package)

  ret$tagname <- make_tagname(type, ret)
  ret$path_build <- sub("^.*/", "", ret$tagname)

  ret
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
                   packages=list(github=NULL, local=NULL),
                   image="r-base")
  if (file.exists(config_file)) {
    ret <- yaml_read(config_file)
    modifyList(defaults, ret)
  } else {
    defaults
  }
}

add_project_deps <- function(info, config) {
  config <- add_dockertest_deps(config)

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

  config
}

add_dockertest_deps <- function(config) {
  config$system <- union(config$system,
                         c("curl", "ca-certificates", "git",
                           "libcurl4-openssl-dev", "ssh"))
  config$packages$github <- union(config$packages$github,
                                  "richfitz/dockertest")
  config$packages$R <- union(config$packages$R, "devtools")
  config
}

make_tagname <- function(type, info) {
  if (is.null(info$tagname)) {
    sprintf("dockertest/%s-%s", tolower(info$name), type)
  } else {
    info$tagname
  }
}

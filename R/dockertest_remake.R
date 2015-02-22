##' Build a docker container for a a remake project
##' @title Build a docker container for a a remake project
##' @param target Target in remake to run.  This becomes the "type"
##' (for \code{\link{build}}) and gets translated into the tagname,
##' which can be remapped via the \code{names:} field in
##' \code{dockertest.yml}.  The target "clean" is special and will
##' unpack the project but not run any remake target.
##' @param prepare Rerun \code{\link{prepare_remake}} before building
##' the image?
##' @param use_cache Set to FALSE to skip docker's cache
##' @export
build_remake <- function(target="clean", prepare=TRUE, use_cache=TRUE) {
  info <- project_info_remake(target)
  if (prepare) {
    prepare_remake(info)
  }
  dockerfile <- file.path(info$path_build, "Dockerfile")
  path <- if (info$inplace) info$path_project else "."
  docker_build(path, dockerfile, info$tagname, use_cache)
}

##' Prepare for a build by writing a dockerfile and copying scripts
##' into the build directory.  This will eventually merge in with the
##' other prepare functions, but it can't easily because it follows a
##' slightly different pattern.
##' @title Prepare for docker build
##' @param info Result of \code{project_info_remake}, which is not
##' exported, so that's a bit mean.
##' @export
prepare_remake <- function(info) {
  dir.create(info$path_build, FALSE, TRUE)
  ## clone_local(info) # can't use at present
  if (!info$inplace) {
    clone_self(info)
  }
  format_docker(dockerfile_remake(info), # different to prepare()
                file.path(info$path_build, "Dockerfile"))
  write_launch_script(info)
}

dockerfile_remake <- function(info) {
  if (is.null(info$remake_target)) {
    dockerfile_remake_clean(info)
  } else {
    dockerfile_remake_run(info)
  }
}

dockerfile_remake_clean <- function(info) {
  if (is.null(info$path_remake)) {
    workdir <- info$name
  } else {
    workdir <- file.path(info$name, info$path_remake)
  }

  path <- file.path("/root", info$name)
  copy_sources <- docker_copy_sources(path,
                                      info$local_filesystem,
                                      info$path_self)
  c(list(),
    docker_FROM("richfitz/remake"),
    docker_apt_get_install(info$config$system),
    docker_r('remake::install_remake("/usr/local/bin")'),
    copy_sources,
    docker_WORKDIR(workdir),
    docker_r("remake::install_missing_packages()"),
    docker_CMD("bash"))
}

## Much simpler:
dockerfile_remake_run <- function(info) {
  c(list(),
    docker_FROM(project_info("clean")$tagname),
    docker_RUN(paste("remake", info$remake_target)))
}

project_info_remake <- function(target="clean", remake_file="remake.yml") {
  info <- project_info(target)
  info$remake_target <- if (target == "clean") NULL else target

  remake_file <- "remake.yml"
  path_remake <- find_remake_root(NULL, info$path_project,
                                  remake_file)
  remake_file_full <- file.path(path_remake, remake_file)

  ## TODO: For now, we can't do this:
  ##   remake::read_remake_file(remake_file_full)
  ## So we'll do this, which misses dependent remake files
  ## TODO: Also misses target-specific packages.
  dat <- yaml_read(remake_file_full)

  ## This gives us most of what we need.

  ## First, we set the image to be against remake.
  info$config$image <- "richfitz/remake"

  ## Then, we add packages (need to process the github ones later?)
  info$config$packages$R <- dat$packages

  remake_sources <- file.path(path_remake, "remake_sources.yml")
  sources <- remake:::read_remake_packages(remake_sources)
  if (!is.null(sources)) {
    ok <- vapply(sources, function(x) x$source == "github",
                 logical(1))
    if (!all(ok)) {
      stop("Non-github sources not handled yet")
    }
    info$config$packages$github <-
      unname(vapply(sources[ok], function(x) x$repo, character(1)))
  }

  ## Target-specific packages:
  packages_target <- unlist(lapply(dat$targets, function(x) x$packages))
  if (length(packages_target) > 0L) {
    info$config$packages$R <- union(info$config$packages$R,
                                    packages_target)
  }

  ## Need to get system deps here, still, but only because we want
  ## *system* information; process that.
  info$config$system <- dockertest_dependencies(info)$system

  ## Path to remake, relative to that of the *project*:
  if (path_remake == info$path_project) {
    info$path_remake <- NULL
  } else {
    ## TODO: This is a *path difference*, see project_info()
    rel <- substr(path_remake,
                  nchar(info$path_project) + 1L, nchar(path_remake))
    ## I believe that there is never a trailing slash so we'd be
    ## better doing +2L but I'd rather do it this way:
    info$path_remake <- sub("^/", "", rel)
  }

  class(info) <- c("dockertest_remake", class(info))
  info
}

find_remake_root <- function(path_remake, path_project,
                             remake_file="remake.yml",
                             error=TRUE) {
  path_project <- find_project_root(path_project)
  if (is.null(path_remake)) {
    find_file_descend(remake_file, path_project, error)
  } else {
    path_remake
  }
}

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
  prepare_remake(info)
  docker_build(info$path_build, info$tagname, TRUE)
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
  if (!is.null(info$config$source)) {
    prepare_run_clone(info)
  }
  writeLines(dockerfile_remake(info),
             file.path(info$path_build, "Dockerfile"))
  ## TODO: I need to write different scripts though, with different
  ## targets.  We'd ideally have a make target, but that should leave
  ## you in R or bash after running...
  write_scripts(info)
}

dockerfile_remake <- function(info) {
  if (is.null(info$path_remake)) {
    workdir <- info$name
  } else {
    workdir <- file.path(info$name, info$path_remake)
  }

  if (is.null(info$remake_target)) {
    make <- NULL
  } else {
    make <- sprintf("r -e 'remake::make(\"%s\")'", info$remake_target)
  }

  if (is.null(info$config$source)) {
    clone <- docker_COPY("src", info$name)
  } else {
    clone <- docker_RUN(sprintf("git clone %s %s",
                                info$config$source, info$name))
  }

  ## NOTE: There are really two options here; we could make standalone
  ## things  (that's what I've opted to do here) or we could make
  ## containers that build the project work `FROM` the base
  ## container.  Either way docker will reuse things
  commands <- c(
    list(),
    docker_FROM("richfitz/remake"),
    docker_apt_get_install(info$config$system),
    clone,
    docker_WORKDIR(workdir),
    docker_RUN("r -e 'remake::install_missing_packages()'"),
    if (!is.null(make)) docker_RUN(make)
  )
  format_docker(commands)
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

  remake_sources <- file.path(path_remake, "remake_sources.yml")
  sources <- remake:::read_remake_packages(remake_sources)

  ## This gives us most of what we need.

  ## First, we set the image to be against remake.
  info$config$image <- "richfitz/remake"

  ## Then, we add packages (need to process the github ones later?)
  info$config$packages$R <- dat$packages
  if (!is.null(sources)) {
    stop("Not yet handled")
  }

  ## Need to get system deps here, still, but only because we want
  ## *system* information; process that.
  info$config$system <- dockertest_dependencies(info)$system
  info$path_remake <- path_remake

  ## Path to remake, relative to that of the *project*:
  if (path_remake == info$path_project) {
    info$path_remake <- NULL
  } else {
    rel <- substr(path_remake,
                  nchar(info$path_project) + 1L, nchar(path_remake))
    ## I believe that there is never a trailing slash so we'd be
    ## better doing +2L but I'd rather do it this way:
    info$path_remake <- sub("^/", "", rel)
  }

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

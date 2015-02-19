##' Prepare for a build by writing a dockerfile and copying scripts
##' into the build directory.
##' @title Prepare for docker build
##' @param suffix Suffix used to generate the tag and directory name
##' @export
prepare <- function(suffix="-test") {
  if (prepare) {
    ## This hard codes functions and suffixes together in a way that
    ## is particularly unnice.
    if (suffix == "-run") {
      prepare_run(suffix)
    } else {
      prepare_test(suffix)
    }
  }
}

##' Build a docker container
##' @title Build a docker container
##' @param suffix Suffix to use;  this will be passed through to
##' \code{\link{prepare}} and also used to construct the tag.
##' @param use_cache Set to FALSE to skip docker's cache
##' @param prepare Rerun \code{\link{prepare}} before building the
##' image?
##' @export
build <- function(suffix, prepare=TRUE, use_cache=TRUE) {
  info <- project_info(suffix)
  if (prepare) {
    prepare(suffix)
  }
  docker_build(info$path_build, info$tagname, use_cache)
}

##' @importFrom whisker whisker.render
dockerfile_test <- function(info) {
  config <- load_config(info$path_project)
  deps <- dependencies(info$path_project, config)
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
  invisible(str)
}

prepare_test <- function(suffix="-test") {
  info <- project_info(suffix)
  dir.create(info$path_build, FALSE, TRUE)
  writeLines(dockerfile_test(info),
             file.path(info$path_build, "Dockerfile"))
  write_scripts(info)
}

write_scripts <- function(info) {
  boot2docker_str <-
    if (is_mac()) "$(boot2docker shellinit 2> /dev/null)" else ""

  ## TODO: This is temporary for now at least.
  ## TODO: as in build(), assume that we're testing unless explicitly
  ## run.
  if (info$suffix == "-run") {
    volume_map <- ""
  } else {
    volume_map <- sprintf("-v %s:/src", info$path_project)
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

## Things that should be configurable:
##   - name
##   - tagname
##   - username (currently dockertest)
## This might also be the place to load the config file?
project_info <- function(suffix, path_project=NULL) {
  path_project <- find_project_root(path_project)
  path_package <- find_package_root(NULL, path_project, error=FALSE)
  is_package <- !is.null(path_package)
  if (is_package) {
    name <- devtools::as.package(path_package)$package
  } else {
    name <- basename(path_project)
  }
  tagname <- paste0("dockertest/", tolower(name), suffix)
  path_build <- paste0(name, suffix)

  list(name=name,
       suffix=suffix,
       tagname=tagname,
       path_project=path_project,
       path_package=path_package,
       path_build=path_build,
       is_package=is_package)
}

##' Build a docker container
##' @title Build a docker container
##' @param type Type of container to build.  Valid options are "test",
##'   "run" and "production" (last one only for packages).
##' @param prepare Rerun \code{\link{prepare}} before building the
##'   image?
##' @param use_cache Set to FALSE to skip docker's cache
##' @param machine name of docker machine to use
##' @param filename Full path to dockertest filename if not in the
##'   current directory or in the docker/ or dockertest/ directory.
##' @export
build <- function(type="test", prepare=TRUE, use_cache=TRUE,
                  machine=NULL, filename=NULL) {
  if (prepare) {
    info <- prepare(type, filename)
  } else {
    info <- project_info(type, filename)
  }
  dockerfile <- file.path(info$path_build, "Dockerfile")
  path <- if (info$inplace) info$path_project else "."
  docker_build(path, dockerfile, info$tagname, use_cache, machine)
  list(name=info$tagname,
       id=docker_image_id(info$tagname, machine=machine))
}

##' Prepare for a build by writing a dockerfile and copying scripts
##' into the build directory.
##' @title Prepare for docker build
##' @param type Type of container to build.  Valid options are "test",
##'   "run" and "production" (last one only for packages).
##' @param filename Full path to dockertest filename if not in the
##'   current directory or in the docker/ or dockertest/ directory.
##' @export
prepare <- function(type="test", filename=NULL) {
  info <- project_info(type, filename)
  dir.create(info$path_build, FALSE, TRUE)
  clone_local(info)
  if (!info$inplace) {
    clone_self(info)
  }
  format_docker(dockerfile_dockertest(info),
                file.path(info$path_build, "Dockerfile"))
  info
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
project_info <- function(type, path_project=NULL, filename=NULL) {
  filename <- dockertest_path(filename, error=FALSE)

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
              local_filesystem=type == "test")
  ret$install_package <- is_package && type != "test"
  ret$config <- load_config(filename)

  ## If we install the package, this needs overriding, regardless of
  ## what it was set as in the config.
  if (ret$install_package) {
    ret$config$deps_only <- FALSE
  }

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

  if (is.null(ret$config$workdir)) {
    if (ret$install_package && ret$is_package) {
      ret$workdir <- "/root"
    } else {
      ret$workdir <- file.path("/root", ret$name)
    }
  } else {
    ret$workdir <- ret$config$workdir
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
load_config <- function(filename=NULL) {
  ## We'll look in the local directory and in the package root.
  filename <- dockertest_path(filename, error=FALSE)
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
    modules=NULL,
    ## Packages to ignore system dependencies from:
    system_ignore_packages=NULL,
    ## Tag names for generated images:
    names=NULL,
    ## Don't set a workdir by default; changes a number of things in
    ## ways that will be tricky to deal with.
    workdir=NULL,
    ## I'm not really sure anymore
    inplace=FALSE,
    ## Keep '.git' directories in clones (costs some space)
    keep_git=FALSE,
    ## Don't include the sources of this package
    deps_only=TRUE)
  if (!is.null(filename) && file.exists(filename)) {
    ret <- yaml_read(filename)
    extra <- setdiff(names(ret), names(defaults))
    if (length(extra) > 0L) {
      msg <- sprintf("Unknown fields in %s: %s", filename,
                     paste(extra, collapse = ", "))
      warning(msg)
    }
    ret <- modifyList(defaults, ret)
  } else {
    ret <- defaults
  }
  ## *Always* include the "dockertest_bootstrap" module which includes
  ## dependencies needed to get started.  Ideally we'd prevent this
  ## from being overwritable but that doesn't seem worthwhile.
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

  modules_files <- sprintf("dockertest_%s.yml", modules)
  ok <- file.exists(modules_files)
  if (any(!ok)) {
    tmp <- file.path("modules", paste0(modules[!ok], ".yml"))
    modules_files[!ok] <- system.file(tmp,
                                      package="dockertest",
                                      mustWork=TRUE)
  }

  ## What we want to do is load github packages and commands *first*
  ## but load them in the order specified.
  r_github_packages <- commands <- character(0)
  for (f in modules_files) {
    x <- read_module(f)
    info$config$apt_packages <-
      union(info$config$apt_packages, x$apt_packages)
    info$config$r_packages <-
      union(info$config$r_packages, x$r_packages)
    r_github_packages <- union(r_github_packages, x$r_github_packages)
    commands <- union(commands, x$commands)
    info$config$system_ignore_packages <-
      union(x$system_ignore_packages,
            info$config$system_ignore_packages)
  }

  info$config$r_github_packages <-
    union(r_github_packages, info$config$r_github_packages)
  info$config$commands <- union(commands, info$config$commands)

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


dockertest_path <- function(filename=NULL, error=TRUE) {
  if (is.null(filename)) {
    filename <- "dockertest.yml"
    ## In descending order of preference:
    paths <- file.path(c(".", "dockertest", "docker"), filename)
    for (p in paths) {
      if (file.exists(p)) {
        return(p)
      }
    }
  } else {
    if (file.exists(filename)) {
      return(filename)
    }
  }

  if (error) {
    stop("Did not find file: ", filename)
  } else {
    NULL
  }
}

dockertest_names <- function(filename=NULL) {
  filename <- dockertest_path(filename, FALSE)
  res <- setNames(character(0), character(0))
  if (!is.null(filename)) {
    dat <- yaml_read(filename)
    if (!is.null(dat$names)) {
      res <- unlist(dat$names)
    }
  }
  res
}

##' Launches a docker container that was created by dockertest.
##'
##' Note that this is the worst sort of function; the output totally
##' changes depending on what mode it is in.  It will either run a
##' function or it will print what it \emph{would} run so that that
##' command can be used elsewhere.
##'
##' @title Launch a docker container
##' @param type Type of container to run (as for \code{link{build}}.
##'   "run" and "production" (last one only for packages).
##' @param args Additional arguments to pass through to docker.  As
##'   it's not totally transparent what arguments dockertest will
##'   inject, this should really be (optionally) things like the name
##'   of the program to run within a container.
##' @param interactive Should the container be interactive and
##'   allocate a pseudo-tty?  The default is \code{TRUE} because it is
##'   possible to lock your console hard if you set this to
##'   \code{FALSE} when input is needed.
##' @param dry_run Rather than actually run the command, return the
##'   command string suitable for running in a separate script.
##' @param mount_volume Should a volume be mounted?  This generates
##'   the appropriate \emph{absolute} path name for a mapping.  By
##'   default this is \code{TRUE} if the image requires it.  If the
##'   image is "inplace", the the mount will map the project root to
##'   the docker working directory.  If not then it will mount the
##'   `self` directory (a clean clone) into the place where that is
##'   re-cloned into the container (therefore isolated from the
##'   machine).  This behaviour may change.
##' @param machine Name of docker machine to use
##' @param link Vector of links (e.g. \code{mycontainer:redis})
##' @param name As an alternative to \code{type}, a full name can be
##'   given here, which dockertest will attempt to map onto a type.
##' @param filename Optional filename used when \code{name} is given;
##'   only needed if \code{dockertest.yml} is hard to find.
##' @export
launch <- function(type="test", args=NULL, interactive=TRUE, dry_run=FALSE,
                   mount_volume=NULL, machine=NULL, link=NULL,
                   name=NULL, filename=NULL) {
  if (!is.null(name)) {
    if (!missing(type)) {
      stop("If name is given, type must be empty")
    }
    nms <- dockertest_names(filename)
    i <- match(name, nms)
    if (is.na(i)) {
      stop(sprintf("Name %s not found", name))
    }
    type <- names(nms)[[i]]
  }
  if (!dry_run) {
    docker_machine_init(machine)
  }
  docker <- callr::Sys_which("docker")
  info <- project_info(type)

  if (is.null(mount_volume)) {
    mount_volume <- info$local_filesystem
  }
  if (mount_volume) {
    if (info$inplace) {
      src <- info$path_project
      dest <- info$workdir
    } else {
      ## TODO: I don't know if this is always correct:
      path_dockertest <- dirname(dockertest_path(filename, TRUE))
      src <- file.path(getwd(), path_dockertest, "self")
      dest <- "/src"
    }
    volume_map <- c("-v", sprintf("%s:%s", src, dest))
  } else {
    volume_map <- character(0)
  }
  if (interactive) {
    interactive <- "-it"
  } else {
    interactive <- character(0)
  }
  if (!is.null(link)) {
    link <- as.character(rbind("--link", link))
  }
  args <- c("run", link, volume_map, interactive, info$tagname, args)

  ## NOTE: Not using callr here because I need i/o
  if (dry_run) {
    paste(c(docker, args), collapse = " ")
  } else {
    system2(docker, args)
  }
}

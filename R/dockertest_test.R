dockerfile_dockertest <- function(info) {
  config <- info$config

  info <- add_project_deps(info)
  deps <- dockertest_dependencies(info)
  path <- file.path("/root", info$name)
  copy_sources <- docker_copy_sources(path,
                                      info$local_filesystem,
                                      info$path_self,
                                      info$config$deps_only)

  if (info$install_package) {
    if (!info$is_package) {
      stop("Makes no sense")
    }
    post_install <- c(list(),
                      copy_sources,
                      docker_RUN(c(paste("R CMD INSTALL", path),
                                   paste("rm -rf", path))),
                      info$post_install)
    workdir <- "/root"
  } else {
    post_install <- c(list(),
                      copy_sources,
                      info$post_install)
    workdir <- path
  }

  if (is.null(deps$local)) {
    local_paths <- NULL
  } else {
    local_paths <- file.path(info$path_local, names(deps$local))
  }

  extra <- process_extra_commands(info)

  c(list(),
    docker_FROM(info$config$image),
    docker_apt_get_install(deps$system),
    extra$system,
    docker_install2(deps$R, deps$repos),
    extra$R,
    docker_install_github(deps$github),
    extra$github,
    ## Local paths are relative to the build directory.
    docker_install_local(local_paths),
    extra$local,
    post_install,
    docker_WORKDIR(workdir),
    extra$workdir,
    docker_CMD("bash"))
}

## There are may be multiple commands, and those commands might be
## executed after different other commands:
process_extra_commands <- function(info) {
  cmds <- info$config$commands

  ret <- list(system=NULL, R=NULL, github=NULL, local=NULL, workdir=NULL)
  if (length(cmds) > 0L) {
    when <- lapply(cmds, "[[", "after")
    when[vapply(when, is.null, logical(1))] <- "github"
    when <- vapply(when, identity, character(1))

    err <- setdiff(unique(when), names(ret))
    if (length(err) > 0L) {
      stop("Unknown timing: ", paste(err, collapse=", "))
    }

    cmds <- split(vapply(cmds, function(x) x$command, character(1)), when)

    for (i in names(cmds)) {
      ## Add trailing backslashes where they look necessary:
      add_backslash <- function(x) {
        gsub("([^\\])([^\\ ]*)\n", "\\1\\2 \\\\\n",
             sub("\n$", "", x))
      }
      split_command <- function(x) {
        re <- "([A-Z]+)\\s+(.+)$"
        if (!all(grepl(re, x))) {
          stop("Does not look like a docker command")
        }
        cmd <- sub(re, "\\1", x)
        value <- sub(re, "\\2", x)
        names(value) <- cmd
        value
      }
      ret[[i]] <- split_command(add_backslash(cmds[[i]]))
    }
  }
  ret
}

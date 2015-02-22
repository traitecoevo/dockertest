format_docker <- function(commands, filename=NULL) {
  ## NOTE: this is not really our place to check...
  valid <- c("COPY", "RUN", "WORKDIR", "FROM", "CMD")
  if (!all(names(commands) %in% valid)) {
    stop("Invalid command")
  }
  msg <- vapply(commands, is.null, logical(1))
  commands <- unlist(commands[!msg])
  str <- paste(names(commands), commands, sep=" ", collapse="\n\n")
  if (!is.null(filename)) {
    writeLines(str, filename)
  }
  str
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

docker_build <- function(path, dockerfile, tagname, use_cache=TRUE) {
  if (Sys.info()[["sysname"]] == "Darwin") {
    ## TODO: Also windows, apparently.
    boot2docker_shellinit()
  }
  ## TODO: Need to get *relative* path to project here; i.e., how many
  ## steps down are we?  That becomes the build directory.  It's going
  ## to be ".." in many cases.
  args <- c("build",
            "-f", dockerfile,
            "-t", tagname,
            if (!use_cache) "--no-cache",
            path)
  ok <- system2("docker", args)
  if (ok != 0L) {
    stop("Error running docker")
  }
  message("Created image ", tagname)
}

docker_join <- function(x, list=TRUE, sort=list) {
  if (length(x) <= 1) {
    unname(x)
  } else {
    collapse <- " \\\n  "
    if (list) {
      collapse <- paste0(collapse, "  ")
      if (sort) {
        x <- sort(x)
      }
      x <- c("", x) # also pad
    } else {
      collapse <- paste0(collapse, "&& ")
    }
    paste(x, collapse=collapse)
  }
}

docker_apt_get_install <- function(packages) {
  if (length(packages) == 0L) {
    return(NULL)
  }
  cmd <- c("apt-get update",
           paste0("apt-get install -y --no-install-recommends ",
                  docker_join(packages, list=TRUE)),
           "apt-get clean")
  # "rm -rf /var/lib/apt/lists/")
  docker_RUN(cmd)
}

docker_install2 <- function(packages, repos=NULL) {
  if (length(packages) == 0) {
    return(NULL)
  }
  ## RUN install2.r --error {{{dependencies.repos}}} {{{dependencies.R}}}
  cmd <- "install2.r --error"
  if (!is.null(repos)) {
    repos <- docker_join(sprintf("--repos=%s", repos), list=TRUE)
    cmd <- paste(cmd, repos)
  }
  cmd <- paste(cmd, docker_join(packages, list=TRUE))
  if (!is.null(repos)) {
    url <- "https://raw.githubusercontent.com/cboettig/littler/master/examples/install2.r"
    upgrade_install2_r <-
      c("rm /usr/local/bin/install2.r",
        paste0("wget --no-check-certificate -O /usr/local/bin/install2.r\\\n",
               "  ", url),
        "chmod +x /usr/local/bin/install2.r")
    cmd <- c(upgrade_install2_r, cmd)
  }

  docker_RUN(cmd)
}

docker_install_github <- function(github_repos) {
  if (is.null(github_repos)) {
    return(NULL)
  }
  docker_RUN(paste0("installGithub.r ", docker_join(github_repos, list=TRUE)))
}

docker_install_local <- function(local_paths) {
  if (is.null(local_paths)) {
    return(NULL)
  }
  local_dir <- dirname(local_paths[[1]])
  local_docker <- file.path("/local", basename(local_paths))
  packages_local <- docker_join(local_docker, list=TRUE, sort=FALSE)
  c(list(),
    docker_COPY(local_dir, "/local"),
    docker_RUN(c(paste("install2.r --repos=NULL --error", packages_local),
                 "rm -rf /local")))
}

docker_copy_sources <- function(path, local_filesystem, path_self=".") {
  if (local_filesystem) {
    str   <- paste("clone.sh", path)
    str_r <- sprintf("system('%s')", str)
    cmd <- c(paste0("mkdir ", path),
             sprintf('echo "%s" >> /root/.bashrc', str),
             sprintf('echo "%s" > /root/.Rprofile', str_r),
             sprintf('echo "%s" > /root/.littler.r', str_r))
    copy_sources <- docker_RUN(cmd)
  } else {
    copy_sources <- docker_COPY(path_self, path)
  }
  copy_sources
}

## TODO: Support multiple arguments:
docker_r <- function(command) {
  docker_RUN(sprintf("r -e '%s'", command))
}

docker_COPY <- function(from, to) {
  list(COPY=paste(from, to))
}
docker_RUN <- function(commands) {
  if (is.null(commands)) {
    NULL
  } else {
    list(RUN=docker_join(commands, list=FALSE))
  }
}
docker_FROM <- function(image) {
  list(FROM=image)
}
docker_WORKDIR <- function(path) {
  if (is.null(path)) {
    NULL
  }
  list(WORKDIR=path)
}
docker_CMD <- function(cmd) {
  if (is.null(cmd)) {
    NULL
  }
  list(CMD=sprintf('["%s"]', cmd))
}
docker_MAINTAINER <- function(name, email) {
  list(MAINTAINER=sprintf('"%s" %s', name, email))
}

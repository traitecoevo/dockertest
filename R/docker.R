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

##' Initialise environment variables to connect to a docker machine
##'
##' @title Connect to a docker machine
##' @param machine Name of a machine
##' @export
##' @importFrom callr Sys_which
docker_machine_init <- function(machine=NULL) {
  machine <- docker_machine_init_which(machine)
  if (!is.null(machine)) {
    message(sprintf("Setting up docker-machine '%s'", machine))
    docker_machine <- callr::Sys_which("docker-machine")
    status <- callr::call_system(docker_machine, c("status", machine))
    if (!identical(status, "Running")) {
      stop(sprintf("docker-machine '%s' not running? Status: '%s'",
                   machine, status))
    }

    res <- callr::call_system(docker_machine, paste("env ", machine),
                              stderr=FALSE)

    ## Filter to lines containing `export`
    re <- "^\\s*export\\s+"
    res <- res[grep(re, res)]
    vars <- strsplit(sub("^\\s*export\\s+", "", res), "=", fixed=TRUE)
    if (!all(vapply(vars, length, integer(1)) == 2)) {
      stop("Unexpected output from docker-machine")
    }

    strip_quotes <- function(x) {
      gsub('(^"|"$)', "", x)
    }
    var_name <- vcapply(vars, function(x) x[[1]])
    var_val  <- as.list(strip_quotes(vcapply(vars, function(x) x[[2]])))

    names(var_val) <- var_name
    do.call("Sys.setenv", var_val)

    if (Sys.getenv("DOCKER_HOST") == "") {
      stop("Failed to set docker_machine variables")
    }
    tryCatch(callr::call_system(callr::Sys_which("docker"), "ps"),
             error=function(e)
               stop("While trying to test docker:\n", e$message))
  }
}

docker_machine_init_which <- function(machine) {
  if (Sys.getenv("DOCKER_HOST") == "") {
    docker_machine <- callr::Sys_which("docker-machine")
    args <- c("ls", "-q", "--filter", "state=Running")
    machines <- callr::call_system(docker_machine, args)
    if (is.null(machine)) {
      if (length(machines) < 1L) {
        stop("No running docker machines detected")
      } else if (length(machines) > 1L) {
        message("More than one machine present, taking the first")
      }
      machines[[1]]
    } else {
      if (!(machine %in% machines)) {
        stop(sprintf("machine '%s' requested but not in running set: %s",
                     machine, paste(machines, collapse=", ")))
      }
      machine
    }
  } else if (!is.null(machine) && Sys.getenv("DOCKER_MACHINE_NAME") != machine) {
    machine
  } else {
    NULL
  }
}

docker_build <- function(path, dockerfile, tagname, use_cache=TRUE,
                         machine=NULL) {
  if (Sys.info()[["sysname"]] == "Darwin") {
    ## TODO: Also windows, apparently.
    docker_machine_init(machine)
  }

  ## TODO: Need to get *relative* path to project here; i.e., how many
  ## steps down are we?  That becomes the build directory.  It's going
  ## to be ".." in many cases.
  args <- c("build",
            "-f", dockerfile,
            "-t", tagname,
            if (!use_cache) "--no-cache",
            path)
  docker <- callr::Sys_which("docker")
  ## NOTE: Using system2 and *not* callr::call_system here because I
  ## want the program output to be printed here.
  ok <- system2("docker", args)
  if (ok != 0L) {
    stop("Error running docker")
  }
  message("Created image ", tagname)
}

docker_image_id <- function(name, machine=NULL) {
  if (Sys.info()[["sysname"]] == "Darwin") {
    ## TODO: Also windows, apparently.
    docker_machine_init(machine)
  }
  docker <- callr::Sys_which("docker")
  callr::call_system(docker, c("images", "--no-trunc", "-q", name))
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

docker_apt_get_install <- function(packages, unstable) {
  if (length(packages) == 0L) {
    return(NULL)
  }
  opts <- c("-y", "--no-install-recommends",
            if (unstable) "-t unstable")

  cmd_install <- sprintf("apt-get install %s", paste(opts, collapse=" "))
  cmd <- c("apt-get update",
           paste0(cmd_install, docker_join(packages, list=TRUE)),
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
  docker_RUN(paste0("installGithub.r ",
                    docker_join(github_repos, list=TRUE, sort=FALSE)))
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
    docker_RUN(c(paste("R CMD INSTALL", packages_local),
                 "rm -rf /local")))
}

docker_copy_sources <- function(path, local_filesystem, path_self=".",
                                deps_only=FALSE) {
  if (local_filesystem) {
    str   <- paste("clone.sh", path)
    str_r <- sprintf("system('%s')", str)
    cmd <- c(paste0("mkdir ", path),
             sprintf('echo "%s" >> /root/.bashrc', str),
             sprintf('echo "%s" > /root/.Rprofile', str_r),
             sprintf('echo "%s" > /root/.littler.r', str_r))
    copy_sources <- docker_RUN(cmd)
  } else if (deps_only) {
    copy_sources <- NULL
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

format_docker <- function(commands) {
  valid <- c("COPY", "RUN", "WORKDIR", "FROM")
  if (!all(names(commands) %in% valid)) {
    stop("Invalid command")
  }
  msg <- vapply(commands, is.null, logical(1))
  commands <- unlist(commands[!msg])
  paste(names(commands), commands, sep=" ", collapse="\n")
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

docker_build <- function(path, tagname, use_cache=TRUE) {
  if (Sys.info()[["sysname"]] == "Darwin") {
    ## TODO: Also windows, apparently.
    boot2docker_shellinit()
  }
  args <- c("build", "-t", tagname,
            if (!use_cache) "--no-cache",
            path)
  ok <- system2("docker", args)
  if (ok != 0L) {
    stop("Error running docker")
  }
  message("Created image ", tagname)
}

docker_join_commands <- function(x, list=TRUE) {
  if (length(x) <= 1) {
    unname(x)
  } else {
    collapse <- "  \\\n    "
    if (list) {
      x <- c("", sort(x)) # also pad
    } else {
      collapse <- paste0(collapse, "&& ")
    }
    paste(x, collapse=collapse)
  }
}

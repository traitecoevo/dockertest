main <- function(args=commandArgs(TRUE)) {
  opts <- parse_main_args(args)
  if (is.null(opts$type)) {
    opts$type <- "test"
  }
  if (isTRUE(opts$build)) {
    build(opts$type,
          prepare=!opts$"no-prepare",
          use_cache=!opts$"no-cache",
          machine=opts$machine)
  } else if (isTRUE(opts$prepare)) {
    invisible(prepare(opts$type))
  } else if (isTRUE(opts$launch)) {
    if (opts$"dry-run") {
      res <- suppressMessages(launch(opts$type,
                                     opts$args,
                                     machine=opts$machine,
                                     dry_run=TRUE,
                                     link=opts$link))
      ## NOTE: Uses cat because that goes to stdout:
      cat(res, "\n", sep="")
    } else {
      launch(opts$type, opts$args, machine=opts$machine, link=opts$link)
    }
  } else {
    stop("Unimplemented command")
  }
}

parse_main_args <- function(args) {
  'Usage:
  dockertest prepare [<type>]
  dockertest build  [--machine=NAME] [--no-prepare] [--no-cache] [<type>]
  dockertest launch [--machine=NAME] [--link LINK...] [--dry-run] [<type>] [--] [<args>...]

  Options:
  --machine=NAME  docker machine name to use (non Linux) [default: default]
  --no-prepare    don\'t reclone/recreate Dockerfile
  --no-cache      skip docker\'s cache on building
  --link LINK     passed to docker\'s --link option
  --dry-run       don\'t launch container but print command to do so' -> str
  res <- docopt_parse(str, args)
  if (!isTRUE(res[["--"]]) && identical(res$type, "--")) {
    res["type"] <- list(NULL)
    res["--"] <- TRUE
  }
  res
}

##' @importFrom docopt docopt
docopt_parse <- function(...) {
  oo <- options(warnPartialMatchArgs=FALSE)
  if (isTRUE(oo$warnPartialMatchArgs)) {
    on.exit(options(oo))
  }
  docopt::docopt(...)
}

##' Install dockertest worker script
##' @title Install dockertest worker script
##' @param destination_directory Directory to install to
##' @param overwrite Overwrite existing file?
##' @export
install_script <- function(destination_directory, overwrite=FALSE) {
  script <- c(
    "#!/usr/bin/Rscript",
    "library(methods)",
    "dockertest:::main()")

  file <- file.path(destination_directory, "dockertest")
  if (file.exists(file) && !overwrite) {
    stop(sprintf("File %s already exists", file))
  }
  dir.create(destination_directory, FALSE, TRUE)
  writeLines(script, file)
  Sys.chmod(file, "0755")
}

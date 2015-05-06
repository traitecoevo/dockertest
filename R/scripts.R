main <- function(args=commandArgs(TRUE)) {
  'Usage:
  dockertest prepare [<type>]
  dockertest build [<type>]' -> str
  opts <- docopt_parse(str, args)
  if (is.null(opts$type)) {
    opts$type <- "test"
  }
  if (isTRUE(opts$build)) {
    build(opts$type)
  } else if (isTRUE(opts$prepare)) {
    prepare(project_info(opts$type))
  } else {
    stop("Unimplemented command")
  }
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

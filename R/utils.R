## https://github.com/viking/r-yaml/issues/5#issuecomment-16464325
##' @importFrom yaml yaml.load
yaml_load <- function(string) {
  ## More restrictive true/false handling.  Only accept if it maps to
  ## full true/false:
  handlers <- list("bool#yes" = function(x) {
    if (identical(toupper(x), "TRUE")) TRUE else x},
                   "bool#no" = function(x) {
    if (identical(toupper(x), "FALSE")) FALSE else x})
  yaml::yaml.load(string, handlers=handlers)
}

yaml_read <- function(filename) {
  catch_yaml <- function(e) {
    stop(sprintf("while reading '%s'\n%s", filename, e$message),
         call.=FALSE)
  }
  tryCatch(yaml_load(read_file(filename)),
           error=catch_yaml)
}

read_file <- function(filename, ...) {
  ## assert_file_exists(filename)
  paste(readLines(filename), collapse="\n")
}

is_mac <- function() {
  Sys.info()[["sysname"]] == "Darwin"
}

##' @importFrom rappdirs user_cache_dir
user_data_dir <- function() {
  path <- rappdirs::user_cache_dir("dockertest")
  dir.create(path, FALSE, TRUE)
  path
}

find_project_root <- function(path_project=NULL) {
  if (is.null(path_project)) {
    find_file_descend(".git")
  } else {
    path_project
  }
}

find_package_root <- function(path_package=NULL, path_project=NULL,
                              error=TRUE) {
  path_project <- find_project_root(path_project)
  if (is.null(path_package)) {
    find_file_descend("DESCRIPTION", path_project, error)
  } else {
    path_package
  }
}

find_file_descend <- function(target, limit="/", error=TRUE) {
  root <- normalizePath(limit, mustWork=TRUE)
  f <- function(path) {
    if (file.exists(file.path(path, target))) {
      return(path)
    }
    if (normalizePath(path, mustWork=TRUE) == root) {
      if (error) {
        stop(sprintf("Hit %s without finding %s", root, target))
      } else {
        return(NULL)
      }
    }
    Recall(file.path("..", path))
  }
  ret <- f(".")
  if (!(is.null(ret) && !error)) {
    ret <- normalizePath(ret, mustWork=TRUE)
  }
  ret
}


## TODO: The github one should probably fetch the package and get the
## name from there - otherwise things like geiger break.  There are a
## few others too.  We can just nail that early on.
##
## One option is to download the DESCRIPTION file here.  Do that at
## the same time that we do automatic expiry.
github_package_name <- function(repo) {
  sub(".*/", "", repo)
}

local_package_name <- function(path) {
  f <- function(x) {
    description_field(devtools::as.package(x),  "Package")
  }
  unname(vapply(path, f, character(1)))
}

add_to_gitignore <- function(path) {
  if (length(path) != 1) {
    stop("Just one length for now")
  }
  git <- Sys.which("git")
  if (system2(git, c("check-ignore", path), stderr=FALSE) != 0L) {
    write(path, ".gitignore", append=TRUE)
  }
}

git_clone <- function(repo, dest, quiet=FALSE, shallow=FALSE) {
  if (quiet) {
    stderr <- stdout <- FALSE
  } else {
    stderr <- stdout <- ""
  }

  if (shallow) {
    args <- c("clone", "--depth=1", "--single-branch")
  } else {
    args <- c("clone", repo, dest)
  }

  ok <- system2(Sys.which("git"), args,
                stderr=stderr, stdout=stdout)
  if (ok != 0L) {
    stop("Error cloning ", repo, " to ", dest)
  }
}

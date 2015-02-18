
## Attempt to guess packages that won't be installable from CRAN
## because they are base, but might still be listed in a packages's
## various dependencies:
base_packages <- function() {
  rownames(installed.packages(priority=c("base", "recommended")))
}

find_package_root <- function(path_package=NULL) {
  if (is.null(path_package)) {
    root <- normalizePath("/", mustWork=TRUE)
    f <- function(path) {
      if (file.exists(file.path(path, "DESCRIPTION"))) {
        return(path)
      }
      if (normalizePath(path, mustWork=TRUE) == root) {
        stop("Hit the root without finding a package")
      }
      Recall(file.path("..", path))
    }
    normalizePath(f("."), mustWork=TRUE)
  } else {
    ## Assume we've done this before:
    path_package
  }
}

##' @importFrom devtools as.package
as_package <- function(path_package=NULL) {
  as.package(find_package_root(path_package))
}

package_name <- function(path_package=NULL) {
  as_package(path_package)$package
}

package_descriptions <- function(package_names) {
  dat <- lapply(package_names,
                function(x) suppressWarnings(packageDescription(x)))
  names(dat) <- package_names
  keep <- sapply(dat, inherits, "packageDescription")
  dat[keep]
}

description_field <- function(x, v) {
  x[[match(tolower(v), tolower(names(x)))]]
}

package_dependencies_recursive <- function(packages, all=FALSE) {
  v <- c("Depends", "Imports", "LinkingTo")
  if (all) {
    v <- c(v, "Suggests", "VignetteBuilder")
  }

  join <- function(x) {
    ret <- do.call("rbind", x)
    rownames(ret) <- NULL
    n <- sapply(x, function(x) if (is.null(x)) 0L else nrow(x))
    ret$package <- rep(names(x), n)
    ret
  }
  join_deps <- function(x) {
    gsub("\n", " ", paste(na.omit(x), collapse=", "))
  }

  dat <- try(fetch_PACKAGES())
  if (inherits(dat, "try-error")) { # offline, etc
    dat <- matrix(nrow=0, ncol=length(v), dimnames=list(character(0), v))
  }

  seen <- base_packages()
  deps <- NULL
  while (length(packages) > 0L) {
    i <- packages %in% rownames(dat)
    str <- character(0)
    ## From PACKAGES:
    if (any(i)) {
      str <- c(str, apply(dat[packages[i], v, drop=FALSE], 1, join_deps))
    }
    ## Offline, or locally installed
    if (any(!i)) {
      str <- c(str, sapply(package_descriptions(packages[!i]),
                           function(x) join_deps(x[v])))
    }
    x <- join(lapply(str, devtools:::parse_deps))
    seen <- c(seen, packages)
    deps <- c(deps, list(x))
    packages <- setdiff(x$name, seen)
  }

  deps_all <- do.call("rbind", deps)
  deps_all <- deps_all[order(deps_all$name), ]
  rownames(deps_all) <- NULL
  deps_all
}

##' @importFrom downloader download
fetch_PACKAGES <- function(force=FALSE) {
  dest <- file.path(system.file(package="dockertest"), "PACKAGES.rds")
  if (force || !file.exists(dest)) {
    tmp <- tempfile()
    url <- "https://github.com/metacran/PACKAGES/raw/master/PACKAGES"
    downloader::download(url, tmp)
    dat <- read.dcf(tmp)
    rownames(dat) <- dat[, "Package"]
    try(saveRDS(dat, dest))
  } else {
    dat <- readRDS(dest)
  }
  invisible(dat)
}

##' @importFrom httr GET content
##' @importFrom jsonlite fromJSON
fetch_PACKAGES_crandb <- function(force=FALSE) {
  dest <- file.path(system.file(package="dockertest"),
                    "PACKAGES_crandb.rds")
  if (force || !file.exists(dest)) {
    ## From metacran/crandb's DB:
    api <- "/-/latest"
    url <- paste0("http://crandb.r-pkg.org", "/", api)
    dat_json <- httr::content(httr::GET(url), as="text", encoding="UTF-8")
    dat <- jsonlite::fromJSON(dat_json)

    ## Convert the nice crandb metadata into the sort that we can
    ## process from other packages.
    clean <- function(x) {
      join_field <- function(x) {
        if (is.null(x)) NA_character_ else paste(names(x), collapse=", ")
      }
      v <- c("Depends", "Imports", "LinkingTo",
             "Suggests", "VignetteBuilder")
      x <- x[v]
      names(x) <- v
      x <- lapply(x, join_field)
      unlist(x)
    }

    ret <- cbind(Package=names(dat),
                 do.call("rbind", lapply(dat, clean)))
    try(saveRDS(ret, dest))
  } else {
    ret <- readRDS(dest)
  }
  invisible(ret)
}

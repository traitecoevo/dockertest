## Attempt to guess packages that won't be installable from CRAN
## because they are base, but might still be listed in a packages's
## various dependencies:
base_packages <- function() {
  rownames(installed.packages(priority=c("base", "recommended")))
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

package_dependencies_recursive <- function(packages, package_info,
                                           all=FALSE) {
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
    gsub("\n", " ", paste(na.omit(x[v]), collapse=", "))
  }

  ## TODO: Need to pass in either the configuration or a list of
  ## github repos here.

  seen <- base_packages()
  deps <- NULL
  while (length(packages) > 0L) {
    i <- packages %in% rownames(package_info)
    str <- character(0)
    ## From PACKAGES:
    if (any(i)) {
      str <- c(str, apply(package_info[packages[i], v, drop=FALSE],
                          1, join_deps))
    }
    ## Offline, or locally installed
    if (any(!i)) {
      str <- c(str, sapply(package_descriptions(packages[!i]),
                           join_deps))
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
fetch_PACKAGES_CRAN <- function(force=FALSE) {
  dest <- file.path(user_data_dir(), "PACKAGES.rds")
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
  dest <- file.path(user_data_dir(), "PACKAGES_crandb.rds")
  if (force || !file.exists(dest)) {
    ## From metacran/crandb's DB:
    api <- "/-/latest"
    url <- paste0("http://crandb.r-pkg.org", "/", api)
    message("Downloading crandb/latest - may take a minute")
    dat_json <- httr::content(httr::GET(url), as="text", encoding="UTF-8")
    dat <- jsonlite::fromJSON(dat_json)

    ## Convert the nice crandb metadata into the sort that we can
    ## process from other packages.  We want to get the
    ## SystemDependencies out of here too.
    clean <- function(x) {
      join_field <- function(x) {
        if (is.null(x)) NA_character_ else paste(names(x), collapse=", ")
      }
      to_join <- c("Depends", "Imports", "LinkingTo",
                   "Suggests", "VignetteBuilder")
      x <- x[description_fields()]
      names(x) <- description_fields()
      x[to_join] <- lapply(x[to_join], join_field)
      if (is.null(x$SystemRequirements)) {
        x$SystemRequirements <- NA_character_
      }
      unlist(x)
    }

    ret <- do.call("rbind", lapply(dat, clean))
    try(saveRDS(ret, dest))
  } else {
    ret <- readRDS(dest)
  }
  invisible(ret)
}

##' @importFrom downloader download
fetch_PACKAGES_github <- function(repos) {
  path <- file.path(user_data_dir(), "github")
  dir.create(path, FALSE, TRUE)

  fmt <- "https://raw.githubusercontent.com/%s/master/DESCRIPTION"
  dat <- list()
  for (r in repos) {
    path_r <- file.path(path, r)
    dest_r <- file.path(path_r, "DESCRIPTION")
    dir.create(path_r, FALSE, TRUE)
    message("Fetching ", r)
    ok <- try(downloader::download(sprintf(fmt, r), dest_r,
                                   quiet=TRUE))
    dat_r <- read.dcf(dest_r)
    dat_r <- drop(dat_r)[description_fields()]
    names(dat_r) <- description_fields()
    dat[[r]] <- dat_r
  }

  if (length(dat) == 0L) {
    ret <- NULL
  } else {
    ret <- do.call("rbind", dat)
    ## NOTE: Here we use reported package names, unlike
    ## github_package_names().
    rownames(ret) <- ret[, "Package"]
  }
  ret
}

fetch_PACKAGES_local <- function(paths, path_build) {
  if (length(paths) == 0) {
    return(NULL)
  }
  dest <- file.path(path_build, ".local")
  if (file.exists(dest)) {
    unlink(dest, recursive=TRUE)
  }
  dir.create(dest, FALSE, TRUE)
  add_to_gitignore(dest)

  package_names <- local_package_name(paths)
  paths_dest <- file.path(dest, package_names)

  dat <- vector("list", length(paths))
  for (i in seq_along(paths)) {
    git_clone(paths[[i]], paths_dest[[i]])
    dat_i <- read.dcf(file.path(paths_dest[[i]], "DESCRIPTION"))
    dat_i <- drop(dat_i)[description_fields()]
    names(dat_i) <- description_fields()
    dat[[i]] <- dat_i
  }
  names(dat) <- package_names
  names(package_names) <- paths
  ret <- do.call("rbind", dat)
  ret
}

fetch_PACKAGES <- function(github_repos, local_paths, build_dir) {
  dat_crandb <- fetch_PACKAGES_crandb()
  dat_github <- fetch_PACKAGES_github(github_repos)
  dat_local  <- fetch_PACKAGES_local(local_paths, build_dir)

  ## Ordering things this way means we prefer local over github over
  ## cran.
  dat <- rbind(dat_local, dat_github, dat_crandb)
  keep <- !duplicated(rownames(dat))
  dat <- dat[keep, , drop=FALSE]

  dat
}

description_fields <- function() {
  c("Package",
    "Depends", "Imports", "LinkingTo", "Suggests", "VignetteBuilder",
    "SystemRequirements")
}

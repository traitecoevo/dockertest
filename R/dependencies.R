dockertest_dependencies <- function(info) {
  config <- info$config

  packages <- deps(config$packages$R,
                   config$packages$github,
                   config$packages$local)

  package_info <- deps_fetch_package_info(packages$github, packages$local)
  package_names <- unlist(lapply(packages, names), use.names=FALSE)
  package_names_all <- deps_recursive(package_names, package_info)$name
  system <- deps_system(c(package_names, package_names_all),
                        package_info)
  repos <- deps_repos(info)

  c(list(system=union(system, config$system), repos=repos),
    packages)
}

deps_repos <- function(info) {
  repos <- NULL
  if (info$is_package) {
    pkg <- devtools::as.package(info$path_package)
    if (!is.null(pkg$additional_repositories)) {
      repos <- c("http://cran.rstudio.com",
                 strsplit(pkg$additional_repositories, "\\s*,\\s*")[[1]])
    }
  }
  repos
}

deps <- function(package_names,
                 github_repos=NULL,
                 local_paths=NULL,
                 local_dependencies=TRUE,
                 keep_devtools=TRUE) {
  ## First, ensure that we have all the github data we need:
  deps_github_fetch(github_repos)

  ## Then get names for our github repos and file repos:
  names(package_names) <- package_names
  if (!is.null(github_repos)) {
    names(github_repos) <- deps_github_package_names(github_repos)
  }
  if (!is.null(local_paths)) {
    names(local_paths) <- deps_local_package_names(local_paths)
  }

  ## Move dependencies of local files into the main set
  if (local_dependencies && length(local_paths) > 0L) {
    ## TODO: Do we want to get *all* local dependencies here?
    deps_extra_local <-
      sort(unique(unlist(lapply(local_paths, function(x)
        devtools:::pkg_deps(x, TRUE)$name))))

    ## Collect all dependencies of
    exclude <- c(names(local_paths),
                 names(github_repos),
                 names(package_names),
                 base_packages())
    deps_extra_local <- setdiff(deps_extra_local, exclude)
    names(deps_extra_local) <- deps_extra_local
    package_names <- c(package_names, deps_extra_local)
  }

  ## Then go the other way and exclude packages known from elsewhere:
  drop_github <- names(github_repos) %in% names(local_paths)
  if (any(drop_github)) {
    github_repos <- github_repos[!drop_github]
  }

  ## However, take care not to drop devtools from the R list.
  non_R <- c(names(local_paths), names(github_repos))
  if (keep_devtools) {
    non_R <- setdiff(non_R, "devtools")
  }
  drop_R <- package_names %in% non_R
  if (any(drop_R)) {
    package_names <- package_names[!drop_R]
  }

  list(R=package_names,
       github=github_repos,
       local=local_paths)
}

deps_recursive <- function(package_names, package_info, all=FALSE) {
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
  while (length(package_names) > 0L) {
    i <- package_names %in% rownames(package_info)
    str <- character(0)
    ## From PACKAGES:
    if (any(i)) {
      str <- c(str, apply(package_info[package_names[i], v, drop=FALSE],
                          1, join_deps))
    }
    ## Offline, or locally installed
    if (any(!i)) {
      str <- c(str, sapply(package_descriptions(package_names[!i]),
                           join_deps))
    }
    x <- join(lapply(str, devtools:::parse_deps))
    seen <- c(seen, package_names)
    deps <- c(deps, list(x))
    package_names <- setdiff(x$name, seen)
  }

  deps_all <- do.call("rbind", deps)
  deps_all <- deps_all[order(deps_all$name), ]
  rownames(deps_all) <- NULL
  deps_all
}

deps_system <- function(package_names, package_info) {
  package_names <- sort(setdiff(unique(package_names), base_packages()))

  ok <- package_names %in% rownames(package_info)
  sys_reqs <- character(0)
  if (any(ok)) {
    sys_reqs <- c(sys_reqs,
                  package_info[package_names[ok], "SystemRequirements"])
  }

  if (any(!ok)) {
    ## Failed to find the requirements in the database, so try
    ## locally:
    f <- function(package_description) {
      ret <- description_field(package_description, "SystemRequirements")
      if (is.null(ret)) NA_character_ else ret
    }
    pkgs_msg <- lapply(package_names[!ok], package_descriptions)
    names(pkgs_msg) <- package_names[!ok]
    sys_reqs <- c(sys_reqs, vapply(pkgs_msg, f, character(1)))
  }

  ## Convert impossible to understand requirements:
  sys_reqs <- system_requirements_sanitise(sys_reqs)
  ## And convert those into apt get requirements:
  sys_reqs <- system_requirements_apt_get(sys_reqs)

  ## Provide a message (but not a warning) about system requirements
  ## that might be unsatisified.
  if (length(sys_reqs$unresolved) > 0) {
    unresolved_name <- names(sys_reqs$unresolved)
    unresolved_pkg  <- sapply(sys_reqs$unresolved, paste, collapse=", ")
    msg <- paste(sprintf("\t- %s: %s", unresolved_name, unresolved_pkg),
                 collapse="\n")
    msg <- paste("Unresolved SystemRequirements:", msg,
                 "Suppress with dockertest.yml:system_ignore_packages",
                 sep="\n")
    message(msg)
  }

  sys_reqs$resolved
}

##' @importFrom downloader download
deps_github_fetch <- function(repos) {
  dir.create(deps_github_paths(""), FALSE, TRUE)
  fmt <- "https://raw.githubusercontent.com/%s/master/DESCRIPTION"
  dat <- list()
  for (r in repos) {
    path_r <- deps_github_paths(r)
    dest_r <- file.path(path_r, "DESCRIPTION")
    dir.create(path_r, FALSE, TRUE)
    message("Fetching DESCRIPTION for ", r)
    ok <- try(downloader::download(sprintf(fmt, r),
                                   dest_r, quiet=TRUE))
  }
}

deps_github_paths <- function(repos) {
  file.path(user_data_dir(), "github", repos)
}

deps_local_package_names <- function(paths) {
  description_package_names(paths)
}

deps_github_package_names <- function(repos) {
  description_package_names(deps_github_paths(repos))
}

deps_fetch_package_info <- function(github_repos, local_paths) {
  dat_crandb <- deps_fetch_package_info_crandb()
  dat_extra  <- deps_fetch_package_info_extra(github_repos, local_paths)
  dat <- rbind(dat_crandb, dat_extra)
  dat[!duplicated(rownames(dat), fromLast=TRUE), , drop=FALSE]
}

##' @importFrom httr GET content
##' @importFrom jsonlite fromJSON
deps_fetch_package_info_crandb <- function(force=FALSE) {
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

deps_fetch_package_info_extra <- function(github_repos,
                                     local_paths) {
  github_paths <- deps_github_paths(github_repos)
  dat <- lapply(c(github_paths, local_paths),
                description_dependencies)
  ret <- do.call("rbind", dat)
  rownames(ret) <- ret[, "Package"]
  ret
}

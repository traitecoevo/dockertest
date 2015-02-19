dependencies <- function(info, config) {
  ## Determine the *names* of packages that we depend on
  ## (`package_names`).  This is just the immediate set; not including
  ## their dependencies yet.

  ## NOTE: Being sneaky, using hidden function `pkg_deps` to parse
  ## dependencies; need to write my own for this.
  path_project <- info$path_project
  if (info$is_package) {
    pkg <- devtools::as.package(info$path_package)
    package_names <- devtools:::pkg_deps(pkg, dependencies=TRUE)[, "name"]
    ## Some packages come from different places:
    if (is.null(pkg$additional_repositories)) {
      repos <- NULL
    } else {
      repos <- c("http://cran.rstudio.com",
                 strsplit(pkg$additional_repositories, "\\s*,\\s*")[[1]])
    }
  } else {
    package_names <- character(0)
    repos <- NULL
  }

  ## Gather metadata information for all CRAN packages and all
  ## referenced github packages.  This uses crandb, and I need to work
  ## out a way of gracefully expiring that information.  The github
  ## bits are redownloaded each time.

  ## For packages, we take the list of required packages and split
  ## them up into github and non-github packages, based on config.
  ## This function may identify additional github packages that are
  ## needed from .travis.yml and from .dockertest.yml
  deps_packages <- dependencies_packages(package_names,
                                         config, path_project)

  ## Recompute the list of package names here, including any ones that
  ## we added.
  package_names <- c(deps_packages$R,
                     names(deps_packages$github),
                     names(deps_packages$local))

  package_info <- fetch_PACKAGES(deps_packages$github,
                                 deps_packages$local,
                                 info$path_build)

  ## System dependencies, based on this.  We use
  ##   1. github information
  ##   2. CRAN information
  ##   3. local package installation information
  ## in decending order of preference.  We then try to coerce this
  ## into a set of suitable packages for travis.  Hints in
  ## .dockertest.yml and .travis.yml may come in helpful here.
  deps_system <- dependencies_system(package_names, package_info,
                                     config, path_project)

  c(list(system=deps_system, repos=repos), deps_packages)
}

dependencies_system <- function(package_names, package_info,
                                config, path_project=NULL) {
  ## These are required by the bootstrap:
  dockertest_system <- c("curl", "ca-certificates", "git",
                         "libcurl4-openssl-dev", "ssh")

  ## Starting with our package dependencies we can identify the full
  ## list.

  dat <- package_dependencies_recursive(package_names, package_info)
  pkgs <- sort(unique(c(package_names, dat$name)))
  pkgs <- setdiff(pkgs, config[["system_ignore_packages"]])
  pkgs <- setdiff(pkgs, base_packages())

  ok <- pkgs %in% rownames(package_info)
  sys_reqs <- character(0)
  if (any(ok)) {
    sys_reqs <- c(sys_reqs, package_info[pkgs[ok], "SystemRequirements"])
  }
  if (any(!ok)) {
    ## Failed to find the requirements in the database, so try
    ## locally:
    f <- function(package_description) {
      ret <- description_field(package_description, "SystemRequirements")
      if (is.null(ret)) {
        NA_character_
      } else {
        ret
      }
    }
    pkgs_msg <- lapply(pkgs[!ok], package_descriptions)
    names(pkgs_msg) <- pkgs[!ok]
    sys_reqs <- c(sys_reqs, vapply(pkgs_msg, f, character(1)))
  }

  ## Convert impossible to understand requirements:
  sys_reqs <- system_requirements_sanitise(sys_reqs)
  ## And convert those into apt get requirements:
  sys_reqs <- system_requirements_apt_get(sys_reqs)
  ## Merge in travis' apt get requirements:
  sys_reqs_travis <- system_requirements_travis(path_project)
  if (!is.null(sys_reqs_travis)) {
    sys_reqs$resolved <- sort(unique(c(sys_reqs$resolved, sys_reqs_travis)))
  }

  ## Provide a message (but not a warning) about system requirements
  ## that might be unsatisified.
  if (length(sys_reqs$unresolved) > 0) {
    unresolved_name <- names(sys_reqs$unresolved)
    unresolved_pkg  <- sapply(sys_reqs$unresolved, paste, collapse=", ")
    msg <- paste(sprintf("\t- %s: %s", unresolved_name, unresolved_pkg),
                 collapse="\n")
    msg <- paste("Unresolved SystemRequirements:", msg,
                 "Suppress with .dockertest.yml:system_ignore_packages",
                 sep="\n")
    message(msg)
  }

  sort(unique(c(dockertest_system,
                sys_reqs$resolved,
                config[["system"]])))
}

dependencies_packages <- function(package_names, config, path_project) {
  dockertest_packages <- c("devtools", "testthat")
  ## Start with all the packages:
  deps_R <- setdiff(union(dockertest_packages, package_names),
                    base_packages())
  deps_github <- unique(c("richfitz/dockertest",
                          packages_github_travis(path_project),
                          config[["packages"]][["github"]]))
  deps_local <- config[["packages"]][["local"]]

  ## Now, filter things so that local packages replace github packages
  ## and github packages replace R (CRAN) packages.
  names(deps_R)      <- deps_R
  names(deps_github) <- github_package_name(deps_github)
  names(deps_local)  <- local_package_name(deps_local)

  ## If we have local packages, we need to manually add their
  ## dependencies because the way I install them won't do that
  ## automatically.  This is complete hack:
  if (length(deps_local) > 0L) {
    deps_extra_local <-
      sort(unique(unlist(lapply(deps_local,
                                function(x) devtools:::pkg_deps(x)$name))))
    ## TODO: we could use this to organise the order of local
    ## installation.
    exclude <- c(names(deps_local),
                 names(deps_github),
                 names(deps_R),
                 base_packages())
    deps_extra_local <- setdiff(deps_extra_local, exclude)
    names(deps_extra_local) <- deps_extra_local
    deps_R <- c(deps_R, deps_extra_local)
  }

  ## Then go the other way and exclude packages known from elsewhere:
  drop_github <- names(deps_github) %in% names(deps_local)
  if (any(drop_github)) {
    deps_github <- deps_github[!drop_github]
  }

  ## However, take care not to drop devtools from the R list.
  non_R <- c(names(deps_local), names(deps_github) )
  drop_R <- names(deps_R) %in% setdiff(non_R, "devtools")
  if (any(drop_R)) {
    deps_R <- deps_R[!drop_R]
  }

  list(R=deps_R, github=deps_github, local=deps_local)
}

## No validation here yet.
load_config <- function(path_project=NULL) {
  ## We'll look in the local directory and in the package root.
  config_file_local <- ".dockertest.yml"
  config_file_package <- file.path(find_project_root(path_project),
                                   ".dockertest.yml")
  if (file.exists(config_file_local)) {
    ## Ideally here we'd merge them, but that's hard to do.
    if (config_file_local != config_file_package &&
        file.exists(config_file_package)) {
      warning("Ignoring root .dockertest.yml", immediate.=TRUE)
    }
    config_file <- config_file_local
  } else {
    config_file <- config_file_package
  }
  defaults <- list(system_ignore_packages=NULL,
                   system=NULL,
                   packages=list(github=NULL, local=NULL),
                   image="r-base")
  if (file.exists(config_file)) {
    ret <- yaml_read(config_file)
    modifyList(defaults, ret)
  } else {
    defaults
  }
}

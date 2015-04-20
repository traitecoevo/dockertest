## Creating the System depends bit by hand is a total PITA.  Might be
## worth going through and trying to hit up all the packages to get
## dependency information.  Given that most people are likely to run
## this on a machine where they have all the packages installed, we
## can go through and be fairly clever about this.

## The system requirements field is pretty ugly, but they can be
## parsed pretty easily by eye.  I've done a few in
## system_requirements_sanitise.yml but these are just a handful that
## I saw.
system_requirements_sanitise <- function(reqs, ignore=NULL) {
  file <- system.file("system_requirements_sanitise.yml",
                      package="dockertest", mustWork=TRUE)
  dat <- yaml_read(file)

  ## Drop ignored packages:
  reqs <- reqs[setdiff(names(reqs), ignore)]
  ## And packages that are ignored
  reqs <- reqs[!is.na(reqs)]

  ## Replacements:
  i <- match(names(reqs), names(dat))
  j <- !is.na(i)
  reqs[j] <- dat[i[j]]

  ## Then, we can try and parse names out using devtools:
  f <- function(x) {
    ret <- try(devtools:::parse_deps(x)$name, silent=TRUE)
    if (inherits(ret, "try-error")) {
      ret <- x
    }
    ret
  }
  lapply(reqs, f)
}

## This translates from requirements in SystemRequirements (sanitised)
## to apt-get packages.  Returns a list with a set of required
## packages and unknown dependencies.
system_requirements_apt_get <- function(reqs) {
  file <- system.file("system_requirements_apt_get.yml",
                      package="dockertest",
                      mustWork=TRUE)
  dat <- yaml_read(file)
  i <- match(unlist(reqs), names(dat))
  resolved <- sort(unique(unlist(dat[i], use.names=FALSE)))
  unresolved <- unlist(reqs)[is.na(i)]
  if (length(unresolved) > 0) {
    unresolved <- split(names(unresolved), unresolved)
  }
  list(resolved=resolved, unresolved=unresolved)
}

## Try and get requirements from travis:
system_requirements_travis <- function(path_project=NULL) {
  path_project <- find_project_root(path_project)
  travis_yml <- file.path(path_project, ".travis.yml")
  if (file.exists(travis_yml)) {
    travis <- yaml_read(travis_yml)
    re <- ".*apt-get\\s+install\\s+"
    to_check <- c(travis$install, travis$before_script)
    x <- sub(re, "", grep(re, to_check, value=TRUE))
    unlist(strsplit(x, "\\s+"))
  }
}

packages_github_travis <- function(path_project=NULL) {
  path_project <- find_project_root(path_project)
  travis_yml <- file.path(path_project, ".travis.yml")
  if (file.exists(travis_yml)) {
    travis <- yaml_read(travis_yml)
    re <- ".*travis-tool\\.sh\\s+github_package\\s+"
    to_check <- c(travis$install, travis$before_script)
    x <- sub(re, "", grep(re, to_check, value=TRUE))
    unlist(strsplit(x, "\\s+"))
  }
}

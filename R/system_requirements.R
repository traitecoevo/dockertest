## Creating the System depends bit by hand is a total PITA.  Might be
## worth going through and trying to hit up all the packages to get
## dependency information.  Given that most people are likely to run
## this on a machine where they have all the packages installed, we
## can go through and be fairly clever about this.

## The system requirements field is pretty ugly, but they can be
## parsed pretty easily by eye.  I've done a few in
## system_requirements_sanitise.yml but these are just a handful that
## I saw.
system_requirements_sanitise <- function(reqs) {
  file <- system.file("system_requirements_sanitise.yml",
                      package="dockertest", mustWork=TRUE)
  dat <- yaml_read(file)

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

  ret <- sort(unique(unlist(lapply(reqs, f), use.names=FALSE)))
  ## Drop GNU make entirely:
  exclude <- "GNU make"

  setdiff(ret, exclude)
}

## This translates from requirements in SystemRequirements (sanitised)
## to apt-get packages.  Returns a list with a set of required
## packages and unknown dependencies.
system_requirements_apt_get <- function(reqs) {
  file <- system.file("system_requirements_apt_get.yml",
                      package="dockertest",
                      mustWork=TRUE)
  dat <- yaml_read(file)
  i <- match(reqs, names(dat))
  resolved <- sort(unique(unlist(dat[i], use.names=FALSE)))
  unresolved <- reqs[is.na(i)]
  list(resolved=resolved, unresolved=unresolved)
}

## Try and get requirements from travis:
system_requirements_travis <- function(path_package=NULL) {
  path_package <- find_package_root(path_package)
  travis_yml <- file.path(path_package, ".travis.yml")
  if (file.exists(travis_yml)) {
    travis <- yaml_read(travis_yml)
    re <- ".*apt-get\\s+install\\s+"
    to_check <- c(travis$install, travis$before_script)
    x <- sub(re, "", grep(re, to_check, value=TRUE))
    unlist(strsplit(x, "\\s+"))
  }
}

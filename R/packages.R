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

description_fields <- function() {
  c("Package",
    "Depends", "Imports", "LinkingTo", "Suggests", "VignetteBuilder",
    "SystemRequirements")
}

## These might be a bit more general:
description_package_names <- function(paths) {
  f <- function(x) {
    description_field(devtools::as.package(x),  "Package")
  }
  unname(vapply(paths, f, character(1)))
}

description_dependencies <- function(path) {
  ret <- read.dcf(file.path(path, "DESCRIPTION"))
  ret <- drop(ret)[description_fields()]
  names(ret) <- description_fields()
  ret
}

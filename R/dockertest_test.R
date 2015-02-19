##' @importFrom whisker whisker.render
dockerfile_test <- function(info) {
  config <- load_config(info$path_project)
  deps <- dockertest_dependencies(info, config)

  ## Repos needs to go in as a long arg:
  if (!is.null(deps$repos)) {
    deps$repos <- sprintf("--repos=%s", deps$repos)
  }
  if (!is.null(deps$local)) {
    deps$local <- paste(file.path("/local", names(deps$local)),
                        collapse=" ")
  }
  pretty <- function(x) {
    if (length(x) <= 1) x else paste(c("", sort(x)), collapse="  \\\n    ")
  }
  for (i in c("system", "github", "R", "repos")) {
    deps[[i]] <- unname(pretty(deps[[i]]))
  }

  template <- system.file("Dockerfile.whisker", package="dockertest",
                          mustWork=TRUE)
  dat <- list(image=config[["image"]], dependencies=deps)
  str <- whisker.render(readLines(template), dat)
  invisible(str)
}

prepare_test <- function(suffix="-test") {
  info <- project_info(suffix)
  dir.create(info$path_build, FALSE, TRUE)
  writeLines(dockerfile_test(info),
             file.path(info$path_build, "Dockerfile"))
  write_scripts(info)
}

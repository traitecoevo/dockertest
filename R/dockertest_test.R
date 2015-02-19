##' @importFrom whisker whisker.render
dockerfile_test <- function(info) {
  config <- info$config
  deps <- dockertest_dependencies(info)

  ## Repos needs to go in as a long arg:
  if (!is.null(deps$repos)) {
    deps$repos <- sprintf("--repos=%s", deps$repos)
  }
  if (!is.null(deps$local)) {
    deps$local <- paste(file.path("/local", names(deps$local)),
                        collapse=" ")
  }
  for (i in c("system", "github", "R", "repos")) {
    deps[[i]] <- docker_join_commands(deps[[i]], TRUE)
  }

  template <- system.file("Dockerfile.whisker", package="dockertest",
                          mustWork=TRUE)
  ## TODO: This should be image_from I think.
  dat <- list(image=config[["image"]], dependencies=deps)
  str <- whisker.render(readLines(template), dat)
  invisible(str)
}

prepare_test <- function(info) {
  dir.create(info$path_build, FALSE, TRUE)
  writeLines(dockerfile_test(info),
             file.path(info$path_build, "Dockerfile"))
  write_scripts(info)
}

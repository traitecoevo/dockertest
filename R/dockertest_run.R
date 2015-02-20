## "Production" dockerfiles that include the source code too.
##
## Ideally, we'd also allow additional paths to be allowed here, I
## think.
dockerfile_run <- function(info) {
  ## A post install script.  It's a list because that's going to let
  ## us drop things by making them NULL, and possibly later run more
  ## clever sanitisation of commands.
  install_package <- sprintf("install2.r --error --repos=NULL %s",
                             info$name)

  commands <- c(
    list(),
    docker_FROM(project_info("test")$tagname),
    docker_COPY("src", info$name),
    if (info$is_package) docker_RUN(install_package)
  )

  format_docker(commands)
}

prepare_run <- function(info) {
  dir.create(info$path_build, FALSE, TRUE)
  prepare_run_clone(info)
  writeLines(dockerfile_run(info),
             file.path(info$path_build, "Dockerfile"))
  write_scripts(info)
}

prepare_run_clone <- function(info) {
  dest_src <- file.path(info$path_build, "src")
  if (file.exists(dest_src)) {
    unlink(dest_src, recursive=TRUE)
  }
  add_to_gitignore(dest_src)
  git_clone(info$path_project, dest_src)
  ## Perhaps disconnect from remote:
  ## system2("git", c(sprintf("--git-dir=%s/.git", dest_src),
  ##                  "remote", "rm", "origin"))
}

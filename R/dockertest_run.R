## "Production" dockerfiles that include the source code too.
##
## Ideally, we'd also allow additional paths to be allowed here, I
## think.
dockerfile_run <- function(info) {
  ## A post install script.  It's a list because that's going to let
  ## us drop things by making them NULL, and possibly later run more
  ## clever sanitisation of commands.
  install <- c(sprintf("git clone /src %s", info$name),
               sprintf("install2.r --error --repos=NULL %s",
                       info$name),
               "rm -rf /src")

  commands <- c(
    list(),
    FROM=project_info("test")$tagname,
    COPY="src /src",
    RUN=if (info$is_package) docker_join_commands(install, list=FALSE),
    WORKDIR=info$name)

  format_docker(commands)
}

prepare_run <- function(info) {
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

## "Production" dockerfiles without source code
##
## Ideally, we'd also allow additional paths to be allowed here, I
## think.
dockerfile_production <- function(info) {
  if (!info$is_package) {
    stop("This can only be used for packages")
  }
  install <- c("install2.r --error --repos=NULL /src",
               "rm -rf /src")
  commands <- c(
    list(),
    FROM=project_info("test")$tagname,
    COPY="src /src",
    RUN=docker_join_commands(install, list=FALSE))
  format_docker(commands)
}

prepare_production <- function(info) {
  prepare_production_clone(info)
  writeLines(dockerfile_production(info),
             file.path(info$path_build, "Dockerfile"))
  write_scripts(info)
}

prepare_production_clone <- function(info) {
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

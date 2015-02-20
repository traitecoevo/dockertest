## "Production" dockerfiles without source code
##
## Ideally, we'd also allow additional paths to be allowed here, I
## think.
##
## This differs from _run only in that no trace of the sources are
## left behind.
##
## TODO: Perhaps run this from _run and just nuke the wd back to /home?
dockerfile_production <- function(info) {
  if (!info$is_package) {
    stop("This can only be used for packages")
  }
  commands <- c(
    list(),
    docker_FROM(project_info("run")$tagname),
    docker_RUN(paste0("rm -rf ", info$name)))
  format_docker(commands)
}

prepare_production <- function(info) {
  writeLines(dockerfile_production(info),
             file.path(info$path_build, "Dockerfile"))
  write_scripts(info)
}

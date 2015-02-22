dockerfile_dockertest <- function(info) {
  config <- info$config

  info <- add_project_deps(info)
  deps <- dockertest_dependencies(info)
  path <- file.path("/root", info$name)
  copy_sources <- docker_copy_sources(path,
                                      info$local_filesystem,
                                      info$path_self)
  copy_scripts <- docker_r('dockertest:::copy_scripts_dir("/usr/local/bin")')

  if (info$install_package) {
    if (!info$is_package) {
      stop("Makes no sense")
    }
    post_install <- c(list(),
                      copy_scripts,
                      copy_sources,
                      docker_RUN(c(paste("R CMD INSTALL", path),
                                   paste("rm -rf", path))),
                      info$post_install)
    workdir <- "/root"
  } else {
    post_install <- c(list(),
                      copy_scripts,
                      copy_sources,
                      info$post_install)
    workdir <- path
  }

  if (is.null(deps$local)) {
    local_paths <- NULL
  } else {
    local_paths <- file.path(info$path_local, names(deps$local))
  }

  c(list(),
    docker_FROM(info$config$image),
    docker_apt_get_install(deps$system),
    docker_install2(deps$R, deps$repos),
    docker_install_github(deps$github),
    ## Local paths are relative to the build directory.
    docker_install_local(local_paths),
    post_install,
    docker_WORKDIR(workdir),
    docker_CMD("bash"))
}

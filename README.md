# dockertest

Proof of concept of generating Dockerfiles from R packages and other R projects, and using them to run tests.

Use cases:

* **Packages:** Build a docker container that contains all your package dependencies, and use this to test installation, tests and `R CMD check` within, independent of your system.  This gives the isolated environment of [travis](https://travis-ci.org) with a much faster cycle time (no installation time, potentially no slower than running the tests in the package directory).
* **Projects**:
  - **start:** Build images that contain all the dependencies of a project to run in a container-supporting cloud environment
  - **end:** Build images that contain a project in its completely run state.

Below documentation is listed for *packages*, then for *projects*.  Below that is the general configuration options that apply across all types.

## Installing docker

To run the material below you need to have [docker installed](http://docs.docker.com/installation/) and running.


## Packages

The package you are supporting must be at the root of a git repository, but you'll want to run dockertest from within a subdirectory (e.g., `docker/`) because we copy files and clone some repos around.  So your directory structure should look like

```
repo
 +- R/
 +- man/
 +- docker/
 +- DESCRIPTION
```

Also add to your `.Rbuildignore` a line that says

```
^docker$
```

to keep `R CMD check` happy.  Then from within the `docker` directory run:

```
dockertest::build()
```

to build an image that contains all the package dependencies.  By default this image will be called `dockertest/<package_name>-test` with `<package_name>` being your package name, converted to lower case. The dockerfile to create the image will be in `<package_name>-test/Dockerfile` and a script `<package_name>-test/launch.sh` is created.

In order to aid rapid testing, your sources are *not* included in this container: instead, the container will clone them when you start it - that way you don't need to rebuild the container every time you update your project, and only when the dependencies are updated.

Running the launch script with no arguments will clone your project from your local disk (so you get the most recent *checked in* sources -- not your working directory).  You can also pass it the names of a program to run, so

```
<package_name>-test/launch.sh R
```

will launch R, after cloning the sources.

There are also some convenience scripts that are installed in the docker image so can be used as additional arguments to the launch script:

* `<package_name>-test/launch.sh check`: Runs `R CMD check` on the fresh clone
* `<package_name>-test/launch.sh devtools_test`: Runs `devtools::test()` on the fresh clone
* `<package_name>-test/launch.sh devtools_check`: Runs `devtools::check()` on the fresh clone
* `<package_name>-test/launch.sh R_test`: clone your repo and launch R in the `tests/testthat` directory, load the package (with `devtools::load_all()`) and read all helper files.  This means you can start interactively evaluating tests in approximately the way that devtools would work.

## Projects

Non-package projects can be accessed in the same way. As before, you'll want to run dockertest from within a subdirectory (e.g., `docker/`) because we copy files and clone some repos around.  So your directory structure should look like

```
project
 +- ...
 +- docker/
 +- README
```

Next you need to specify any depenencies you want included in the image -- see the [Configuration](#configuration) section.

Then from within the `docker` directory, run

```
dockertest::build()
```

to build an image that contains project dependencies, and clones your project into `/root/<project_name>` (which will be the working directory when the container starts).

The dockerfile to create the image will be in `<package_name>-test/Dockerfile` and a script `<package_name>-test/launch.sh` is created.

To aid development, project sources are *not* included in this container: instead, the container will clone them when you start it - that way you don't need to rebuild the container every time you update your project, and only when the dependencies are updated.

Running the launch script with no arguments will clone your project from your local disk (so you get the most recent *checked in* sources -- not your working directory).  You can also pass it the names of a program to run, so

```
<package_name>-test/launch.sh R
```

## Remake

[Remake](https://github.com/richfitz/remake)-based projects can be handled in a more automated way, though this is not selected automatically at present.  Running

```
dockertest::build_remake()
```

will build a container that has all of the dependencies and the sources unpacked in `/root/<project_name>` as for "Projects" above.  (This is called the "clean" continer and will be called `dockertest/<project_name>-clean` by default.

In addition, you can pass `build_remake` an target name as an argument and it will build that as part of the build:

```
dockertest::build_remake("all")
```

will build a container *based on the clean container* by making the "all" target.  The resulting container will be called `dockertest/<project_name>-<target_name>`.


## Configuration

Dependnecies are added via a [yaml](https://yaml.org) file saved at `root/docker/dockertest.yml. See [here](https://github.com/dfalster/baad/blob/master/docker/dockertest.yml) for a worked example. These must be specified *before* you build your container.

### Base image

Dockerfiles use `FROM r-base` by default - this is the official R docker image.  You can change the base image with the `image:` key (no effect on remake images, which are always built `FROM richfitz/remake`)

```yaml
image: rocker/ropensci
```

### Image names

For packages and projects:

```yaml
names:
  test: dockerhubname/imagename
  run: dockerhubname/anotherimagename
```

For remake files:

```yaml
names:
  clean: dockerhubname/imagename
  all: dockerhubname/imagename_all
  anothertarget: dockerhubname/imagename_anothertarget
```

### Dependency identification

For **packages**, most dependency information can be gathered from `DESCRIPTION` and for remake projects most can be gathered from package lists in `remake.yml`.  However some may still be required, and can be specified in yaml format in the file `dockertest.yml` within the working directory (the keys here follow the format of the [new r travis support](http://docs.travis-ci.com/user/languages/r/):

```yaml
r_packages:
  - an.r.package
r_github_packages:
  - richfitz/RcppR6
r_local_packages:
  - ../path/to/local/package
```

The `r_packages` section should generally be required only for non-remake projects, as both packages and remake projects should identify all packages.  However, sometimes a package that is listed in `Suggests` of a package that you depend on is really needed.

The `r_github_packages` section is useful for non-CRAN packages or where a more recent version is required.

The `r_local_packages` section lists local packages *relative to the project root* (not the working directory).  They must also be git repositories as we'll clone them.  The package name need not match the repository or directory name (the true package name will be gathered from the `DESCRIPTION`) but the package must not be in a subdirectory.  The path here will usually start with `../`.

**System depenendencies** are also automatically detected for some packages.  We attempt to work out what system dependencies might be required for your package, and its dependencies, by scanning through all the packages that your package depends on (and their dependencies and so on), then looking at the `SystemDependencies` field and translating that into a set of Debian packages.  This process is unsurprisingly pretty flakey, but if it's useful it will be fairly easy to extend (just edit [this file](https://github.com/traitecoevo/dockertest/blob/master/inst/system_requirements_sanitise.yml) to turn unparseable `SystemDependencies` fields into something that looks like `Depends` and [this file](https://github.com/traitecoevo/dockertest/blob/master/inst/system_requirements_apt_get.yml) to turn *that* into a set of Debian packages.  Eventually we can use [metacran/sysreqs](https://github.com/metacran/sysreqs) but that's a way off so far.

dockertest will also scan for system dependencies in a `.travis.yml` file if that exists, by looking for calls to `apt-get install`.

R packages for which system dependencies could not be resolved will result in a message.  You can manually specify system package requirements for these packages by listing them in the `apt_packages` field (list Debian package name)

```yaml
apt_packages:
  - libgsl0-dev
```

You may also want to list the R packages to ignore as

```yaml
system_ignore_packages:
  - package1
  - package2
```

which will suppress the message.

### Other options

* `deps_only:` (`true` / `false`) -- Don't arrange to put a copy of the sources into the docker container.  A copy is still available in the `self` directory though, which might get mounted into the container.
* `keep_git:` (`true` / `false`) -- Keep the .git directory in the container
*  `inplace:` (`true` / `false`) -- By default, docker will clone your project into the directory `self`, so that it avoids any uncommited files (which may be large and result in sending a lot of context to the build daemon).  To avoid this and build in place set `inplace: true`

If project source is a moving target, then setting `deps_only: true` and `keep_git: true` is probably a good idea.

### Modules

Modules can be loaded which include bundles of dependencies.  For example

```
modules:
  - rrqueue
```

will install packages required for `rrqueue`.  Other modules are:

* `remake`
* `callr`

Modules are just dockertest yaml files containing only the fields `r_packages`, `r_github_packages`, `apt_packages` and `commands`.  If a module `foo` is listed, dockertest looks for `foo.yml`, first in the working directory and then in [dockertest's files](https://github.com/traitecoevo/dockertest/tree/master/inst/modules) for `foo.yml`.  Use them to keep complexity out of the main dockertest file, or to create reusable combinations of packages.

### Additional commands

The `commands:` section can be used to add arbitrary docker commands to the Dockerfile at given points.  For example, in the dockertest bootstrap, we use:

```
commands:
  install_dockertest_scripts:
    command: RUN r -e 'dockertest:::copy_scripts_dir("/usr/local/bin")'
    after: github
```

which runs a bit of R code after installing the github packages.  Valid options for `after` are (in order if execution)
* `apt_packages`: after apt packages are installed but before any R packages are installed
* `r_packages`: after CRAN packages are installed
* `r_github_packages`: after github packages are installed
* `r_local_packages`: after local packages are installed
* `workdir` after the `WORKDIR` directive, so very late in the build

Multiple entries are allowed for a given `after` setting; don't rely on the order though.

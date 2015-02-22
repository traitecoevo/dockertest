# dockertest

Proof of concept of generating Dockerfiles from R packages and other R projects, and using them to run tests.

Build a docker container that contains all your package dependencies, and use this to test installation, tests and `R CMD check` within, independent of your system.  This gives the isolated environment of [travis](https://travis.org) with a much faster cycle time (no installation time, potentially no slower than running the tests in the package directory).

## Packages

The package must be at the root of a git repository, and you'll want to run dockertest from within a subdirectory (e.g., `docker/`) because we copy and clone a few files around.  Add that path to `.Rbuildignore`, and possibly to `.gitignore`.  Then run:

```
dockertest::build()
```

to build an image that contains all the package dependencies.  By default this image will be called `dockertest/<package_name>-test` with `<package_name>` being your package name, converted to lower case.  the dockerfile to create the image will be in `<package_name>-test/Dockerfile` and a script `<package_name>-test/launch.sh` is created.

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

Non-package projects can be used in the same way:

```
dockertest::build()
```

will build an image that contains project dependencies, and clone your project into `/root/<project_name>` (which will be the working directory when the container starts).  You will have to specify required packages however -- see the Configuration section.

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

Configuration is via a [yaml](https://yaml.org) file `dockertest.yml`

Dockerfiles use `FROM r-base` by default.  You can change the base image with the `image:` key (no effect on remake images, which are always built `FROM richfitz/remake`)

```yaml
image: rocker/ropensci
```

By default, docker will clone your project into the directory `self`, so that it avoids any uncommited files (which may be large and result in sending a lot of context to the build daemon).  To avoid this and build in place use

```
inplace: true
```

## Image names

For packages and projects:

```yaml
names:
  test: dockerhubname/imagename
  run: dockerhubname/anotherimagename
```

For remake files:

```
names:
  clean: dockerhubname/imagename
  all: dockerhubname/imagename_all
  anothertarget: dockerhubname/imagename_anothertarget
```

For example, see [here](https://github.com/dfalster/baad/blob/master/docker/dockertest.yml).

### Dependency identification

For **packages**, most dependency information can be gathered from `DESCRIPTION` and for remake projects most can be gathered from package lists in `remake.yml`.  However some may still be required, and can be specified in yaml format in the file `dockertest.yml` within the working directory:

```yaml
packages:
  R:
    - an.r.package
  github:
    - richfitz/RcppR6
  local:
    - path/to/local/package
```

The `R` section should only be required for non-remake projects, as both packages and remake projects should identify all packages.  The `github` section is useful for non-CRAN packages or where a more recent version is required.  The `local` section lists local packages *relative to the project root* (not the working directory).  They must also be git repositories.  The package name need not match the repository or directory name (the true name will be gathered from the `DESCRIPTION`) but the package must not be in a subdirectory.

**System depenendencies** are also automatically detected for some packages.  We attempt to work out what system dependencies might be required for your package, and its dependencies, by scanning through all the packages that your package depends on (and their dependencies and so on), then looking at the `SystemDependencies` field and translating that into a set of Debian packages.  This process is unsurprisingly pretty flakey, but if it's useful it will be fairly easy to extend (just edit [this file](https://github.com/richfitz/dockertest/blob/master/inst/system_requirements_sanitise.yml) to turn unparseable `SystemDependencies` fields into something that looks like `Depends` and [this file](https://github.com/richfitz/dockertest/blob/master/inst/system_requirements_apt_get.yml) to turn *that* into a set of ubuntu packages.

dockertest will also scan for system dependencies in a `.travis.yml` file if that exists, by looking for calls to `apt-get install`.

Packages for which dependencies could not be resolved will result in a message.  You can manually specify system package requirements for these packages by listing them in the `system` field (list ubuntu package name)


```yaml
system:
  - libgsl0-dev
```

You may also want to list the packages to ignore as

```yaml
system_ignore_packages:
  - package1
  - package2
```

which will suppress the message.

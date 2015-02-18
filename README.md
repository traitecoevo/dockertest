# dockertest

Proof of concept of generating Dockerfiles from R packages, and using them to
run tests.  Your package must be in the root of a git repository

Most metadata is gathered from the `DESCRIPTION`.

In the package root or in the working directory, a file `.dockertest.yml`, in yaml format can describe where github packages are and any system dependencies, e.g.:

```yaml
packages:
  github:
    - "richfitz/RcppR6"
system:
  - libgsl0-dev
```

We attempt to work out what system dependencies might be required for your package, and its dependencies, by scanning through all the packages that your package depends on (and their dependencies and so on), then looking at the `SystemDependencies` field and translating that into a set of Debian packages.  This process is unsurprisingly pretty flakey, but if it's useful it will be fairly easy to extend (just edit [this file](https://github.com/richfitz/dockertest/blob/master/inst/system_requirements_sanitise.yml) to turn unparseable `SystemDependencies` fields into something that looks like `Depends` and [this file](https://github.com/richfitz/dockertest/blob/master/inst/system_requirements_apt_get.yml) to turn *that* into a set of ubuntu packages.

dockertest will also scan for system dependencies in a `.travis.yml` file if that exists, by looking for calls to `apt-get install`.

Probably best to work in a subdirectory of your package (e.g., `<package_root>/docker`) because we'll move a few files around.  Add that file to `.Rbuildignore`, and possibly to `.gitignore`.

From within `<package_root>/docker`, run (in R)

```R
dockertest::build()
```

which will try to construct a Dockerfile, and create two scripts (`build.sh`) which can rebuild the docker image, and `launch.sh` which can load the image.  The launch script takes one option:

* For interactive use:
  * `R`: clone your repo and launch R
  * `R_test`: clone your repo and launch R in the `tests/testthat` directory, load the package (with `devtools::load_all()`) and read all helper files.  This means you can start interactively evaluating tests in approximately the way that devtools would work.
  * `bash`: don't clone your repo and launch bash.  If you do this, you can clone the repo later by running `clone.sh`
* For non-interactive use:
  * `check`: clone your repo and run `R CMD check` on it
  * `test` or `devtools_test` clone your repo and run `devtools::test()` on it
  * `devtools_check` clone your repo and run `devtools::check()` on it

The `launch.sh` script mounts the package root at `/src` on the docker image, so that you can see the actual source directory, and can clone from it - no need to push anywhere, or deal with keys, etc.  Your package is *not* copied across during the build, so you don't need to rebuild after changing it.

Dockerfiles use `FROM r-base` by default.  Adding `image: rocker/ropensci` or some other name will change this.

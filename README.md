# dockertest

Proof of concept of generating Dockerfiles from R packages, and using them to
run tests.

Most metadata is gathered from the `DESCRIPTION`

In the package root, a file `.dockertest.yml`, in yaml format can describe where github packages are and any system dependencies, e.g.:

```yaml
packages:
  github:
    - "richfitz/RcppR6"
system:
  - libgsl0-dev
```

Probably best to work in a subdirectory of your package (e.g., `<package_root>/docker`) because we'll move a few files around.  Add that file to `.Rbuildignore`.

From within `<package_root>/docker`, run (in R)

```R
dockertest::build()
```

which will try to construct a Dockerfile, and create two scripts (`build.sh`) which can rebuild the docker image, and `launch.sh` which can load the image.  The launch script takes one option:

* `R`: clone your repo and launch R
* `R_test`: clone your repo and launch R in the `tests/testthat` directory
* `check`: clone your repo and run `R CMD check` on it
* `devtools_test` clone your repo and run `devtools::test()` on it
* `devtools_check` clone your repo and run `devtools::check()` on it
* `bash`: don't clone your repo and launch bash.  If you do this, you can clone the repo later by running `clone.sh`

This mounts the package root at `/src` on the docker image, so that you can see the actual source directory, and can clone from it - no need to push anywhere, or deal with keys, etc.  Your package is *not* copied across during the build.

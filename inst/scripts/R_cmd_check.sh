#!/bin/sh
PACKAGE=src
clone.sh

CRAN=${CRAN:-"http://cran.rstudio.com"}
BIOC=${BIOC:-"http://bioconductor.org/biocLite.R"}
BIOC_USE_DEVEL=${BIOC_USE_DEVEL:-"TRUE"}
OS=$(uname -s)
R_BUILD_ARGS=${R_BUILD_ARGS-"--no-build-vignettes --no-manual"}
R_CHECK_ARGS=${R_CHECK_ARGS-"--no-build-vignettes --no-manual --as-cran"}

R CMD build ${R_BUILD_ARGS} $PACKAGE
# We want to grab the version we just built.
FILE=$(ls -1t *.tar.gz | head -n 1)

_R_CHECK_CRAN_INCOMING_=FALSE R CMD check "${FILE}" ${R_CHECK_ARGS} ${R_CHECK_INSTALL_ARGS}

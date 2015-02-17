#!/bin/sh
clone.sh
cd src/tests/testthat

cat >> .Rprofile <<EOF
library(testthat)
devtools::load_all("../../")
local({
  for (f in list.files(".", pattern="^helper")) {
    message("Reading ", f)
    sys.source(f, .GlobalEnv)
  }})
EOF

R

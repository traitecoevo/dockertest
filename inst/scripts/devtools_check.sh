#!/bin/sh
clone.sh

Rscript -e "devtools::check('src')"

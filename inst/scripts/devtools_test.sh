#!/bin/sh
clone.sh
Rscript -e "devtools::test('src')"

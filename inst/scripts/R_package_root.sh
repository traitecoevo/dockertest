#!/bin/sh
clone.sh
cd src

cat >> .Rprofile <<EOF
devtools::load_all()
EOF

R

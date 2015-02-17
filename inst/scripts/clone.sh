#!/bin/sh
SOURCE_DIR=src
if [ ! -d $SOURCE_DIR ]; then
    git clone --depth 1 file:///$SOURCE_DIR $SOURCE_DIR
fi

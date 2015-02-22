#!/bin/sh
SOURCE_DIR=$1
if [ -d /src/.git ]; then
    test -d $SOURCE_DIR/.git || git clone --depth 1 file:///src $SOURCE_DIR
fi

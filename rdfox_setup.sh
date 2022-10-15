#!/bin/sh

VERSION="5.7"
NAME="RDFox-linux-x86_64-$VERSION"
LINK="https://rdfox-distribution.s3.eu-west-2.amazonaws.com/release/v$VERSION/$NAME.zip"
DEST="lib"

mkdir -p "$DEST"
cd "$DEST"
wget "$LINK"
unzip "$NAME.zip"
ln -s "$NAME/lib/JRDFox.jar"
cd -

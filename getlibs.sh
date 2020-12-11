#!/bin/sh

VERSION="4.1.0"
NAME="RDFox-linux-$VERSION"
LINK="https://rdfox-distribution.s3.eu-west-2.amazonaws.com/release/v$VERSION/$NAME.zip"
DEST="./lib/"

mkdir -p "$DEST"
wget "$LINK"
unzip "$NAME.zip"
cp "./$NAME/lib/JRDFox.jar" "$DEST"

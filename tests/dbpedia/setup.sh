#!/bin/sh

BENCHMARK=dbpedia
wget "http://krr-nas.cs.ox.ac.uk/2015/jair/PAGOdA/$BENCHMARK.zip" && \
    unzip "$BENCHMARK.zip" && \
    mv "$BENCHMARK"/* ./ && \
    rm -rf "$BENCHMARK.zip" "$BENCHMARK/"

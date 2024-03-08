#!/bin/bash

echo "Building COBOL program"
cobc -O2 -x APICONV.cbl
echo "Building C converter"
cc -Ofast -shared -fPIC -o apimapper.so apimapper.c

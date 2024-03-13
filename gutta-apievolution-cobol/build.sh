#!/bin/bash

echo "Building COBOL program"
cobc -O2 -x APICONV.cbl
echo "Building C converter and timing helper"
cc -Ofast -shared -fPIC -o apimapper.so apimapper.c
cc -Ofast -shared -fPIC -o timer.so timer.c

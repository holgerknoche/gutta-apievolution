#!/bin/bash

echo "Building COBOL benchmark program"
cobc -O2 -x APICONV.cbl
echo "Building COBOL test progam"
cobc -O2 -x APITEST.cbl
echo "Building C converter and timing helper"
cc -Ofast -shared -fPIC -o apimapper.so apimapper.c
cc -Ofast -shared -fPIC -o timer.so timer.c

echo "Running test program"
./APITEST


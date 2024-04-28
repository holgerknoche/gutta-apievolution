#!/bin/bash

for i in $(seq 1 $NUMBER_OF_FORKS); do
    echo "Fork $i"
    ./APICONV
done

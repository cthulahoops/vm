#!/bin/bash

for i in tests/*.ss; do
    echo "$i"
    ./RunScheme "$i"
done

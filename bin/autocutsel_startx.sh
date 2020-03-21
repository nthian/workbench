#!/bin/bash

which autocutsel >/dev/null 2>&1
if [ $? -eq 0 ]; then
    pkill autocutsel
    autocutsel -fork
    autocutsel -selection PRIMARY -fork
else 
    echo "WARNING: autocutsel is not installed or in PATH=$PATH"
fi


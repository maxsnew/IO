#!/usr/bin/env bash
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <elm-file> <output-file>"
    exit 1
fi

elm-make $1 --output $2 && echo "Elm.worker(Elm.Main)" >> $2


#!/bin/bash

elm -mo Test.elm && echo "Making exe" && cat prescript.sh elm-runtime.js build/Test.js handler.js > runTest && chmod +x runTest

#!/bin/bash

cp public/index.html dist/
cp LICENSE dist/

elm make src/Main.elm --output=dist/fractal-play.js

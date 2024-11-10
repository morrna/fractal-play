#!/bin/bash

cp -R public/* dist/
cp LICENSE dist/

elm make src/Main.elm --output=dist/fractal-play.js

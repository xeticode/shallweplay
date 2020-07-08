#!/bin/bash
set -uo pipefail

NAME="build.sh v2020.06.03"
OUTPUT_DIR=./static/assets/js
OUTPUT_FILE=shallweplay.js
OUTPUT_PATH="${OUTPUT_DIR/%\//}/${OUTPUT_FILE}"

echo "Starting ${NAME}..."
echo ""

echo "NOTE: Must be executed from the project root directory."
echo ""

echo "...building elm..."
elm make ./src/Main.elm --optimize --output=./build/elm.js

echo "...compressing and mangling js..."
mkdir -p "${OUTPUT_DIR}"
uglifyjs ./build/elm.js \
  --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' |
  uglifyjs --mangle "toplevel,eval" --output "${OUTPUT_PATH}"

echo "Completed ${NAME}..."

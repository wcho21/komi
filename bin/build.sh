#!/usr/bin/env bash

# configuration
OUT_DIR=./dist

# internal variables
BUILD_MODE="--dev"

# script body
build() {
  wasm-pack build --no-typescript --target web ${BUILD_MODE} --out-dir ${OUT_DIR}

  if [ $? -ne 0 ]; then
    echo "Build failed"
    exit 1
  fi

  if [[ "${BUILD_MODE}" == "--dev" ]]; then
    exit 0
  fi

  # clean up
  rm ${OUT_DIR}/.gitignore ${OUT_DIR}/package.json ${OUT_DIR}/README.md
}

clean() {
  rm -rf ${OUT_DIR}
}

help() {
  echo "Usage: $0 [--dev|--release]"
}

for ARG in "$@"; do
  case $ARG in
    "clean")
      clean
      ;;
    "--release")
      BUILD_MODE="--release"
      build
      ;;
    "--dev")
      BUILD_MODE="--dev"
      build
      ;;
    *)
      echo "Unknown option: ${ARG}"
      help
      exit 1
    ;;
  esac
done
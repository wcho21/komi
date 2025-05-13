#!/usr/bin/env bash

# configuration
OUT_DIR=./dist

# internal variables
BUILD_MODE="--release"
COMMAND="build"

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
  echo "Usage: $0 [clean|help] [--dev|--release]"
}

for ARG in "$@"; do
  case $ARG in
    "clean")
      COMMAND="clean"
      ;;
    "help")
      COMMAND="help"
      ;;
    "--dev")
      BUILD_MODE="--dev"
      ;;
    "--release")
      BUILD_MODE="--release"
      ;;
    *)
      echo "Unknown option: ${ARG}"
      help
      exit 1
    ;;
  esac
done

case $COMMAND in
  "build")
    build
    ;;
  "help")
    help
    ;;
  "clean")
    clean
    ;;
  *)
    echo "Unexpected command: '${COMMAND}'"
    exit 1
esac
#!/usr/bin/env bash

echo "Info: check formatting" && cargo fmt --check && \
  echo "Info: check rust errors" && cargo check --locked && \
  echo "Info: run unit test" && cargo test --locked && \
  echo "Info: run integration test" && cd test && bun test
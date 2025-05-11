#!/usr/bin/env bash

cargo fmt --check && cargo test --locked && cargo check --locked
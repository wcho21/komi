FROM ubuntu:24.10

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC
RUN apt-get update && apt-get --yes install git vim gnupg2 curl unzip build-essential && apt-get clean

ARG RUSTUP_VERSION=1.28.2
ARG RUST_VERSION=1.86.0
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --default-toolchain ${RUST_VERSION} -y
ARG WASM_PACK_VERSION=0.13.1
RUN PATH="/root/.cargo/bin:${PATH}" cargo install wasm-pack --version ${WASM_PACK_VERSION}
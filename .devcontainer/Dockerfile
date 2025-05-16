FROM ubuntu:24.10

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC
RUN apt-get update && apt-get --yes install git vim gnupg2 curl unzip build-essential && apt-get clean

ARG RUSTUP_VERSION=1.28.2
ARG RUST_VERSION=1.86.0
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --default-toolchain ${RUST_VERSION} -y
ARG WASM_PACK_VERSION=0.13.1
ARG JUST_VERSION=1.40.0
RUN PATH="/root/.cargo/bin:${PATH}" cargo install wasm-pack@${WASM_PACK_VERSION} just@${JUST_VERSION}
ARG NVM_VERSION=0.40.3
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v${NVM_VERSION}/install.sh | bash
ARG NODE_VERSION=22.15.1
RUN /bin/bash -c "source /root/.nvm/nvm.sh && nvm install ${NODE_VERSION}"
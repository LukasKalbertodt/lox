FROM ubuntu:bionic

# Install required native applications and libraries
RUN apt update && apt install -y curl build-essential git

# Install nightly Rust compiler
RUN curl https://sh.rustup.rs -sSf | sh -s -- --default-toolchain=nightly -y
ENV PATH "/root/.cargo/bin:$PATH"

# Install `cargo-fuzz` subcommand
RUN cargo install cargo-fuzz

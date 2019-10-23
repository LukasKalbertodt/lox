FROM ubuntu:bionic

# Install required native applications and libraries
RUN apt update && apt install -y curl build-essential git

# Install nightly Rust compiler and remove unused files again.
# (TODO: once it's possible, we should rustup to not install docs in the first place)
RUN curl https://sh.rustup.rs -sSf | sh -s -- --profile minimal --default-toolchain=nightly -y \
    && cd /root/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu \
    && rm -rf share/doc \
    && cd lib/rustlib/x86_64-unknown-linux-gnu/ \
    && rm -rf lib/*.so \
    && rm -rf codegen-backends/librustc_codegen_llvm-emscripten.so

ENV PATH "/root/.cargo/bin:$PATH"

# Install `cargo-fuzz` subcommand
RUN cargo install cargo-fuzz

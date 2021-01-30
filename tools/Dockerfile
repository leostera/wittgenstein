FROM rust:1.44.1 AS builder

#
# Build OpenSSL for a MUSL deployment
#
RUN apt-get update && apt-get install musl-tools libssl-dev --yes
RUN ln -s /usr/include/x86_64-linux-gnu/asm /usr/include/x86_64-linux-musl/asm \
 && ln -s /usr/include/asm-generic /usr/include/x86_64-linux-musl/asm-generic \
 && ln -s /usr/include/linux /usr/include/x86_64-linux-musl/linux
RUN mkdir /musl
RUN wget https://github.com/openssl/openssl/archive/OpenSSL_1_1_1f.tar.gz \
 && tar zxvf OpenSSL_1_1_1f.tar.gz

RUN CC="musl-gcc -fPIE -pie" /openssl-OpenSSL_1_1_1f/Configure no-shared no-async --prefix=/musl --openssldir=/musl/ssl linux-x86_64
RUN make depend
RUN make -j$(nproc)
RUN make install

#
# Set up Rust tooclhains for cross-compilation
#

RUN rustup target add x86_64-unknown-linux-musl
RUN rustup component add rustfmt --toolchain 1.44.1-x86_64-unknown-linux-gnu

#
# Build and install project
#
WORKDIR /tools
COPY . /tools
RUN PKG_CONFIG_ALLOW_CROSS=1 \
    OPENSSL_STATIC=true \
    OPENSSL_DIR=/musl \
    cargo build --release --target x86_64-unknown-linux-musl
RUN OPENSSL_STATIC=true \
    OPENSSL_DIR=/musl \
    cargo install --target x86_64-unknown-linux-musl --path .

#
# Copy relevant binary out into new flat image
#
FROM scratch
ARG TOOL_NAME
COPY --from=builder /usr/local/cargo/bin/ .
USER 1000

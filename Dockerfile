FROM ubuntu:21.04 AS base

WORKDIR /base

RUN apt-get update
RUN apt-get install -y wget xz-utils

RUN wget https://ziglang.org/download/0.8.0/zig-linux-x86_64-0.8.0.tar.xz
RUN echo "502625d3da3ae595c5f44a809a87714320b7a40e6dff4a895b5fa7df3391d01e  zig-linux-x86_64-0.8.0.tar.xz" | sha256sum -c -
RUN tar xf zig-linux-x86_64-0.8.0.tar.xz
RUN mv zig-linux-x86_64-0.8.0 zig

COPY src/ src/
COPY test/ test/

RUN ./zig/zig build --build-file test/build.zig --prefix ./

RUN rm zig-linux-x86_64-0.8.0.tar.xz

FROM ubuntu:21.04 AS runner
WORKDIR /base

RUN apt-get update
RUN apt-get install -y wabt

COPY --from=base base/ ./

ENTRYPOINT [ "bash", "test/run.sh" ]
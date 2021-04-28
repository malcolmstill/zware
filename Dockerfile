FROM ubuntu:20.10 AS base

WORKDIR /base

RUN apt-get update
RUN apt-get install -y wget xz-utils

RUN wget https://ziglang.org/builds/zig-linux-x86_64-0.8.0-dev.2064+da9da76e3.tar.xz
RUN echo "926166f08046a582658f1e36f347ace00b4b7081dbaf0b43271da237a8cc2690  zig-linux-x86_64-0.8.0-dev.2064+da9da76e3.tar.xz" | sha256sum -c -
RUN tar xf zig-linux-x86_64-0.8.0-dev.2064+da9da76e3.tar.xz
RUN mv zig-linux-x86_64-0.8.0-dev.2064+da9da76e3 zig

COPY src/ src/
COPY test/ test/

RUN ./zig/zig build --build-file test/build.zig --prefix ./

RUN rm zig-linux-x86_64-0.8.0-dev.2064+da9da76e3.tar.xz

FROM ubuntu:20.10 AS runner
WORKDIR /base

RUN apt-get update
RUN apt-get install -y wabt

COPY --from=base base/ ./

ENTRYPOINT [ "bash", "test/run.sh" ]
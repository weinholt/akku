# -*- mode: dockerfile; coding: utf-8 -*-
FROM weinholt/chezscheme:latest AS build
RUN apt-get update && apt-get -y --no-install-recommends install \
       ca-certificates \
       curl \
       git \
       xz-utils \
  && rm -rf /var/lib/apt/lists/*

COPY . /tmp
WORKDIR /tmp
RUN set -xe; \
    test -d .git && git clean -d -d -x -f; \
    . .akku/bin/activate; \
    mkdir -p ~/.local/share/akku/keys.d; \
    cp akku-archive-*.gpg ~/.local/share/akku/keys.d; \
    akku.sps update; \
    private/build.chezscheme.sps; \
    tar -xvaf akku-*-linux.tar.xz; \
    cd akku-*-linux; \
    ./install.sh; \
    ~/.local/bin/akku

FROM weinholt/ironscheme:latest
RUN apt-get update && apt-get -y --no-install-recommends install \
       ca-certificates \
       curl \
       git \
  && rm -rf /var/lib/apt/lists/*
COPY --from=build /root/.local /root/.local
ENV PATH="/root/.local/bin:${PATH}"

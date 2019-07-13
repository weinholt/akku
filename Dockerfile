# -*- mode: dockerfile; coding: utf-8 -*-
FROM weinholt/chezscheme:alpine AS build
RUN apk add --no-cache curl git xz tar

COPY . /tmp
WORKDIR /tmp
RUN set -xe; \
    test -d .git && git clean -d -d -x -f; \
    . .akku/bin/activate; \
    mkdir -p ~/.local/share/akku/keys.d; \
    cp akku-archive-*.gpg ~/.local/share/akku/keys.d; \
    akku.sps update; \
    private/build.chezscheme.sps
RUN set -ex; \
    tar -xvaf akku-*-linux.tar.xz; \
    cd akku-*-linux; \
    ./install.sh; \
    ~/.local/bin/akku

FROM weinholt/chezscheme:alpine
RUN apk add --no-cache curl git
COPY --from=build /root/.local /root/.local
ENV PATH="/root/.local/bin:${PATH}"
RUN akku version

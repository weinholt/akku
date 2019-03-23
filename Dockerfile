# -*- mode: dockerfile; coding: utf-8 -*-
FROM weinholt/chezscheme:alpine AS build
RUN apk add --no-cache curl git xz tar

COPY . /tmp
WORKDIR /tmp
RUN set -xe; \
    test -d .git && git clean -d -d -x -f; \
    . .akku/bin/activate; \
    mkdir -p ~/.akku/share/keys.d; \
    cp akku-archive-*.gpg ~/.akku/share/keys.d; \
    akku.sps update; \
    private/build.chezscheme.sps; \
    tar -xvaf akku-*-linux.tar.xz; \
    cd akku-*-linux; \
    ./install.sh; \
    ~/bin/akku

FROM weinholt/chezscheme:alpine
RUN apk add --no-cache curl git
COPY --from=build /root/.akku /root/.akku
ENV PATH="/root/.akku/bin:${PATH}"
RUN akku version

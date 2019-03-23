# Akku.scm

[![Build Status](https://travis-ci.org/weinholt/akku.svg?branch=master)](https://travis-ci.org/weinholt/akku)
[![pipeline status](https://gitlab.com/akkuscm/akku/badges/master/pipeline.svg)](https://gitlab.com/akkuscm/akku/commits/master)

Akku.scm is a language package manager for Scheme. It grabs hold of
code and vigorously shakes it until it behaves properly.

* Separately declare your dependencies and locked versions.
* One command to install everything needed for a project.
* Project-based, installs your locked dependencies to a single library
  directory.
* Scan repositories for libraries and install them to the right paths
  for all supported Schemes.
* Resolve all Scheme files needed for compilation and scan them for
  license notices.
* Convert R7RS libraries for use with Chez Scheme and other R6RS
  Scheme implementations.

Web site: [akkuscm.org](https://akkuscm.org/).

## Dependencies

Akku.scm requires git and libcurl. It has been tested on GNU/Linux and
macOS. (Windows users can run Akku through WSL for now). Assistance in
porting is very welcome.

## Installation

There are two options:

 - Download, unpack and run the binary installer
   from [GitHub](https://github.com/weinholt/akku/releases). Pre-built
   versions are available for GNU/Linux amd64. The installation is
   completely contained to `~/.akku`.

 - Download the source bundle
   from [GitHub](https://github.com/weinholt/akku/releases) (files
   ending with `+src.tar.xz`). This version is a little slower and
   takes longer to install, but runs on more types of systems. It
   requires Chez Scheme 9.5+ or GNU Guile 2.2+.

Please remember to verify the OpenPGP signatures. The releases are
signed with [E33E61A2E9B8C3A2][key].

 [key]: https://pgp.surfnet.nl/pks/lookup?op=vindex&fingerprint=on&search=0xE33E61A2E9B8C3A2

## Usage

How to get started with a new project:

 - Run `akku init project-name`. This creates a new project from a
   template. You can also safely run akku in your existing project
   directory.
 - Run `akku list` to list available packages (`akku update` downloads
   the package index).
 - Run `akku install <pkg>` to install a named package. This also
   installs the code in your current directory into `.akku`. If you
   add local source files you'll need to rerun `akku install`.
 - Run `.akku/env` to get a shell in an environment that uses the
   programs and libraries in `.akku`.

The installed libraries and programs should now be available to you,
assuming you use one of these Schemes: Chez Scheme, GNU Guile (with
R6RS settings), Ikarus, Larceny, Mosh, Racket (plt-r6rs), Sagittarius,
Vicare or Ypsilon.

Your users can unpack your source code and run `akku install` to get
the same dependencies that you used during development.

More details are in the manpage: `man docs/akku.1`.

## Docker image

The [akkuscm/akku](https://hub.docker.com/r/akkuscm/akku) image is
automatically built in Docker hub from the repository at GitHub. It is
based on Alpine Linux and comes with a stripped down Chez Scheme that
doesn't require X or ncurses. Here's a simple way to use this image to
run tests with GitLab CI:

```yaml
image: "akkuscm/akku:latest"

build:
  before_script:
    - akku install
  script:
    - . .akku/bin/activate
    - tests/test-foo.sps
```

## License

Akku.scm is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

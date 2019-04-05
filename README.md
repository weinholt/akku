# Akku.scm

[![Build Status](https://travis-ci.org/weinholt/akku.svg?branch=master)](https://travis-ci.org/weinholt/akku)
[![pipeline status](https://gitlab.com/akkuscm/akku/badges/master/pipeline.svg)](https://gitlab.com/akkuscm/akku/commits/master)

[Akku.scm](https://akkuscm.org/) is a language package manager for
Scheme. It grabs hold of code and vigorously shakes it until it
behaves properly.

* No complicated setup to point out where libraries are; Akku finds
  them!
* Separately declare dependencies and locked versions for your
  project.
* Convert R7RS libraries for use with Chez Scheme and other R6RS
  Scheme implementations.
* Numerous R6RS [packages][packages] and as well as R7RS libraries
  mirrored from [Snow][snow].

 [packages]: https://akkuscm.org/packages/
 [snow]: http://snow-fort.org/

## Dependencies

Akku.scm requires git and libcurl. It has been tested on GNU/Linux and
macOS. (Windows users can run Akku through WSL for now). Assistance in
porting is very welcome.

## Installation

There are two options:

 - Download, unpack and run the binary installer
   from [GitLab][GitLabTags]. Pre-built versions are available for
   GNU/Linux amd64. The installation is completely contained to
   `~/.akku`.

 - Download the source bundle from [GitLab][GitLabTags] (files ending
   with `+src.tar.xz`). This version is a little slower and takes
   longer to install, but runs on more types of systems. It requires
   Chez Scheme 9.5+ or GNU Guile 2.2+.

Please remember to verify the OpenPGP signatures. The releases are
signed with [E33E61A2E9B8C3A2][key]. The releases are also mirrored on
[GitHub][GitHubReleases].

 [GitLabTags]: https://gitlab.com/akkuscm/akku/tags
 [GitHubReleases]: https://github.com/weinholt/akku/releases
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

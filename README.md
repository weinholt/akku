# Akku.scm

[![Build Status](https://travis-ci.org/weinholt/akku.svg?branch=master)](https://travis-ci.org/weinholt/akku)
[![pipeline status](https://gitlab.com/akkuscm/akku/badges/master/pipeline.svg)](https://gitlab.com/akkuscm/akku/commits/master)
[![Build Status](https://api.cirrus-ci.com/github/weinholt/akku.svg)](https://cirrus-ci.com/github/weinholt/akku)

[Akku.scm](https://akkuscm.org/) is a language package manager for
Scheme. It grabs hold of code and vigorously shakes it until it
behaves properly.

* No complicated setup to point out where libraries are; Akku finds
  them!
* Separately declare dependencies and locked versions for your
  project.
* Convert R7RS libraries for use with Chez Scheme and other R6RS
  Scheme implementations.
* Numerous R6RS [packages][packages], as well as R7RS libraries
  mirrored from [Snow][snow].

 [packages]: https://akkuscm.org/packages/
 [snow]: http://snow-fort.org/

## Dependencies

Akku requires git and libcurl. It has been tested on GNU/Linux, macOS,
FreeBSD, Cygwin, OpenBSD and MSYS2. Although it supports many Scheme
implementations, Akku itself currently requires either GNU Guile or
Chez Scheme.

## Installation

There are these options:

 - Use the release source tarball. This option uses the GNU build
   system and requires Guile 2.2 (the development package),
   pkg-config, make, git and libcurl. This works on the widest range
   of operating systems and architectures.

 - Use the pre-built version from [GitLab][GitLabTags]. Pre-built
   versions based on Chez Scheme are available for GNU/Linux amd64.
   Use the file ending with src.tar.xz for other architectures.
   This type of installation uses `~/.local`.

 - If you would like to install directly from Git, then
   see [CONTRIBUTING](CONTRIBUTING.md) for instructions. Running
   `bin/akku.sps` directly is not going to work.

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
   the latest package index).
 - Run `akku install <pkg>` to install a named package. This also
   installs the code in your current directory into `.akku`. If you
   add new source code files you'll need to rerun `akku install`.
 - Run `.akku/env` to get a shell in an environment that uses the
   programs and libraries in `.akku`.

The installed libraries should now be in the load path of Chez Scheme,
Digamma, GNU Guile (with R6RS settings), Ikarus, Larceny, Mosh, Racket
(plt-r6rs), Sagittarius, Vicare and Ypsilon. Original R7RS libraries
will be available to Chibi, Digamma, Gauche, Larceny and Sagittarius.
Any installed programs are available in your shell's path.

Most implementations can use the libraries as-is. GNU Guile requires a
small adjustment: `guile -x .guile.sls -x .sls`. You may also need to
enable R6RS syntax with `(read-enable 'r6rs-hex-escapes)`
and `(read-enable 'hungry-eol-escapes)`.

Your users can unpack your source code and run `akku install` to get
the same dependencies that you used during development.

When you have a new package you want to make available to others, you
can publish it with `akku publish`.

More details are in [akku.1](https://akkuscm.org/docs/manpage.html) manpage.

## Docker image

The [akkuscm/akku](https://hub.docker.com/r/akkuscm/akku) image is
automatically built in Docker hub from the repository at GitHub. It is
a stripped down build based on Alpine Linux. Here's a simple way to
use this image to run tests with GitLab CI:

```yaml
image: "akkuscm/akku:latest"

build:
  before_script:
    - akku install
  script:
    - .akku/env ./run-tests.sh
```

Debian-based images are available for a few Scheme implementations.
See Docker hub for a list of tags. They come prepared with a
`scheme-script` wrapper that works as described in the R6RS
non-normative appendix.

## License

Akku.scm is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

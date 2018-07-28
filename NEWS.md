# Akku.scm release notes

## Version 0.3.1

This release fixes an bug with updating cloned git repositories. When
a project was cloned using a git tag, it could then later not be
updated to a newer version that was not using a tag. A workaround was
to remove its directory under `.akku/src`.

A bug in the semver library meant that version ranges that did not
allow pre-releases could still match a pre-release.

The file parser recognizes Chez and Guile modules and the installer
also installs Guile modules.

This release adds support for Larceny. The library path is now set by
`.akku/bin/activate` and Larceny's special library file mangling works
so that SRFIs are usable again.

The Chez library path is now split into `.akku/lib` and `.akku/libobj`
(compiled objects are placed in the latter directory).

The installer no longer skips test libraries when they come from
the current project.

Some limitations of the R7RS support are no longer applicable. See
this example: https://gitlab.com/akkuscm/akku/wikis/R7RS-example

## Version 0.3.0

This release introduces support for translating R7RS `define-library`
forms to R6RS `library` forms. The libraries are made available as
R6RS libraries and work transparently with other code. The `akku-r7rs`
package must be installed to get the `(scheme *)` libraries. See the
akku(1) manpage for a list of limitations.

SRFI libraries named according to the SRFI 97 convention, i.e. `(srfi
:1 lists)`, are now installed correctly for Guile.

New in this release is support for running under GNU Guile 2.2, which
greatly increases the portability of Akku.

Source files are now scanned with an error tolerant reader. This means
that it ignores extensions to Scheme's lexical syntax and is therefore
happier to process implementation-specific source.

The environment variable `AKKU_LOG_LEVEL` can be set to get more
verbose log output. See the akku(1) manpage.

Project metadata is available in `(akku metadata)`. This library will
have the name and version of the main package, but the primary purpose
is to support `akku-r7rs` in its implementations of `cond-expand` and
`include`.

## Version 0.2.3

This release fixes the support for `include-file` from the
`wak-common` and `spells` packages, which was broken for one of the
forms.

The new `scan` command analyzes and shows the result of a repository
scan. This is useful for developers who want to know what Akku.scm is
going to do with their source files.

The `update` and `publish` commands now use https://akkuscm.org/.

This release also contains a file parser for R7RS code, preliminary
support for downloading and installing packages from tar.xz files,
improved console output and a manpage.

## Version 0.2.2

This release adds a command for publishing packages. This process
requires gpg, a gpg public/private key pair and curl. Simply tag your
git repo with a vX.Y.Z tag and run `akku publish` (nothing is sent
without your approval). Currently published packages are manually
reviewed before installation in the index, so give it a day or two.

The new `show` command displays detailed information about a package.
The `list` command now shows the synopsis of packages.

There are new commands to get rid of dependencies: `remove` and
`uninstall`. The `remove` command is the opposite of `add` and merely
manipulates the manifest. The `uninstall` command runs `remove` and
also runs `install` to get rid of files that are no longer needed.

The `install` command now scans .akku/lib for files that are no longer
part of the installation. These files are removed.

When akku runs from the home directory it will no longer scan it to
analyze the "current project". This should make it fine to run akku
from the home directory.

## Version 0.2.1

The current project is now symlinked into .akku/lib, rather than
copied. This makes development easier because it is no longer
necessary to rerun akku install after every code change.

There is a new command to update the package index: `akku update`.

## Version 0.2.0

This release comes with a dependency solver and a local package index.

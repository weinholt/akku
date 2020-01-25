# Akku.scm release notes

## Version 1.1.0

This release introduces new features to assist in packaging and using
Scheme code with system package managers (such as dpkg and rpm).

The environment activation scripts now look at the variables
`R6RS_PATH` and `R7RS_PATH`. These path variables are included at the
end of each Scheme implementation's own library search path. This
allows use of globally installed libraries, but still gives priority
to the local libraries.

The `AKKU_PROJECTS` environment variable is a list of extra projects
to install into `.akku`. Multiple directories can be separated by
semicolons. The libraries in these directories are symlinked into
`.akku`. This is an alternative to the `R[67]RS_PATH` variables
described above which is useful when code expects libraries to be
located in the `.akku/lib` directory.

*The above features are not intended for most users.* They can be used
to accidentally create dependencies on libraries that are not part of
the lockfile.

The `AKKU_SETTINGS` environment variable is a comma separated list of
settings. The `no-network` setting prevents Akku from using the
network. The `no-dependencies` setting prevents Akku from using the
dependencies in the lockfile. The only available libraries will be:
the current project's libraries, the libraries from `R[67]RS_PATH`,
the libraries from `AKKU_PROJECTS` and the implementation-specific
built-in libraries. These settings can be combined in system package
manager scripts to ensure that no unpackaged dependencies are used.

Thanks to Amirouche Boubekki and erkin for contributing to this
release.

## Version 1.0.1

This is a bug fix release.

The R6RS library installer did not correctly handle libraries that
contain non-R6RS lexical syntax, such as `#!chezscheme`. This has now
been fixed, which means that packages like thunderchez can be
installed again.

A crash triggered by some R7RS packages has been fixed.

Some log messages during package installation that had severity
*error* or *warning* have been lowered to *info* due to how common
they are.

This release adds Docker images for the upcoming Guile 3.0 release,
Chibi Scheme, Loko Scheme and IronScheme on .NET Core.

Thanks to Lassi Kortela, Francesco Montanari and Anes for contributing
to this release.

## Version 1.0.0

Akku releases are now built with autotools for improved portability.
The types of releases that use Chez Scheme are still available as the
"pre-built" tarballs.

Pre-built Akku releases are now installed to `~/.local/bin/akku` and
Akku stores its files in `~/.local` instead of `~/.akku`. If you
installed from Git then you need to migrate your installation:

```
mkdir -p ~/.local/share/akku
mv ~/.akku/share/* ~/.local/share/akku/
```

This release introduces the `compat-scan` command, which scans a
library or program for implementation compatibility. It is useful to
find which `.<impl>.sls` files are missing when porting code to a new
Scheme implementation.

The order of the lockfile (and therefore the order in which packages
are installed) is now based on the order in the manifest. Packages
which are not in the manifest are installed first, followed by the
packages in the manifest (and in the same order), followed by the
current project. This allows for better control when you want to
override some library in a package.

This release adds support for Guile's `include-from-path`.

The R7RS support is improved. The file parser recursively scans for
R7RS includes in order to find include forms in included files. The
`cond-expand` used in the R7RS -> R6RS conversion now has the features
`r6rs` and `syntax-case`.

The file extension `.sc`, as used by the package manager Raven, is now
recognized.

The `(include "filename")` form is now recognized as relative to the
file containing the form.

SPDX license expressions are now checked for validity and warnings are
printed for unknown or deprecated license identifiers.

The R6RS library name mangler now preserves the original code layout
of libraries, including whitespace and comments (such as copyright
notices).

A bug was fixed in the library to filename mapping for Sagittarius
Scheme and its list of supported SRFIs is amended.

A bug was fixed in the dependency-scan and license-scan commands that
stopped them working on GNU Guile.

A crash in the installer was fixed. Libraries that included
non-existent files caused a crash.

Mosh support was fixed and this release also provides a Docker image
prepared with Mosh and Akku.

Thanks to Graham Watt, Amirouche Boubekki, Lassi Kortela and Judah for
contributing to this release.

## Version 0.5.1

This release blocks installation of some SRFI libraries for some
Scheme implementations. SRFIs are often provided by the
implementations themselves, but when Akku installed another version of
a SRFI library the practical result was that many SRFIs stopped
working. To fix this, Akku now avoids installing a SRFI when an
implementation already provides a working version of it. This improves
compatibility with Larceny, IronScheme, Sagittarius and Ypsilon.

Akku now sets the load path for Gauche to allow loading of R7RS
libraries.

There is now support for the library-to-filename mapping of Ypsilon.

Docker images, autobuilt by Docker hub, are now provided that contain
Akku along with a Scheme implementation. Images are available with
Chez Scheme, Guile, Ikarus, IronScheme, Larceny, Racket, Sagittarius,
Vicare and Ypsilon. See the relevant [FAQ entry][FAQ] in the Akku wiki
for more information on how to use them in CI testing.

Thanks to Daniel Szmulewicz for contributing to this release.

  [FAQ]: https://gitlab.com/akkuscm/akku/wikis/FAQ

## Version 0.5.0

This release is the first to mirror R7RS libraries
from [Snow](http://snow-fort.org). They are named by their main
library name, e.g. `(chibi match)`. Note that the names need to be
quoted when used from the shell: `akku install '(chibi match)'`.

This release adds the `.akku/env` script, which is an improvement over
the old activate scripts. This new script starts a subshell, which
means it is easier to unset the environment variables that it changes.
It also exports `AKKU_ENV` which you can use to update your prompt, if
you wish, and it can print sourceable shell snippets that have the
right paths independent of which directory it runs from.

This release adds support for direct dependencies on Git repositories,
checksummed tarballs at URLs and local directories. See the wiki
for [direct dependency examples][direct-dep].

  [direct-dep]: https://gitlab.com/akkuscm/akku/wikis/Direct-dependencies

It is now possible to ignore files and directories by adding their
names to `.akkuignore` (no support for wildcards yet).

R7RS programs are translated to R6RS and installed in .akku/bin. This
was meant to work earlier, but was broken.

A number of `(scheme *)` libraries are no longer considered to be
exclusive to Chibi. They were always a part of R7RS-large and marking
them as Chibi-specific was a mistake.

A bug was fixed that caused Akku to crash when a package name was a
list of symbols.

The installer script now also checks if Chez Scheme is installed under
the names `chez`, `chezscheme` or `chez-scheme`.

Thanks to Yanying Wang, Amirouche Boubekki and Lassi Kortela for
contributing to this release.

## Version 0.4.1

This release adds an activate script for fish, the friendly
interactive shell. It works with fish 3.0.0 or later. Thanks to Luis
Osa for this contribution.

Akku is now compatible with Chez Scheme built for musl libc, an
implementation of the C standard library. Thanks to Andrew Kravchuk
for pointing out the incompatibility. As a further consequence of this
change, the docker image "akkuscm/akku" is now built for Alpine Linux
together with a slimmed down Chez Scheme.

## Version 0.4.0

This release introduces scripts that run during installation of
packages. The intended use is for compiling shared objects that are
loaded by a foreign function interface such
as [r6rs-pffi](https://akkuscm.org/packages/r6rs-pffi/). The first
user of this feature
is [linenoise](https://akkuscm.org/packages/linenoise/), a
readline-like library.

R7RS libraries are now additionally installed as `.sld` files for use
in R7RS Schemes. In addition, activating the environment now also
updates Chibi's module path. Contributions to support more Schemes are
very welcome.

A bug was fixed in the HTTP library that prevented the package index
from being updated if it grew beyond a certain size. As part of the
fix, Akku.scm now depends on libcurl rather than curl itself. Guile
needs the libcurl.so file from the -dev/-devel package.

A newer version of chez-srfi is used that provides Unicode support in
its SRFI-14 implementation. This fixes some crashes that would happen
if library names contained letters outside of the latin-1 range.

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

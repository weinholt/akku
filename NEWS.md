# Akku.scm release notes

## Version 0.2.0

This release comes with a dependency solver and a local package index.

## Version 0.2.1

The current project is now symlinked into .akku/lib, rather than
copied. This makes development easier because it is no longer
necessary to rerun akku install after every code change.

There is a new command to update the package index: `akku update`.

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

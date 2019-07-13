# Contributing

Want to discuss the project? Chat with `weinholt`
in [`#scheme`](irc://irc.freenode.org/#scheme) on Freenode or open an
issue. There's also some discussion in `#chez`.

Go to [GitHub projects][projects] (pending a move to GitLab) to see
the direction of the project and [GitLab issues][issues] for a list of
current trouble.

 [projects]: https://github.com/weinholt/akku/projects
 [issues]: https://gitlab.com/akkuscm/akku/issues

## Setting up a development environment

Akku comes with its Scheme dependencies committed to the git
repository, so you can do a checkout and a slight manual installation.
Here are instructions for Debian:

```sh
sudo apt install git chezscheme libcurl4
git clone https://gitlab.com/akkuscm/akku
cd akku
.akku/env
mkdir -p ~/.local/share/akku/keys.d
cp akku-archive-*.gpg ~/.local/share/akku/keys.d
bin/akku.sps update
bin/akku.sps install
```

There are two options for how to use Akku this way. The first is
convenient to quickly test out changes. Activate the environment in
the repository you prepared above and, instead of running `akku`, run
`akku.sps` or `bin/akku.sps`. Basically you would activate this
environment in one shell, go to the project where you want to test
something out and run `akku.sps`, and then activate that project's
environment in another shell to do development in that project.

The other option is more convenient for day-to-day use. It requires
that you build a release and install it. This will overwrite your
regular installation with a freshly built version, which will work
just like the pre-built releases. Here's how to build it:

```sh
sudo apt install xz-utils
.akku/env
private/build.chezscheme.sps
```

This produces `akku-$VERSION.$ARCH-linux.tar.xz` and
`akku-$VERSION.src.tar.xz`. These can be used to install Akku to
`~/.akku` like a normal release. Unpack either one and run the
`install.sh` script.

## Windows development

Note that none of this will work on a Windows machine, mostly because
some filenames contain colon (:) characters. This is a problem caused
by SRFI libraries being named (srfi :n name) in combination with Chez
Scheme's simple translation of these to filenames. For now please use
the [Windows Subsystem for Linux][wsl], e.g. by installing Debian from
the Windows Store.

 [wsl]: https://docs.microsoft.com/en-us/windows/wsl/install-win10

Fixes for Akku on Windows are welcome, but full support will/may
require that Chez Scheme makes a change to how it translates library
names to filenames. Ikarus Scheme used URL encoding as its fix (see
`akku/lib/library-name.scm`).

## Building a release

Releasing currently requires Chez Scheme and a working development
environment (see above). Run `private/build.chezscheme.sps` and
hopefully it will generate a binary and a source release tarball.

## Submitting patches

Fork the project on GitHub or GitLab. Please consider using `git
commit -s` when you create patches, to get an automatic sign-off in
the commit message. Please write explanatory commit messages.

When you feel comfortable with your commits, submit a pull request
through GitHub, a merge request through GitLab or format a patch and
email it. For larger changes it will be better to discuss the changes
ahead of time, either through an issue or IRC.

The
[Developer Certificate of Origin](https://developercertificate.org/)
is included here by reference, please have a look and see if you can
abide by it.

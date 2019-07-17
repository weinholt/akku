# Contributing

Want to discuss the project? Chat with `weinholt`
in [`#scheme`](irc://irc.freenode.org/#scheme) on Freenode or open an
issue. There's also some discussion in `#chez` and the small but
dedicated channel `#akku`.

Go to [GitLab issues][issues] for a list of current trouble.

 [issues]: https://gitlab.com/akkuscm/akku/issues

## Setting up a development environment / Building from Git

Akku comes with its Scheme dependencies committed to the git
repository, so you can do a checkout and a manual build. For building
the normal release tarball, use `./bootstrap` and have autoreconf,
autoconf and automake installed. You may need to adjust the
`SCHEMESCRIPT` line to get it started on your system, but then it
works mostly the same as any project that uses autotools.

If you want to build with Chez Scheme then you don't need to use
autotools. Here are instructions for Debian:

```sh
sudo apt install git chezscheme libcurl4 xz-utils
git clone https://gitlab.com/akkuscm/akku
cd akku
.akku/env
mkdir -p ~/.local/share/akku/keys.d
cp akku-archive-*.gpg ~/.local/share/akku/keys.d
bin/akku.sps update
bin/akku.sps install
private/build.chezscheme.sps
```

This produces `akku-$VERSION.$ARCH-linux.tar.xz` and
`akku-$VERSION.src.tar.xz`. These can be used to install Akku to
`~/.local` like a normal release. Unpack either one and run the
`install.sh` script.

If you want to test your changes, you can also fork the repository on
GitLab and push your changes to a branch. GitLab CI will build a
release for you.

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

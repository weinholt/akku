# Contributing

Want to discuss the project? Chat with `weinholt`
in [`#scheme`](irc://irc.freenode.org/#scheme) on Freenode or open an
issue. There's also `#akku` and some discussion in `#chez`.

Go to [GitHub projects][projects] to see the direction of the project
and [GitHub issues][issues] for a list of current trouble.

 [projects]: https://github.com/weinholt/akku/projects
 [issues]: https://github.com/weinholt/akku/issues

## Setting up a development environment

Akku comes with its dependencies committed to the git repository, so
you can simply do a checkout:

```
$ git clone https://github.com/weinholt/akku
$ cd akku
$ source .akku/bin/activate
```

Note that this probably will not work on a Windows machine, because
some filenames contain colon (:) characters. This is a problem caused
by SRFI libraries being named (srfi :n name) in combination with Chez
Scheme's simple translation of these to filenames. For now please use
the [Windows Subsystem for Linux][wsl].

 [wsl]: https://docs.microsoft.com/en-us/windows/wsl/install-win10

## Building a release

Releasing currently requires Chez Scheme and a working development
environment (see above). Run `private/build.chezscheme.sps` and
hopefully it will generate a binary and a source release tarball.

## Submitting patches

Fork the project on GitHub. Please consider using `git commit -s` when
you create patches, to get an automatic sign-off in the commit
message. Please write explanatory commit messages. When you feel
comfortable with your commits, submit a pull request through GitHub.
For larger changes it will be better to discuss the changes ahead of
time, either through a GitHub issue or IRC.

The
[Developer Certificate of Origin](https://developercertificate.org/)
is included here by reference, please have a look and see if you can
abide by it.

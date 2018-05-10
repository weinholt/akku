# Contributing

Want to discuss the project? Chat with `weinholt`
in [`#scheme`](irc://irc.freenode.org/#scheme) on Freenode or open an
issue. There's also `#akku`.

Go to [GitHub projects][projects] to see the direction of the project.

 [projects]: https://github.com/weinholt/akku/projects

## Setting up a development environment

If you already have a working Akku installation (e.g. by installing
one of the released versions) then things are simple:

```
$ git clone https://github.com/weinholt/akku
$ akku install
$ source .akku/bin/activate
```

If your development machine can't run Akku yet then the least manual
way forward is to run Akku on a working machine (e.g. a virtual
machine) and afterwards move the files. Alternatively you can read
Akku.lock and install the projects manually.

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

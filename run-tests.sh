#!/bin/bash
set -ex

export PATH=$PWD/bin:$PATH

if [ ! -f bin/akku ]; then
    # When Chez was not available to build the binary.
    ln -s akku.sps bin/akku
fi

tests/test-lockfile1.sh
tests/test-lockfile2.sh
tests/test-utils.sps

akku list
akku lock
akku update
akku lock

echo All tests passed

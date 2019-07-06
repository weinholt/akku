#!/bin/sh
set -ex

export PATH=$PWD/bin:$PATH

if [ ! -f bin/akku ]; then
    # When Chez was not available to build the binary.
    ln -s akku.sps bin/akku
fi

tests/test-lockfile1.sh
tests/test-lockfile2.sh
tests/test-init.sh
tests/test-library-name.sps
tests/test-r7rs.sps
tests/test-utils.sps

akku list
akku lock
akku update
akku lock
akku compat-scan bin/akku.sps

echo All tests passed

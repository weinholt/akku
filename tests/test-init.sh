#!/bin/bash
set -x

TESTNAME=test-init
WORKDIR=$PWD/$TESTNAME

function cleanup {
  rm -rf -- "$WORKDIR"
}
trap cleanup EXIT
cleanup

akku init "$WORKDIR"
(
    set -e
    cd "$WORKDIR"
    akku install
    . .akku/bin/activate
    $TESTNAME.sps
    tests/test-$TESTNAME.sps
)

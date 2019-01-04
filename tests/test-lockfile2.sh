#!/bin/sh
set -x

WORKDIR=$PWD/test-workdir
rm -rf "$WORKDIR"
mkdir "$WORKDIR"
tar -C "$WORKDIR" -xJf $(dirname $0)/test-project.tar.xz

cat > "$WORKDIR/Akku.lock" <<EOF
#!r6rs
(import (akku format lockfile))
(projects
 ((name "test-project-rev")
  (location (git "file://$WORKDIR/test-project"))
  (revision "e191670f7a700a8da869f297920c21ca32059faa"))
 ((name "test-project-tag")
  (location (git "file://$WORKDIR/test-project"))
  (revision "fb8f0f5bf52bc807d9b667757f133fadaca3b050")
  (tag "v0.2.0")))
EOF

(set -e;cd "$WORKDIR"; akku install)
STATUS=$?

if [ ! -f "$WORKDIR"/.akku/lib/test-project/foo.sls ]; then
    echo "The source was not checked out."
    STATUS=1
fi

if [ ! -f "$WORKDIR"/.akku/lib/test-project/bar.sls ]; then
    echo "The source was not checked out."
    STATUS=1
fi

if [ ! -f "$WORKDIR"/.akku/lib/test-project/bar/baz.sls ]; then
    echo "The source was not checked out."
    STATUS=1
fi

if [ $STATUS = 0 ]; then
    rm -rf "$WORKDIR"
else
    exit $STATUS
fi

#!/bin/sh
# Copyright © 2017-2019 Göran Weinholt <goran@weinholt.se>
# SPDX-License-Identifier: GPL-3.0-or-later

# This file is part of akku-*+src.tar.xz and installs a script wrapper
# that starts Akku.scm.

set -eu

die()
{
    echo "$0: error: $*" >&2
    exit 1
}

is_executable()
{
    which "$1" >/dev/null 2>&1
}

chez_scheme=""
for x in chez-scheme chezscheme chez scheme; do
    if is_executable "$x"; then
        chez_scheme="$x"
        break
    fi
done

PREFIX=${PREFIX:-~/.local}
bindir=$PREFIX/bin
libdir=$PREFIX/lib/akku
datadir=${XDG_DATA_HOME:-$PREFIX/share}
if [ -d "$libdir" ]; then
    rm -rf "$libdir"
    mv "$libdir" "$libdir".old
fi
mkdir -p "$bindir" "$libdir" "$datadir"
cp -a share/* "$datadir"
cp -a lib "$PREFIX"

cat > "$libdir/akku.chezscheme" << EOF
#!/bin/sh
export CHEZSCHEMELIBDIRS="$libdir/lib"
unset CHEZSCHEMELIBEXTS
exec $chez_scheme --program "$libdir/bin/akku.sps" "\$@"
EOF

cat > "$libdir/akku.guile" << EOF
#!/bin/sh
export GUILE_LOAD_PATH="$libdir/lib"
exec guile -q -l "$libdir/r6rs-guile.scm" -ds "$libdir/bin/akku.sps" "\$@"
EOF

cat > "$libdir/r6rs-guile.scm" << EOF
;; Sets R6RS options for GNU Guile.
(set! %load-extensions (append '(".guile.sls" ".sls") %load-extensions))
(read-enable 'r6rs-hex-escapes)
(read-enable 'hungry-eol-escapes)
EOF

chmod 0755 "$libdir/akku.chezscheme"
chmod 0755 "$libdir/akku.guile"

rm -f "$bindir/akku"

install_chez()
{
    export CHEZSCHEMELIBDIRS="$libdir/lib"
    unset CHEZSCHEMELIBEXTS
    $chez_scheme \
        --compile-imported-libraries \
        --program "$libdir/bin/akku.sps" 2>/dev/null
    ln -s "../lib/akku/akku.chezscheme" "$bindir/akku"
    echo You can now run "$bindir/akku"
}

install_guile()
{
    ln -s "../lib/akku/akku.guile" "$bindir/akku"
    "$bindir/akku" version >/dev/null
    echo You can now run "$bindir/akku" "(configured for GNU Guile 2.2+)"
}

if [ -n "$chez_scheme" ]; then
    install_chez
else
    install_guile
fi

if [ -d ~/.akku ]; then
    echo To remove the legacy version: rm -rf '~/.akku' '~/bin/akku'
fi

exit 0

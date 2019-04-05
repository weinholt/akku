#!/bin/sh
# Copyright © 2017-2019 Göran Weinholt <goran@weinholt.se>
# SPDX-License-Identifier: GPL-3.0-or-later

# This file is part of akku-*+src.tar.xz and installs a script wrapper
# that starts Akku.scm.

die()
{
    echo "$0: error: $*" >&2
    exit 1
}

is_executable()
{
    which "$1" >/dev/null 2>&1
}

chez_scheme_script=""
for x in chez-scheme-script scheme-script; do
    if is_executable "$x"; then
        chez_scheme_script="$x"
        break
    fi
done
chez_scheme=""
for x in chez-scheme chezscheme chez scheme; do
    if is_executable "$x"; then
        chez_scheme="$x"
        break
    fi
done
if [ -n "$chez_scheme_script" ] && [ -z "$chez_scheme" ]; then
    die "found $chez_scheme_script but not the Chez Scheme compiler"
elif [ -n "$chez_scheme" ] && [ -z "$chez_scheme_script" ]; then
    die "found $chez_scheme but not the Chez Scheme script interpreter"
fi

PREFIX=$HOME/.akku
mkdir -p "$PREFIX"
if [ -d "$PREFIX/lib" ]; then
    rm -rf "$PREFIX/lib.old"
    mv "$PREFIX/lib" "$PREFIX/lib.old"
fi
cp -a share .akku/* "$PREFIX/"
mv -f "$PREFIX/share/index.db" "$PREFIX/share/index.db.bak" 2>/dev/null

cat > "$PREFIX/bin/akku.chezscheme" << EOF
#!/bin/sh
export CHEZSCHEMELIBDIRS="$PREFIX/lib"
unset CHEZSCHEMELIBEXTS
exec $chez_scheme_script "$PREFIX/bin/akku.sps" "\$@"
EOF

cat > "$PREFIX/bin/akku.guile" << EOF
#!/bin/sh
export GUILE_LOAD_PATH="$PREFIX/lib"
exec guile -q -l "$PREFIX/share/r6rs-guile.scm" -ds "$PREFIX/bin/akku.sps" "\$@"
EOF

cat > "$PREFIX/share/r6rs-guile.scm" << EOF
;; Sets R6RS options for GNU Guile.
(set! %load-extensions (append '(".guile.sls" ".sls") %load-extensions))
(read-enable 'r6rs-hex-escapes)
(read-enable 'hungry-eol-escapes)
EOF

chmod 0755 "$PREFIX/bin/akku.chezscheme"
chmod 0755 "$PREFIX/bin/akku.guile"
mkdir -p "$HOME/bin"

rm -f "$HOME/bin/akku"

install_chez()
{
    export CHEZSCHEMELIBDIRS="$PREFIX/lib"
    unset CHEZSCHEMELIBEXTS
    $chez_scheme \
        --compile-imported-libraries \
        --program "$PREFIX/bin/akku.sps" 2>/dev/null
    ln -s "$PREFIX/bin/akku.chezscheme" "$HOME/bin/akku"
    echo You can now run '~/bin/akku'
}

install_guile()
{
    ln -s "$PREFIX/bin/akku.guile" "$HOME/bin/akku"
    "$HOME/bin/akku" version >/dev/null
    echo You can now run '~/bin/akku' "(configured for GNU Guile 2.2+)"
}

if [ -n "$chez_scheme" ]; then
    install_chez
else
    install_guile
fi

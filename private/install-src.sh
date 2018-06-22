#!/bin/sh
# Copyright © 2017-2018 Göran Weinholt <goran@weinholt.se>
# SPDX-License-Identifier: GPL-3.0-or-later

# This file is part of akku-*+src.tar.xz and installs a script wrapper
# that starts Akku.scm.

is_executable()
{
    which "$1" >/dev/null 2>&1
}

PREFIX=$HOME/.akku
mkdir -p $PREFIX
if [ -d $PREFIX/lib ]; then
    rm -rf $PREFIX/lib.old
    mv $PREFIX/lib $PREFIX/lib.old
fi
cp -a share .akku/* $PREFIX/

cat > $PREFIX/bin/akku.chezscheme << EOF
#!/bin/sh
export CHEZSCHEMELIBDIRS="$PREFIX/lib"
unset CHEZSCHEMELIBEXTS
exec scheme-script $PREFIX/bin/akku.sps \$*
EOF

cat > $PREFIX/bin/akku.guile << EOF
#!/bin/sh
export GUILE_LOAD_PATH="$PREFIX/lib"
exec guile -q -l "$PREFIX/share/r6rs-guile.scm" -ds $PREFIX/bin/akku.sps \$*
EOF

cat > $PREFIX/share/r6rs-guile.scm << EOF
;; Sets R6RS options for GNU Guile.
(set! %load-extensions (append '(".guile.sls" ".sls") %load-extensions))
(read-enable 'r6rs-hex-escapes)
(read-enable 'hungry-eol-escapes)
EOF

chmod 0755 $PREFIX/bin/akku.chezscheme
chmod 0755 $PREFIX/bin/akku.guile
mkdir -p $HOME/bin

rm -f $HOME/bin/akku
if is_executable scheme; then
    export CHEZSCHEMELIBDIRS="$PREFIX/lib"
    unset CHEZSCHEMELIBEXTS
    scheme --compile-imported-libraries --program $PREFIX/bin/akku.sps 2>/dev/null
    ln -s $PREFIX/bin/akku.chezscheme $HOME/bin/akku
    echo You can now run '~/bin/akku'
else
    ln -s $PREFIX/bin/akku.guile $HOME/bin/akku
    $HOME/bin/akku version >/dev/null
    echo You can now run '~/bin/akku' "(configured for GNU Guile 2.2+)"
fi

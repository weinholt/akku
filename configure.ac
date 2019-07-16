AC_INIT([akku], [1.0.0-alpha.0])
AC_CONFIG_SRCDIR([bin/akku.sps])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign dist-xz no-dist-gzip])
AC_PROG_MKDIR_P
AC_PATH_PROG([GIT], [git], [])
AC_PATH_PROG([CURL], [curl], [])
dnl AC_SEARCH_LIBS([curl_easy_init], [curl])

GUILE_PKG([3.0 2.2])
GUILE_PROGS
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile installation.])
fi

dnl Convert .akku/lib from an R6RS source library tree to a Guile
dnl build directory. When these libraries are packaged in generally
dnl available Guile packages then they should be filtered out here or
dnl in the installation step.
for fn in `<"$srcdir"/guile-deps.txt`; do
    case "$fn" in
    (*/akku/config.sls)
        ;;
    (*.scm)
        scm_fn=`echo "$fn" | sed 's,^\.akku/lib/,,'`
        akku_incs="$akku_incs $scm_fn"
        if test -d "$srcdir"/.akku/lib; then
            AC_CONFIG_LINKS([$scm_fn:$fn])
        fi
        ;;
    (*.sls)
        sls_fn=`echo "$fn" | sed -e 's,^\.akku/lib/,,' -e 's,\.guile\.sls,\.scm,' -e 's,\.sls,\.scm,'`
        akku_libs="$akku_libs $sls_fn"
        if test -d "$srcdir"/.akku/lib; then
            AC_CONFIG_LINKS([$sls_fn:$fn])
        fi
        ;;
    esac
done

AC_SUBST([akku_libs])
AC_SUBST([akku_incs])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([akku/config.scm:config.scm.in])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_OUTPUT

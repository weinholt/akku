# These directories are prefixed with akku/ so that the libraries we
# install as vendored dependencies are not made available with (or
# conflict with) the libraries provided by Guile. This is likely to
# happen with the SRFI and R7RS libraries.
moddir=$(datadir)/akku/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/akku/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

do_subst = sed -e 's,[@]GUILE[@],$(GUILE),g' \
	    -e 's,[@]moddir[@],$(moddir),g'  \
	    -e 's,[@]godir[@],$(godir),g'    \
	    -e 's,[@]prefix[@],$(prefix),g'  \
	    -e 's,[@]pkgdatadir[@],$(pkgdatadir),g'

akku/config.scm: $(srcdir)/config.scm.in
	@$(MKDIR_P) akku
	$(AM_V_GEN) $(do_subst) < $(srcdir)/config.scm.in > akku/config.scm
akku/lib/*.scm: akku/config.scm

GOBJECTS = $(SOURCES:%.scm=%.go) akku/config.go

nobase_dist_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_mod_DATA = akku/config.scm
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_dist_modDATA install-nobase_modDATA

CLEANFILES = $(GOBJECTS)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go

.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILD) compile $(GUILE_WARNINGS) -o $@ $<

SOURCES = $(akku_libs)
NOCOMP_SOURCES = $(akku_incs)

bin_SCRIPTS = bin/akku

bin/akku: akku/config.scm bin/akku.sps Makefile guile-script-header.in
	@$(MKDIR_P) bin
	$(AM_V_GEN)( $(do_subst) < $(srcdir)/guile-script-header.in; cat $(srcdir)/bin/akku.sps ) > bin/akku
	chmod +x bin/akku

CLEANFILES += bin/akku akku/config.scm

dist_man_MANS = docs/akku.1

keysdir = $(pkgdatadir)/keys.d
dist_keys_DATA = akku-archive-2018.gpg

dist_pkgdata_DATA = bootstrap.db
dist_doc_DATA = CREDITS.md CONTRIBUTING.md NEWS.md README.md

EXTRA_DIST = \
	config.scm.in \
	bin/akku.sps \
	guile-deps.txt \
	guile-script-header.in

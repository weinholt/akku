moddir=$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

do_subst = sed -e 's,[@]GUILE[@],$(GUILE),g' \
	    -e 's,[@]moddir[@],$(moddir),g'  \
	    -e 's,[@]godir[@],$(godir),g'

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
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILD) compile $(GUILE_WARNINGS) -o "$@" "$<"

SOURCES = $(akku_libs)
NOCOMP_SOURCES = $(akku_incs)

bin_SCRIPTS = bin/akku

bin/akku: bin/akku.sps Makefile guile-script-header.in
	@$(MKDIR_P) bin
	$(AM_V_GEN)( $(do_subst) < $(srcdir)/guile-script-header.in; cat "$<" ) > bin/akku
	chmod +x bin/akku

CLEANFILES += bin/akku

dist_man_MANS = docs/akku.1

keysdir = $(pkgdatadir)/keys.d
dist_keys_DATA = akku-archive-2018.gpg

dist_pkgdata_DATA = bootstrap.db
dist_doc_DATA = CREDITS.md CONTRIBUTING.md NEWS.md README.md

EXTRA_DIST = \
	bin/akku.sps \
	guile-deps.txt \
	guile-script-header.in

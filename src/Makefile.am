### Makefile.am
include $(top_builddir)/version.mk

AM_CFLAGS = $(EXTRA_CFLAGS)
AM_CPPFLAGS = -D_POSIX_C_SOURCE=201001L -D_XOPEN_SOURCE=700 -D_DEFAULT_SOURCE -D_BSD_SOURCE
AM_LDFLAGS = $(XCCLDFLAGS)

bin_PROGRAMS =
noinst_PROGRAMS =
lib_LTLIBRARIES =
noinst_LTLIBRARIES =
pkglib_LTLIBRARIES =
noinst_HEADERS =
include_HEADERS =
pkginclude_HEADERS =
BUILT_SOURCES =
EXTRA_DIST = $(BUILT_SOURCES)
CLEANFILES = 
SUFFIXES =

CLEANFILES += version.c
EXTRA_DIST += version.c.in

noinst_HEADERS += nifty.h

lib_LTLIBRARIES += libttl.la
libttl_la_SOURCES =
libttl_la_SOURCES += ttlrdr.c
libttl_la_SOURCES += ttlstr.c
libttl_la_SOURCES += ttldecl.c
pkginclude_HEADERS += ttl.h

bin_PROGRAMS += ttl2nt
ttl2nt_LDADD = libttl.la
ttl2nt_LDFLAGS = -static
BUILT_SOURCES += ttl2nt.yucc

bin_PROGRAMS += ttl2ttl
ttl2ttl_LDADD = libttl.la
ttl2ttl_LDFLAGS = -static
BUILT_SOURCES += ttl2ttl.yucc

bin_PROGRAMS += ttl-wc
ttl_wc_LDADD = libttl.la
ttl_wc_LDFLAGS = -static
BUILT_SOURCES += ttl-wc.yucc

bin_PROGRAMS += ttl-fold
ttl_fold_LDADD = libttl.la
ttl_fold_LDFLAGS = -static
BUILT_SOURCES += ttl-fold.yucc

bin_PROGRAMS += ttl-rm
ttl_rm_LDADD = libttl.la
ttl_rm_LDFLAGS = -static
BUILT_SOURCES += ttl-rm.yucc


## version rules
version.c: $(srcdir)/version.c.in $(top_builddir)/.version
	$(AM_V_GEN) PATH="$(top_builddir)/build-aux:$${PATH}" \
		yuck$(EXEEXT) scmver --ignore-noscm --force -o $@ \
			--use-reference --reference $(top_builddir)/.version \
			$(srcdir)/version.c.in

## yuck rule
SUFFIXES += .yuck
SUFFIXES += .yucc
.yuck.yucc:
	$(AM_V_GEN) PATH=$(top_builddir)/build-aux:"$${PATH}" \
		yuck$(EXEEXT) gen -o $@ $<

## Create preprocessor output (debugging purposes only)
.c.i:
	$(COMPILE) -E -o $@ $<

## Create assembler output (debugging purposes only)
.c.s:
	$(COMPILE) -S -c $(AM_CFLAGS) $<

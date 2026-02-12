/*** ttl-pav.c - emit pav statements
 *
 * Copyright (C) 2017-2026 Sebastian Freundt
 *
 * Author:  Sebastian Freundt <freundt@ga-group.nl>
 *
 * This file is part of libttl.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the author nor the names of any contributors
 *    may be used to endorse or promote products derived from this
 *    software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 ***/
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif	/* HAVE_CONFIG_H */
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>
#include <errno.h>
#include <time.h>
#include <sys/stat.h>
#include "ttl.h"
#include "nifty.h"

typedef size_t strhdl_t;

struct _writer_s {
	/* codec */
	ttl_codec_t *c;
	/* prefix buffer */
	ttl_decl_t *d;
	/* for diversions */
	strhdl_t stri;
};

static char mtim[] = "\"xxxx-xx-xxTxx:xx:xxZ\"^^xsd:dateTime ";
static char wclk[] = "\"xxxx-xx-xxTxx:xx:xxZ\"^^xsd:dateTime ";


static void
__attribute__((format(printf, 1, 2)))
error(const char *fmt, ...)
{
	va_list vap;
	va_start(vap, fmt);
	vfprintf(stderr, fmt, vap);
	va_end(vap);
	if (errno) {
		fputc(':', stderr);
		fputc(' ', stderr);
		fputs(strerror(errno), stderr);
	}
	fputc('\n', stderr);
	return;
}


/* ring buffer */
#define stdi		0U
#define INI_RING	64U
#define MAX_DIST	(INI_RING/2U)
static size_t ringz = INI_RING;
static uint64_t _hring[INI_RING], *hring = _hring;
static char *_sring[countof(_hring)], **sring = _sring;
static size_t _nring[countof(_hring)], *nring = _nring;
static size_t _zring[countof(_hring)], *zring = _zring;

#define FREE_RING(x)				\
	if (x != _##x) free(x)

static void
salloc(strhdl_t stri, size_t n)
{
/* make sure another N bytes could fit */
	if (UNLIKELY(nring[stri] + n >= zring[stri])) {
		size_t nu, mb = nring[stri] + n;
		for (nu = (2U * zring[stri]) ?: 256U; nu < mb; nu *= 2U);
		sring[stri] = realloc(sring[stri], nu);
		zring[stri] = nu;
	}
	return;
}

static void
sput_(int c, strhdl_t stri)
{
/* like sputc but assume enough space */
	sring[stri][nring[stri]++] = (char)c;
	return;
}

static void
sputc(int c, strhdl_t stri)
{
	salloc(stri, 1U);
	sput_(c, stri);
	return;
}

static void
swrit(const char *s, size_t n, strhdl_t stri)
{
	salloc(stri, n);
	memcpy(sring[stri] + nring[stri], s, n);
	nring[stri] += n;
	return;
}

static void
sflsh(strhdl_t stri)
{
	if (LIKELY(!stri)) {
		for (char *sp = sring[stri],
			     *const ep = sp + nring[stri]; sp < ep; sp++) {
			/* turn 0x02 into 0x0a */
			*sp |= (char)((*sp == 0x02) << 3U);
		}
		fwrite(sring[stri], 1, nring[stri], stdout);
		nring[stri] = 0U;
	}
	return;
}


static void
swrite_iri(struct _writer_s *UNUSED(w), ttl_iri_t t, strhdl_t stri)
{
	if (UNLIKELY(!t.pre.len && t.val.len == 1U && *t.val.str == 'a')) {
		sputc('a', stri);
	} else if (t.pre.str) {
		swrit(t.pre.str, t.pre.len, stri);
		sputc(':', stri);
		swrit(t.val.str, t.val.len, stri);
	} else {
		sputc('<', stri);
		swrit(t.val.str, t.val.len, stri);
		sputc('>', stri);
	}
	sflsh(stri);
	return;
}

static void
swrite_term(struct _writer_s *w, ttl_term_t t, strhdl_t stri)
{
	switch (t.typ) {
	case TTL_TYP_IRI:
		swrite_iri(w, t.iri, stri);
		break;
	default:
		break;
	}
	return;
}

static bool
irieqp(struct _writer_s *w, ttl_iri_t i1, ttl_iri_t i2)
{
	if (i1.pre.len) {
		ttl_str_t x = ttl_decl_get(w->d, i1.pre);

		if (UNLIKELY(!x.len)) {
			x = (ttl_str_t){i1.pre.str, i1.pre.len};
		}
		return x.len <= i2.val.len &&
			!memcmp(x.str, i2.val.str, x.len) &&
			x.len + i1.val.len == i2.val.len &&
			!memcmp(i1.val.str, i2.val.str + x.len, i1.val.len);
	} else if (i1.val.len > 1U || *i1.val.str != 'a') {
		return i1.val.len == i2.val.len &&
			!memcmp(i1.val.str, i2.val.str, i1.val.len);
	}
	return *i1.val.str == *i2.val.str;
}

static bool
termeqp(struct _writer_s *w, ttl_term_t t1, ttl_term_t t2)
{
	return t1.typ == t2.typ &&
		(t1.typ == TTL_TYP_IRI && irieqp(w, t1.iri, t2.iri) ||
		 t1.typ == TTL_TYP_BLA && t1.bla.h[0U] == t2.bla.h[0U]);
}


static size_t zbuf[2U];
static char *lbuf[2U];
static ttl_term_t last[1U];

static ttl_iri_t
clon_iri(struct _writer_s *w, ttl_iri_t i, unsigned int which)
{
	size_t ilen = i.val.len;
	ttl_str_t x = {.len = 0U};

	if (i.pre.len) {
		x = ttl_decl_get(w->d, i.pre);
		ilen += x.len ?: i.pre.len;
	}

	if (UNLIKELY(ilen > zbuf[which])) {
		size_t nu;
		for (nu = 2U * zbuf[which]; nu <= ilen; nu *= 2U);
		lbuf[which] = realloc(lbuf[which], countof(lbuf) * nu);
		zbuf[which] = nu;
	}
	if (x.len) {
		memcpy(lbuf[which], x.str, x.len);
	} else if (i.pre.len) {
		memcpy(lbuf[which], i.pre.str, x.len = i.pre.len);
	}
	memcpy(lbuf[which] + x.len, i.val.str, i.val.len);
	return (ttl_iri_t){{lbuf[which], ilen}};
}

static ttl_term_t
clon(struct _writer_s *w, ttl_term_t t, unsigned int which)
{
	switch (t.typ) {
	case TTL_TYP_IRI:
		return (ttl_term_t){TTL_TYP_IRI, clon_iri(w, t.iri, which)};
	case TTL_TYP_BLA:
		return t;
	default:
		break;
	}
	return (ttl_term_t){};
}


static void
decl(void *usr, ttl_iri_t decl)
{
	struct _writer_s *w = usr;

	ttl_decl_put(w->d, decl.pre, decl.val);
	if (last[TTL_SUBJ].typ && w->stri == stdi) {
		sputc('.', stdi);
		sputc('\n', stdi);
		last[TTL_SUBJ] = (ttl_term_t){};
	}
	swrit("@prefix ", 8U, stdi);
	swrit(decl.pre.str, decl.pre.len, stdi);
	sputc(':', stdi);
	sputc(' ', stdi);
	sputc('<', stdi);
	swrit(decl.val.str, decl.val.len, stdi);
	sputc('>', stdi);
	sputc(' ', stdi);
	sputc('.', stdi);
	sputc('\n', stdi);

	sflsh(stdi);
	return;
}

#if 1
static void
stmt(void *usr, const ttl_stmt_t *stmt, size_t where)
{
	static int pav_decl;
	struct _writer_s *w = usr;

	if (UNLIKELY(stmt == NULL)) {
		/* last statement */
		last[TTL_SUBJ] = (ttl_term_t){};
	} else if (!termeqp(w, stmt[where].subj, last[TTL_SUBJ])) {
		static char pavd[] = "@prefix pav: <http://purl.org/pav/> .\n";
		static char xsdd[] = "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n";
		static char impd[] = "\tpav:importedOn\t";
		static char refd[] = "\tpav:lastRefreshedOn\t";
		static char accd[] = "\tpav:sourceAccessedOn\t";
		static char lacc[] = "\tpav:sourceLastAccessedOn\t";

		if (UNLIKELY(!pav_decl)) {
			swrit(pavd, strlenof(pavd), stdi);
			swrit(xsdd, strlenof(xsdd), stdi);
			pav_decl++;
		}
		sputc('\n', stdi);
		swrite_term(w, stmt[where].subj, stdi);
		sputc('\n', stdi);
		swrit(impd, strlenof(impd), stdi);
		swrit(wclk, strlenof(wclk), stdi);
		sputc(';', stdi);
		sputc('\n', stdi);
		swrit(refd, strlenof(refd), stdi);
		swrit(wclk, strlenof(wclk), stdi);
		sputc(';', stdi);
		sputc('\n', stdi);
		swrit(accd, strlenof(accd), stdi);
		swrit(mtim, strlenof(mtim), stdi);
		sputc(';', stdi);
		sputc('\n', stdi);
		swrit(lacc, strlenof(lacc), stdi);
		swrit(mtim, strlenof(mtim), stdi);
		sputc('.', stdi);
		sputc('\n', stdi);

		/* cache him so we know next time whether we've seen him */
		last[TTL_SUBJ] = clon(w, stmt[where].subj, TTL_SUBJ);
	}
	sflsh(stdi);
	return;
}

static void
stmt_auth(void *usr, const ttl_stmt_t *stmt, size_t where)
{
	static int pav_decl;
	struct _writer_s *w = usr;

	if (UNLIKELY(stmt == NULL)) {
		/* last statement */
		last[TTL_SUBJ] = (ttl_term_t){};
	} else if (!termeqp(w, stmt[where].subj, last[TTL_SUBJ])) {
		static char pavd[] = "@prefix pav: <http://purl.org/pav/> .\n";
		static char xsdd[] = "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n";
		static char crea[] = "\tpav:createdOn\t";
		static char refd[] = "\tpav:lastRefreshedOn\t";

		if (UNLIKELY(!pav_decl)) {
			swrit(pavd, strlenof(pavd), stdi);
			swrit(xsdd, strlenof(xsdd), stdi);
			pav_decl++;
		}
		sputc('\n', stdi);
		swrite_term(w, stmt[where].subj, stdi);
		sputc('\n', stdi);
		swrit(crea, strlenof(crea), stdi);
		swrit(wclk, strlenof(wclk), stdi);
		sputc(';', stdi);
		sputc('\n', stdi);
		swrit(refd, strlenof(refd), stdi);
		swrit(wclk, strlenof(wclk), stdi);
		sputc('.', stdi);
		sputc('\n', stdi);

		/* cache him so we know next time whether we've seen him */
		last[TTL_SUBJ] = clon(w, stmt[where].subj, TTL_SUBJ);
	}
	sflsh(stdi);
	return;
}
#endif

static struct _writer_s
make_writer(void)
{
	return (struct _writer_s){.c = ttl_make_codec(), .d = ttl_make_decl()};
}

static void
free_writer(struct _writer_s w)
{
	ttl_free_codec(w.c);
	ttl_free_decl(w.d);
	return;
}


#include "ttl-pav.yucc"

int
main(int argc, char *argv[])
{
	static char buf[16U * 4096U];
	static yuck_t argi[1U];
	struct _writer_s w;
	ttl_parser_t *p = NULL;
	int rc = 0;

	if (yuck_parse(argi, argc, argv) < 0) {
		rc = 1;
		goto out;
	} else if ((p = ttl_make_parser()) == NULL) {
		error("\
Error: cannot instantiate ttl parser");
		rc = 1;
		goto out;
	}

	w = make_writer();
	if (w.c == NULL || w.d == NULL) {
		error("\
Error: cannot instantiate nt writer");
		rc = 1;
		goto out;
	}
	/* instantiate last buffer */
	for (size_t i = 0U; i < countof(lbuf); i++) {
		lbuf[i] = malloc(zbuf[i] = 128U);
		if (lbuf[i] == NULL) {
			error("\
Error: cannot instantiate buffer for previous statement");
			rc = 1;
			goto out;
		}
	}

	p->hdl = (ttl_handler_t){decl, argi->author_flag ? stmt_auth : stmt};
	p->usr = &w;
	w.stri = stdi;

	/* set wall clock time */
	with (struct tm *now) {
		time_t t = time(NULL);
		size_t z;
		now = gmtime(&t);
		z = strftime(wclk + 1U, strlenof(wclk) - 1U, "%FT%T", now);
		wclk[1U + z] = 'Z';
	}

	for (size_t i = 0U; i < argi->nargs + (!argi->nargs); i++) {
		const char *fn = argi->args[i];
		int fd;

		if (!fn) {
			fd = STDIN_FILENO;
			fn = "(stdin)";
			memcpy(mtim, wclk, strlenof(wclk));
		} else if ((fd = open(fn, O_RDONLY)) < 0) {
			error("\
Error: cannot open file `%s'", fn);
			continue;
		}
		/* look for mtime */
		with (struct tm *then) {
			struct stat fdst;
			size_t z;

			fstat(fd, &fdst);
			then = gmtime(&fdst.st_mtime);
			z = strftime(mtim + 1U, strlenof(mtim) - 1U, "%FT%T", then);
			mtim[1U + z] = 'Z';
		}
		/* otherwise read chunks thereof */
		for (ssize_t nrd; (nrd = read(fd, buf, sizeof(buf))) > 0;) {
			if (ttl_parse_chunk(p, buf, nrd) < 0) {
				errno = 0, error("\
Error: cannot parse `%s'", fn);
				break;
			}
		}
		/* give us closure */
		close(fd);
		p->hdl.stmt(&w, NULL, 0U);
	}
	errno = 0;
	for (size_t i = stdi + 1U; i < ringz; i++) {
		if (hring[i] && nring[i] >= 2U) {
			char tbuf[24U];
			int z;

			z = snprintf(tbuf, sizeof(buf), "_:b%016lx", hring[i]);
			fputc('\n', stdout);
			fwrite(tbuf, 1, z, stdout);
			fwrite(sring[i] + 1U, 1, nring[i] - 2U, stdout);
			fputc('.', stdout);
			fputc('\n', stdout);
		}
	}

	FREE_RING(hring);
	FREE_RING(sring);
	FREE_RING(zring);
	FREE_RING(nring);

out:
	ttl_free_parser(p);
	for (size_t i = 0U; i < countof(lbuf); i++) {
		free(lbuf[i]);
	}
	free_writer(w);
	yuck_free(argi);
	return rc;
}

/* ttl2ttl.c ends here */

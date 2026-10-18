/*** ttl-diff.c -- triple-wise diff of two files
 *
 * Copyright (C) 2017-2023 Sebastian Freundt
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
#include "ttl.h"
#include "nifty.h"

struct _context_s {
	/* codec */
	ttl_codec_t *c;
	/* prefix buffer */
	ttl_decl_t *d;
	/* for pulling */
	int fd;
	char *buf;
	size_t bsz;
};


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

static inline __attribute((pure, const)) size_t
min_z(size_t i1, size_t i2)
{
	return i1 < i2 ? i1 : i2;
}

static int
mem2cmp(const char *x, size_t m, const char *y, size_t n)
{
	size_t z = min_z(m, n);
#if 0
	return memcmp(x, y, z) | -(int)(z < n) | n < m;
#else
	size_t i;
	for (i = 0U; i < z && x[i] == y[i]; i++);
	return -(z && (unsigned char)x[i] < (unsigned char)y[i]) |
		(z && (unsigned char)y[i] < (unsigned char)x[i]) |
		-(i < n && i == m) | (i < m && i == n);
#endif
}


static void
fwrite_iri(struct _context_s *w, ttl_iri_t t, void *stream)
{
	if (UNLIKELY(!t.pre.len && t.val.len == 1U && *t.val.str == 'a')) {
		static const char a[] =
			"<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>";
		fwrite(a, 1, sizeof(a) - 1U, stream);
	} else if (t.pre.str) {
		ttl_str_t x = ttl_decl_get(w->d, t.pre);

		if (x.len) {
			fputc('<', stream);
			fwrite(x.str, 1, x.len, stream);
			fwrite(t.val.str, 1, t.val.len, stream);
			fputc('>', stream);
		} else {
			fwrite(t.pre.str, 1, t.pre.len, stream);
			fputc(':', stream);
			fwrite(t.val.str, 1, t.val.len, stream);
		}
	} else {
		fputc('<', stream);
		fwrite(t.val.str, 1, t.val.len, stream);
		fputc('>', stream);
	}
	return;
}

static void
fwrite_lit(struct _context_s *w, ttl_lit_t t, void *stream)
{
	size_t i = 1U;

	i += t.val.str[0 - i] == t.val.str[0 - i - 1];
	i += t.val.str[0 - i] == t.val.str[0 - i - 1];
	
	fwrite(t.val.str - i, 1, i, stream);
	fwrite(t.val.str, 1, t.val.len, stream);
	fwrite(t.val.str - i, 1, i, stream);
	if (t.typ.val.len) {
		fputc('^', stream);
		fputc('^', stream);
		fwrite_iri(w, t.typ, stream);
	}
	if (t.lng.len) {
		fputc('@', stream);
		fwrite(t.lng.str, 1, t.lng.len, stream);
	}

	ttl_codec_clear(w->c);
	return;
}

static void
fwrite_term(struct _context_s *w, ttl_term_t t, void *stream)
{
	switch (t.typ) {
	case TTL_TYP_IRI:
		fwrite_iri(w, t.iri, stream);
		break;
	case TTL_TYP_LIT:
		fwrite_lit(w, t.lit, stream);
		break;
	case TTL_TYP_BLA:
		fprintf(stream, "_:b%016lx", t.bla.h[0U]);
		break;
	default:
		break;
	}
	return;
}

static int
iricmp(struct _context_s *w, ttl_iri_t i1, ttl_iri_t i2)
{
/* return <0 on error,
 * return 0 to stop pursuing
 * return 1 to pursue file A
 * return 2 to pursue file B
 * return 3 to pursue both */
/* also note that ~memcmp(A,B) & 0x3 gives 0 2 3 for
 * A less, B less, AB same respectively */
	(void)w;
	if (i1.pre.len && i2.pre.len) {
		return mem2cmp(i1.pre.str, i1.pre.len, i2.pre.str, i2.pre.len) ?:
			mem2cmp(i1.val.str, i1.val.len, i2.val.str, i2.val.len);
	}
	return mem2cmp(i1.val.str, i1.val.len, i2.val.str, i2.val.len);
}

static int
litcmp(struct _context_s *w, ttl_lit_t l1, ttl_lit_t l2)
{
	return mem2cmp(l1.val.str, l1.val.len, l2.val.str, l2.val.len) ?:
		mem2cmp(l1.lng.str, l1.lng.len, l2.lng.str, l2.lng.len) ?:
		iricmp(w, l1.typ, l2.typ);
}

static int
termcmp(struct _context_s *w, ttl_term_t t1, ttl_term_t t2)
{
/* return <0 on error,
 * return 0 to stop pursuing
 * return 1 to pursue file A
 * return 2 to pursue file B
 * return 3 to pursue both */
	if (LIKELY(t1.typ == t2.typ)) {
		switch (t1.typ) {
		default:
		case TTL_TYP_UNK:
			return -1;
		case TTL_TYP_IRI:
			return iricmp(w, t1.iri, t2.iri);
		case TTL_TYP_LIT:
			return litcmp(w, t1.lit, t2.lit);
		case TTL_TYP_BLA:
			return 3;
		case TTL_TYP_DIR:
			return 3;
		}
	} else {
		switch (t1.typ) {
		default:
		case TTL_TYP_UNK:
			break;
		case TTL_TYP_IRI:
			/* LIT -> 2, BLA -> 1 */
			return 4 - t2.typ;
		case TTL_TYP_LIT:
			/* lits are always first */
			return 1;
		case TTL_TYP_BLA:
			/* blanks are always last */
			return 2;
		case TTL_TYP_DIR:
			return 1;
		}
	}
	return -1;
}

static ttl_iri_t
clon_iri(struct _context_s *w, ttl_iri_t i)
{
	static char *lbuf;
	static size_t zbuf;
	size_t ilen = 0U;
	ttl_str_t x = {.len = 0U};

#define free_clon_iri()	clon_iri(NULL, (ttl_iri_t){NULL})
	if (UNLIKELY(w == NULL)) {
		free(lbuf);
		return i;
	}

	if (i.pre.len) {
		x = ttl_decl_get(w->d, i.pre);
	}
	ilen += x.len;
	ilen += i.val.len;

	if (UNLIKELY(ilen > zbuf)) {
		size_t nu;
		for (nu = (2U * zbuf) ?: 64U; nu <= ilen; nu *= 2U);
		lbuf = realloc(lbuf, nu);
		zbuf = nu;
	}
	if (i.pre.len) {
		memcpy(lbuf, x.str, x.len);
	}
	memcpy(lbuf + x.len, i.val.str, i.val.len);
	return (ttl_iri_t){{lbuf, ilen}};
}


static void
diff(void *usr, const ttl_stmt_t *stmtA, size_t wA, const ttl_stmt_t *stmtB, size_t wB)
{
/* determine which stream to parse next */
	struct _context_s *w = usr;
	if (termcmp(w, stmtA[wA].subj, stmtB[wB].subj)) {
		if (termcmp(w, stmtA[wA].pred, stmtB[wB].subj)) {
		}
	}
	return;
}


static ttl_term_t lastA[3U] , lastB[3U];

static void
decl(void *usr, ttl_iri_t decl)
{
	struct _context_s *w = usr;

	ttl_decl_put(w->d, decl.pre, decl.val);
	return;
}

static void
stmtA(void *usr, const ttl_stmt_t *stmt, size_t where)
{
	struct _context_s *w = usr;

	if (stmt == NULL) {
		return;
	}
	fputc('A', stdout);
	fwrite_term(w, stmt[where].subj, stdout);
	fputc('\t', stdout);
	fwrite_term(w, stmt[where].pred, stdout);
	fputc('\t', stdout);
	fwrite_term(w, stmt[where].obj, stdout);
	if (stmt[where].grph.typ) {
		fputc('\t', stdout);
		fwrite_term(w, stmt[where].grph, stdout);
	}
	fputc('\t', stdout);
	fputc('.', stdout);
	fputc('\n', stdout);
	return;
}

static void
stmtB(void *usr, const ttl_stmt_t *stmt, size_t where)
{
	struct _context_s *w = usr;

	if (stmt == NULL) {
		return;
	}
	fputc('B', stdout);
	fwrite_term(w, stmt[where].subj, stdout);
	fputc('\t', stdout);
	fwrite_term(w, stmt[where].pred, stdout);
	fputc('\t', stdout);
	fwrite_term(w, stmt[where].obj, stdout);
	if (stmt[where].grph.typ) {
		fputc('\t', stdout);
		fwrite_term(w, stmt[where].grph, stdout);
	}
	fputc('\t', stdout);
	fputc('.', stdout);
	fputc('\n', stdout);
	return;
}

static ssize_t
pull(void *usr, char *const*buf, size_t *bsz)
{
	struct _context_s *p = usr;
	return read(p->fd, *buf, *bsz);
}

static struct _context_s
make_context(void)
{
	return (struct _context_s){.c = ttl_make_codec(), .d = ttl_make_decl()};
}

static void
free_context(struct _context_s w)
{
	ttl_free_codec(w.c);
	ttl_free_decl(w.d);
	return;
}


#include "ttl-diff.yucc"

int
main(int argc, char *argv[])
{
	static char bufA[16U * 4096U], bufB[16U * 4096U];
	static yuck_t argi[1U];
	struct _context_s wA, wB;
	ttl_parser_t *pA = NULL, *pB = NULL;
	const char *fnA, *fnB;
	int fdA = -1, fdB = -1;
	int rc = 0;

#if 0
	printf("%d\n",
	       termcmp(NULL,
		       (ttl_term_t){TTL_TYP_IRI,.iri={.val = {"https://rdf.gleif.org/L1/YZ", 28U}}},
		       (ttl_term_t){TTL_TYP_IRI,.iri={.val = {"https://rdf.gleif.org/L1/XYZ", 28U}}}));
	return 0;

	printf("%d\n", mem2cmp("B",1U,"BA",2U));
	printf("%d\n", mem2cmp("BA",2U,"BA",2U));
	printf("%d\n", mem2cmp("BA",2U,"B",1U));
	printf("%d\n", mem2cmp("AA",2U,"BA",2U));
	printf("%d\n", mem2cmp("AA",2U,"AA",2U));
	printf("%d\n", mem2cmp("BA",2U,"AA",2U));
	printf("%d\n", mem2cmp("A",1U,"BA",2U));
	printf("%d\n", mem2cmp("AA",1U,"BA",2U));
	printf("%d\n", mem2cmp("BA",2U,"A",1U));
	return 0;
#endif

	if (yuck_parse(argi, argc, argv) < 0) {
		rc = 1;
		goto out;
	} else if (argi->nargs != 2U) {
		error("\
Error: need exactly two files to compare");
		rc = 1;
		goto out;
	} else if ((pA = ttl_make_parser()) == NULL) {
		error("\
Error: cannot instantiate ttl parser");
		rc = 1;
		goto out;
	} else if ((pB = ttl_make_parser()) == NULL) {
		error("\
Error: cannot instantiate ttl parser");
		rc = 1;
		goto out;
	} else if ((fdA = open(fnA = argi->args[0U], O_RDONLY)) < 0) {
		error("\
Error: cannot open file `%s'", fnA);
		rc = 1;
		goto out;
	} else if ((fdB = open(fnB = argi->args[1U], O_RDONLY)) < 0) {
		error("\
Error: cannot open file `%s'", fnB);
		rc = 1;
		goto out;
	}

	wA = wB = make_context();
	if (wA.c == NULL || wA.d == NULL) {
		error("\
Error: cannot instantiate context");
		rc = 1;
		goto out;
	}

	pA->hdl = (ttl_handler_t){decl, NULL, pull};
	pB->hdl = (ttl_handler_t){decl, NULL, pull};
	pA->usr = &wA;
	pB->usr = &wB;

	/* assign context for pulling */
	wA.fd = fdA;
	wA.buf = bufA;
	wA.bsz = sizeof(bufA);
	/* and more even */
	wB.fd = fdB;
	wB.buf = bufB;
	wB.bsz = sizeof(bufB);

	fprintf(stderr, "%d\n", ttl_parse_chunk(pA, NULL, 0));
	fprintf(stderr, "%d\n", ttl_parse_chunk(pB, NULL, 0));
#if 0
	/* otherwise read chunks thereof */
	for (ssize_t nrdA, nrdB;
	     (nrdA = read(fdA, bufA, sizeof(bufA))) > 0 &&
		     (nrdB = read(fdB, bufB, sizeof(bufB))) > 0;) {
		if (ttl_parse_chunk(pA, bufA, nrdA) < 0) {
			errno = 0, error("\
Error: cannot parse file A, `%s'", fnA);
			break;
		}
		if (ttl_parse_chunk(pB, bufB, nrdB) < 0) {
			errno = 0, error("\
Error: cannot parse file B, `%s'", fnB);
			break;
		}
	}
	for (ssize_t nrdA;
	     (nrdA = read(fdA, bufA, sizeof(bufA))) > 0;) {
		if (ttl_parse_chunk(pA, bufA, nrdA) < 0) {
			errno = 0, error("\
Error: cannot parse file A, `%s'", fnA);
			break;
		}
	}
	for (ssize_t nrdB;
	     (nrdB = read(fdB, bufB, sizeof(bufB))) > 0;) {
		if (ttl_parse_chunk(pB, bufB, nrdB) < 0) {
			errno = 0, error("\
Error: cannot parse file B, `%s'", fnB);
			break;
		}
	}
#endif
//	stmtA(&wA, NULL, 0U);
//	stmtB(&wB, NULL, 0U);

out:
	free_clon_iri();
	if (fdA >= 0) {
		close(fdA);
	}
	if (fdB >= 0) {
		close(fdB);
	}
	ttl_free_parser(pA);
	ttl_free_parser(pB);
	free_context(wA);
	yuck_free(argi);
	return rc;
}

/* ttl-diff.c ends here */

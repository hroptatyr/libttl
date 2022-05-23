/*** ttl2ttl.c - trig/turtle/ntriples/nquads reader
 *
 * Copyright (C) 2017-2018 Sebastian Freundt
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

static unsigned int iri_xpnd = 1U;
static unsigned int iri_xgen = 1U;
static unsigned int sortable = 0U;

typedef size_t strhdl_t;

struct _writer_s {
	/* codec */
	ttl_codec_t *c;
	/* prefix buffer */
	ttl_decl_t *d;
	/* for diversions */
	strhdl_t stri;
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

static uint64_t
strtoux64(const char *str, size_t *len)
{
/* silly version of strtoull(., ., 16) with no range checks */
	const size_t e = len ? *len : -1U;
	unsigned char c;
	uint64_t x;
	size_t i;

#define ctox(x)	(unsigned char)((x | ' ') % 39U - 9U)
	for (i = 0U, x = 0U; i < e && (c = ctox(str[i])) < 16U; i++) {
		x *= 16U;
		x += c;
	}
	if (len != NULL) {
		*len = i;
	}
	return x;
}


/* ring buffer */
#define stdi		0U
#define INI_RING	64U
static size_t ringz = INI_RING;
static uint64_t _hring[INI_RING], *hring = _hring;
static char *_sring[countof(_hring)], **sring = _sring;
static size_t _nring[countof(_hring)], *nring = _nring;
static size_t _zring[countof(_hring)], *zring = _zring;

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
		fwrite(sring[stri], 1, nring[stri], stdout);
		nring[stri] = 0U;
	}
	return;
}

static strhdl_t
ring_get(uint64_t h)
{
/* use h == 0 to obtain an empty slot */
	for (size_t i = stdi+1U; i < ringz; i++) {
		if (hring[i] == h) {
			return i;
		}
	}
	return (strhdl_t)-1;
}

static strhdl_t
ring_put(uint64_t h)
{
	for (size_t i = stdi+1U; i < ringz; i++) {
		if (!hring[i]) {
			hring[i] = h;
			return i;
		}
	}
	/* big disaster */
	ringz *= 2U;
#define REALLOC_RING(x)						\
	if (x != _##x) {					\
		x = realloc(x, ringz * sizeof(*x));		\
	} else {						\
		x = malloc(ringz * sizeof(*x));			\
		memcpy(x, _##x, ringz * sizeof(*x));		\
	}							\
	memset(x + ringz / 2U, 0, ringz / 2U * sizeof(*x))
#define FREE_RING(x)				\
	if (x != _##x) free(x)
	REALLOC_RING(hring);
	REALLOC_RING(sring);
	REALLOC_RING(zring);
	REALLOC_RING(nring);
	/* slot at ringz/2U must be free */
	hring[ringz / 2U] = h;
	return ringz / 2U;
}

static void
ring_rem(strhdl_t k)
{
	hring[k] = 0U;
	nring[k] = 0U;
	return;
}


static void
swrite_iri(struct _writer_s *w, ttl_iri_t t, strhdl_t stri)
{
	if (UNLIKELY(!t.pre.len && t.val.len == 1U && *t.val.str == 'a')) {
		sputc('a', stri);
	} else if (t.pre.str) {
		ttl_str_t x;

		if (UNLIKELY(iri_xpnd) &&
		    LIKELY((x = ttl_decl_get(w->d, t.pre)).len)) {
			sputc('<', stri);
			swrit(x.str, x.len, stri);
			swrit(t.val.str, t.val.len, stri);
			sputc('>', stri);
		} else if (UNLIKELY(iri_xgen) &&
			   LIKELY((x = ttl_decl_get(w->d, t.pre)).len) &&
			   t.pre.str[0U] == 'n' && t.pre.str[1U] == 's' &&
			   t.pre.str[2U] >= '1' && t.pre.str[2U] <= '9') {
			sputc('<', stri);
			swrit(x.str, x.len, stri);
			swrit(t.val.str, t.val.len, stri);
			sputc('>', stri);
		} else {
			swrit(t.pre.str, t.pre.len, stri);
			sputc(':', stri);
			swrit(t.val.str, t.val.len, stri);
		}
	} else {
		sputc('<', stri);
		swrit(t.val.str, t.val.len, stri);
		sputc('>', stri);
	}
	sflsh(stri);
	return;
}

static void
swrite_lit(struct _writer_s *w, ttl_lit_t t, strhdl_t stri)
{
	size_t i = 1U;

	/* count quotedness */
	i -= t.val.str[0 - i] <= ' ';
	i += t.val.str[0 - i] == t.val.str[0 - i - 1];
	i += t.val.str[0 - i] == t.val.str[0 - i - 1];

	if (UNLIKELY(sortable)) {
		t.val = ttl_enquot_str(w->c, t.val, TTL_QUOT_PRNT ^ TTL_QUOT_CTRL);
	} else if (i >= 3U) {
		t.val = ttl_dequot_str(w->c, t.val, TTL_QUOT_PRNT ^ TTL_QUOT_CTRL);
	}

	swrit(t.val.str - i, i, stri);
	swrit(t.val.str, t.val.len, stri);
	swrit(t.val.str - i, i, stri);
	if (t.typ.val.len) {
		sputc('^', stri);
		sputc('^', stri);
		swrite_iri(w, t.typ, stri);
	}
	if (t.lng.len) {
		sputc('@', stri);
		swrit(t.lng.str, t.lng.len, stri);
	}

	ttl_codec_clear(w->c);
	sflsh(stri);
	return;
}

static void
swrite_bla(struct _writer_s *UNUSED(w), ttl_bla_t t, strhdl_t stri)
{
	char buf[24U];
	int z;

	z = snprintf(buf, sizeof(buf), " _:b%016lx", -t.h[0U]);
	swrit(buf, z, stri);
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
	case TTL_TYP_LIT:
		swrite_lit(w, t.lit, stri);
		break;
	case TTL_TYP_BLA:
		swrite_bla(w, t.bla, stri);
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

static ttl_term_t
try_cast(ttl_term_t t)
{
/* take IRI or BLA and try a cast to BLA */
	switch (t.typ) {
	case TTL_TYP_IRI:
		if (t.iri.pre.len != 1U) {
			;
		} else if (*t.iri.pre.str != '_') {
			;
		} else if (UNLIKELY(!t.iri.val.len)) {
			;
		} else {
			/* looking good */
			const char *val = t.iri.val.str;
			size_t len = t.iri.val.len;
			uint64_t u;

			/* skip over potential blank node prefix */
			val += (*t.iri.val.str == 'b' || *t.iri.val.str == 'B');
			val += (*t.iri.val.str == 'n' || *t.iri.val.str == 'N');
			len -= val - t.iri.val.str;
			if (UNLIKELY(!(u = strtoux64(val, &len)))) {
				;
			} else if (t.iri.val.str + t.iri.val.len != val + len) {
				;
			} else {
				/* we've done it */
				return (ttl_term_t){TTL_TYP_BLA, .bla = {u}};
			}
		}
	default:
		break;
	}
	return t;
}


static size_t zbuf[2U];
static char *lbuf[2U];
static ttl_term_t last[3U];

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

static ttl_term_t
cbla(ttl_term_t t, size_t where)
{
	return (ttl_term_t){TTL_TYP_BLA, .bla = {t.bla.h[0U], where}};
}


static void
decl(void *usr, ttl_iri_t decl)
{
	struct _writer_s *w = usr;

	ttl_decl_put(w->d, decl.pre, decl.val);
	if (!iri_xpnd) {
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
	}
	sflsh(stdi);
	return;
}

#if 1
static void
stmt(void *usr, const ttl_stmt_t *stmt, size_t where)
{
	struct _writer_s *w = usr;

	if (UNLIKELY(stmt == NULL)) {
		/* last statement */
		if (last[TTL_SUBJ].typ) {
			sputc('.', stdi);
			sputc('\n', stdi);
		}
		last[TTL_SUBJ] = (ttl_term_t){};
	} else if (!termeqp(w, stmt[where].subj, last[TTL_SUBJ])) {
		if (last[TTL_SUBJ].typ) {
			sputc('.', stdi);
			sputc('\n', stdi);
		}
		sputc('\n', stdi);
		swrite_term(w, stmt[where].subj, stdi);
		sputc('\n', stdi);
		sputc('\t', stdi);
		swrite_term(w, stmt[where].pred, stdi);
		sputc('\t', stdi);
		swrite_term(w, stmt[where].obj, stdi);
		sputc(' ', stdi);
		last[TTL_SUBJ] = clon(w, stmt[where].subj, TTL_SUBJ);
		last[TTL_PRED] = (ttl_term_t){};
	} else if (!termeqp(w, stmt[where].pred, last[TTL_PRED])) {
		sputc(';', stdi);
		sputc('\n', stdi);
		sputc('\t', stdi);
		swrite_term(w, stmt[where].pred, stdi);
		sputc('\t', stdi);
		swrite_term(w, stmt[where].obj, stdi);
		sputc(' ', stdi);
		last[TTL_PRED] = clon(w, stmt[where].pred, TTL_PRED);
	} else {
		sputc(',', stdi);
		sputc(' ', stdi);
		swrite_term(w, stmt[where].obj, stdi);
		sputc(' ', stdi);
	}
	sflsh(stdi);
	return;
}
#endif

static void
stmt_x(void *usr, const ttl_stmt_t *stmt, size_t where)
{
	struct _writer_s *w = usr;
	static size_t j;

	if (UNLIKELY(j > where)) {
		for (; j > where; j--) {
			sputc(';', stdi);
			sputc('\n', stdi);
			for (size_t i = 0U; i < j; i++) {
				sputc('\t', stdi);
			}
			sputc(']', stdi);
			sputc(' ', stdi);
		}
		last[TTL_SUBJ] = clon(w, stmt[j].subj, TTL_SUBJ);
		last[TTL_PRED] = clon(w, stmt[j].pred, TTL_PRED);
		last[TTL_OBJ] = (ttl_term_t){};
		if (LIKELY(stmt != NULL)) {
			goto flush;
		}
	}
	if (UNLIKELY(stmt == NULL)) {
		/* last statement */
		if (last[TTL_SUBJ].typ) {
			sputc('.', stdi);
			sputc('\n', stdi);
		}
		last[TTL_SUBJ] = (ttl_term_t){};
	} else if (!termeqp(w, stmt[j].subj, last[TTL_SUBJ])) {
		if (last[TTL_SUBJ].typ) {
			sputc('.', stdi);
			sputc('\n', stdi);
		}
		sputc('\n', stdi);
		swrite_term(w, stmt[j].subj, stdi);
		sputc('\n', stdi);
		sputc('\t', stdi);
		for (size_t i = 0U; i < j; i++) {
			sputc('\t', stdi);
		}
		swrite_term(w, stmt[j].pred, stdi);
		sputc('\t', stdi);
		if (LIKELY(stmt[j].obj.typ != TTL_TYP_BLA)) {
			swrite_term(w, stmt[j].obj, stdi);
			sputc(' ', stdi);
		} else {
			goto blank;
		}
		last[TTL_SUBJ] = clon(w, stmt[j].subj, TTL_SUBJ);
		last[TTL_PRED] = (ttl_term_t){};
	} else if (!termeqp(w, stmt[j].pred, last[TTL_PRED])) {
		sputc(';', stdi);
		sputc('\n', stdi);
		sputc('\t', stdi);
		for (size_t i = 0U; i < j; i++) {
			sputc('\t', stdi);
		}
		swrite_term(w, stmt[j].pred, stdi);
		sputc('\t', stdi);
		if (LIKELY(stmt[j].obj.typ != TTL_TYP_BLA)) {
			swrite_term(w, stmt[j].obj, stdi);
			sputc(' ', stdi);
		} else {
			goto blank;
		}
		last[TTL_PRED] = clon(w, stmt[j].pred, TTL_PRED);
		last[TTL_OBJ] = cbla(stmt[j].obj, where);
	} else {
		sputc(',', stdi);
		sputc(' ', stdi);
		if (LIKELY(stmt[j].obj.typ != TTL_TYP_BLA)) {
			swrite_term(w, stmt[j].obj, stdi);
			sputc(' ', stdi);
		} else {
			goto blank;
		}
	}
	sflsh(stdi);
	return;
blank:
	sputc('[', stdi);
	sputc('\n', stdi);
	last[TTL_OBJ] = cbla(stmt[j].obj, where);
	/* ascend */
	j++;
	sputc('\t', stdi);
	for (size_t i = 0U; i < j; i++) {
		sputc('\t', stdi);
	}
	swrite_term(w, stmt[j].pred, stdi);
	sputc('\t', stdi);
	if (LIKELY(stmt[j].obj.typ != TTL_TYP_BLA)) {
		swrite_term(w, stmt[j].obj, stdi);
		sputc(' ', stdi);
	} else {
		goto blank;
	}
	last[TTL_SUBJ] = clon(w, stmt[j].subj, TTL_SUBJ);
	last[TTL_PRED] = clon(w, stmt[j].pred, TTL_PRED);
	last[TTL_OBJ] = cbla(stmt[j].obj, where);
flush:
	sflsh(stdi);
	return;
}

static void
swrite_term_or_ring(struct _writer_s *w, ttl_term_t t, strhdl_t stri)
{
	ttl_term_t s;

	switch ((s = try_cast(t)).typ) {
		strhdl_t strk;

	case TTL_TYP_BLA:
		if ((strk = ring_get(s.bla.h[0U])) != (strhdl_t)-1) {
			sputc('\n', strk);
			sputc(']', strk);
			salloc(stri, 2U*nring[strk]);
			for (const char *sp = sring[strk],
				     *const ep = sp + nring[strk]; sp < ep; sp++) {
				sput_(*sp, stri);
				if (*sp == '\n') {
					sput_('\t', stri);
				}
			}
			ring_rem(strk);
			break;
		}
	default:
		swrite_term(w, t, stri);
		break;
	}
	sflsh(stri);
	return;
}

static void
stmt_y(void *usr, const ttl_stmt_t *stmt, size_t where)
{
/* we're in bufferer mode */
	struct _writer_s *w = usr;

	if (UNLIKELY(stmt == NULL)) {
		/* last statement */
		if (last[TTL_SUBJ].typ) {
			sputc('.', w->stri);
			sputc('\n', w->stri);
		}
		last[TTL_SUBJ] = (ttl_term_t){};
	} else if (!termeqp(w, stmt[where].subj, last[TTL_SUBJ])) {
		ttl_term_t s;

		if (last[TTL_SUBJ].typ && w->stri == stdi) {
			sputc('.', w->stri);
			sputc('\n', w->stri);
		} else if (last[TTL_SUBJ].typ) {
			/* undivert */
			sputc(';', w->stri);
			w->stri = stdi;
		}

		if ((s = try_cast(stmt[where].subj)).typ == TTL_TYP_BLA) {
			/* divert */
			if ((w->stri = ring_get(s.bla.h[0U])) == (strhdl_t)-1) {
				w->stri = ring_put(s.bla.h[0U]);
				sputc('[', w->stri);
			}
		} else {
			sputc('\n', w->stri);
			swrite_term(w, stmt[where].subj, w->stri);
		}
		sputc('\n', w->stri);
		sputc('\t', w->stri);
		swrite_term(w, stmt[where].pred, w->stri);
		sputc('\t', w->stri);
		swrite_term_or_ring(w, stmt[where].obj, w->stri);
		sputc(' ', w->stri);
		last[TTL_SUBJ] = clon(w, stmt[where].subj, TTL_SUBJ);
		last[TTL_PRED] = (ttl_term_t){};
	} else if (!termeqp(w, stmt[where].pred, last[TTL_PRED])) {
		sputc(';', w->stri);
		sputc('\n', w->stri);
		sputc('\t', w->stri);
		swrite_term(w, stmt[where].pred, w->stri);
		sputc('\t', w->stri);
		swrite_term_or_ring(w, stmt[where].obj, w->stri);
		sputc(' ', w->stri);
		last[TTL_PRED] = clon(w, stmt[where].pred, TTL_PRED);
	} else {
		sputc(',', w->stri);
		sputc(' ', w->stri);
		swrite_term_or_ring(w, stmt[where].obj, w->stri);
		sputc(' ', w->stri);
	}
	sflsh(stdi);
	return;
}

static void
stnt(void *usr, const ttl_stmt_t *stmt, size_t where)
{
	struct _writer_s *w = usr;

	if (UNLIKELY(stmt == NULL)) {
		return;
	}

	if (last[TTL_SUBJ].typ) {
		sputc('.', stdi);
		sputc('\n', stdi);
	}
	swrite_term(w, try_cast(stmt[where].subj), stdi);
	sputc('\t', stdi);
	swrite_term(w, stmt[where].pred, stdi);
	sputc('\t', stdi);
	swrite_term(w, try_cast(stmt[where].obj), stdi);
	sputc(' ', stdi);
	sputc('.', stdi);
	sputc('\n', stdi);
	sflsh(stdi);
	return;
}

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


#include "ttl2ttl.yucc"

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

	iri_xpnd = argi->expand_flag;
	iri_xgen = argi->expand_generic_flag;
	sortable = argi->sortable_flag;

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

	p->hdl = (ttl_handler_t){
		decl, !sortable
		? !argi->square_blanks_flag
		? argi->no_frills_flag
		? stmt
		: stmt_x
		: stmt_y
		: stnt};
	p->usr = &w;
	w.stri = stdi;

	for (size_t i = 0U; i < argi->nargs + (!argi->nargs); i++) {
		const char *fn = argi->args[i];
		int fd;

		if (!fn) {
			fd = STDIN_FILENO;
			fn = "(stdin)";
		} else if ((fd = open(fn, O_RDONLY)) < 0) {
			error("\
Error: cannot open file `%s'", fn);
			continue;
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
		if (hring[i]) {
			error("Warning: uncalled diverted stream %zu (_:b%016lx)", i, hring[i]);
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

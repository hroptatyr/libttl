/*** ttl-fold.c -- reify rdf
 *
 * Copyright (C) 2017-2018 Sebastian Freundt
 *
 * Author:  Sebastian Freundt <freundt@ga-group.nl>
 *
 * This file is part of rdfsnips.
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
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <stdarg.h>
#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include "ttl.h"
#include "nifty.h"

struct _world_s {
	/* codec */
	ttl_codec_t *c;
	/* prefix buffer */
	ttl_decl_t *d;
	/* filter prefixes */
	ttl_decl_t *f;
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

static inline size_t
memncpy(void *restrict tgt, const void *src, size_t len)
{
	memcpy(tgt, src, len);
	return len;
}

static void*
recalloc(void *oldp, size_t oldz, size_t newz, size_t nmemb)
{
	char *p = realloc(oldp, newz * nmemb);
	if (LIKELY(newz >= oldz)) {
		memset(p + oldz * nmemb, 0, (newz - oldz) * sizeof(nmemb));
	}
	return p;
}

static inline __attribute__((pure, const)) int
alnump(char c)
{
	return (unsigned char)(c ^ '0') < 10 ||
		c >= 'A' && c <= 'Z' ||
		c >= 'a' && c <= 'z';
}


/* murmur3 */
#define HASHSIZE	(128U / 8U)

static inline __attribute__((pure, const)) uint64_t
rotl64(uint64_t x, int8_t r)
{
	return (x << r) | (x >> (64 - r));
}

static inline __attribute__((pure, const, always_inline)) uint64_t
fmix64(uint64_t k)
{
	k ^= k >> 33;
	k *= 0xff51afd7ed558ccdULL;
	k ^= k >> 33;
	k *= 0xc4ceb9fe1a85ec53ULL;
	k ^= k >> 33;

	return k;
}

static void
MurmurHash3_x64_128(const void *key, size_t len, uint8_t out[static HASHSIZE])
{
	const uint8_t *data = (const uint8_t*)key;
	const size_t nblocks = len / 16U;

	uint64_t h1 = 0U, h2 = 0U;

	const uint64_t c1 = 0x87c37b91114253d5ULL;
	const uint64_t c2 = 0x4cf5ad432745937fULL;

	//----------
	// body

	const uint64_t * blocks = (const uint64_t *)(data);

	for (size_t i = 0; i < nblocks; i++) {
		uint64_t k1 = blocks[i*2+0];
		uint64_t k2 = blocks[i*2+1];

		k1 *= c1; k1  = rotl64(k1,31); k1 *= c2; h1 ^= k1;

		h1 = rotl64(h1,27); h1 += h2; h1 = h1*5+0x52dce729;

		k2 *= c2; k2  = rotl64(k2,33); k2 *= c1; h2 ^= k2;

		h2 = rotl64(h2,31); h2 += h1; h2 = h2*5+0x38495ab5;
	}

	//----------
	// tail

	const uint8_t * tail = (const uint8_t*)(data + nblocks*16);

	uint64_t k1 = 0;
	uint64_t k2 = 0;

	switch(len & 15)
	{
	case 15: k2 ^= ((uint64_t)tail[14]) << 48;
	case 14: k2 ^= ((uint64_t)tail[13]) << 40;
	case 13: k2 ^= ((uint64_t)tail[12]) << 32;
	case 12: k2 ^= ((uint64_t)tail[11]) << 24;
	case 11: k2 ^= ((uint64_t)tail[10]) << 16;
	case 10: k2 ^= ((uint64_t)tail[ 9]) << 8;
	case  9: k2 ^= ((uint64_t)tail[ 8]) << 0;
		k2 *= c2; k2  = rotl64(k2,33); k2 *= c1; h2 ^= k2;

	case  8: k1 ^= ((uint64_t)tail[ 7]) << 56;
	case  7: k1 ^= ((uint64_t)tail[ 6]) << 48;
	case  6: k1 ^= ((uint64_t)tail[ 5]) << 40;
	case  5: k1 ^= ((uint64_t)tail[ 4]) << 32;
	case  4: k1 ^= ((uint64_t)tail[ 3]) << 24;
	case  3: k1 ^= ((uint64_t)tail[ 2]) << 16;
	case  2: k1 ^= ((uint64_t)tail[ 1]) << 8;
	case  1: k1 ^= ((uint64_t)tail[ 0]) << 0;
		k1 *= c1; k1  = rotl64(k1,31); k1 *= c2; h1 ^= k1;
	};

	//----------
	// finalization

	h1 ^= len; h2 ^= len;

	h1 += h2;
	h2 += h1;

	h1 = fmix64(h1);
	h2 = fmix64(h2);

	h1 += h2;
	h2 += h1;

	((uint64_t*)out)[0] = h1;
	((uint64_t*)out)[1] = h2;
}

static inline __attribute__((pure, const)) unsigned char
c2h(unsigned int c)
{
	return (unsigned char)((c < 10U) ? (c ^ '0') : (c + 'W'));
}

static size_t
mmh3(unsigned char *restrict tgt, size_t tsz, const char *str, size_t len)
{
	uint8_t h[HASHSIZE];
	MurmurHash3_x64_128(str, len, h);
	/* print hash */
	for (size_t i = 0U; i < countof(h) && 2U * i + 1U < tsz; i++) {
		tgt[2U * i + 0U] = c2h((h[i] >> 0U) & 0b1111U);
		tgt[2U * i + 1U] = c2h((h[i] >> 4U) & 0b1111U);
	}
	return 2U * HASHSIZE;
}

static size_t
memchrz(const char *s, int c, size_t beg, size_t end)
{
/* like memchr but operate on offsets */
	const char *t = memchr(s + beg, c, end - beg);
	return t ? t - s + 1U : end;
}

static char*
xmemmem_(const char *hay, const size_t hayz, const char *ndl, const size_t ndlz)
{
/* looks for @NDL in HAY */
	const char *const eoh = hay + hayz;
	const char *const eon = ndl + ndlz;
	const char *hp;
	const char *np;
	const char *cand;
	unsigned int hsum;
	unsigned int nsum;
	unsigned int eqp;

	/* trivial checks first
         * a 0-sized needle is defined to be found anywhere in haystack
         * then run strchr() to find a candidate in HAYSTACK (i.e. a portion
         * that happens to begin with *NEEDLE) */
	if (ndlz == 0UL) {
		return deconst(hay);
	} else if ((hay = memchr(hay, '@', hayz)) == NULL) {
		/* trivial */
		return NULL;
	}

	/* First characters of haystack and needle are the same now. Both are
	 * guaranteed to be at least one character long.  Now computes the sum
	 * of characters values of needle together with the sum of the first
	 * needle_len characters of haystack. */
	for (hp = hay + 1U, np = ndl, hsum = *hay, nsum = *hay, eqp = 1U;
	     hp < eoh && np < eon;
	     hsum ^= *hp, nsum ^= *np, eqp &= *hp == *np, hp++, np++);

	/* HP now references the (NZ + 1)-th character. */
	if (np < eon) {
		/* haystack is smaller than needle, :O */
		return NULL;
	} else if (eqp) {
		/* found a match */
		return deconst(hay);
	}

	/* now loop through the rest of haystack,
	 * updating the sum iteratively */
	for (cand = hay; hp < eoh; hp++) {
		hsum ^= *cand++;
		hsum ^= *hp;

		/* Since the sum of the characters is already known to be
		 * equal at that point, it is enough to check just NZ - 1
		 * characters for equality,
		 * also CAND is by design < HP, so no need for range checks */
		if (hsum == nsum && memcmp(cand + 1U, ndl, ndlz - 1U) == 0) {
			return deconst(cand);
		}
	}
	return NULL;
}


static char *sbuf;
static size_t sbsz;

static void
sbuf_sbrk(size_t n)
{
	if (UNLIKELY(sbsz <= n)) {
		sbsz += n;
		sbsz >>= 11U;
		sbsz++;
		sbsz <<= 12U;
		/* and that should last for a while */
		sbuf = realloc(sbuf, sbsz);
	}
	return;
}

static size_t
_swrite_iri(size_t bix, const ttl_decl_t *d, ttl_iri_t t)
{
	size_t n = 0U;

	if (t.pre.len) {
		ttl_str_t x = ttl_decl_get(d, t.pre);

		if (UNLIKELY(!x.len)) {
			errno = 0, error("\
Warning: prefix `%.*s' undefined", (int)t.pre.len, t.pre.str);
			return 0U;
		}
		sbuf_sbrk(bix + x.len + t.val.len + 2U);
		sbuf[bix + n++] = '<';
		n += memncpy(sbuf + bix + n, x.str, x.len);
		n += memncpy(sbuf + bix + n, t.val.str, t.val.len);
		sbuf[bix + n++] = '>';
	} else if (t.val.len > 1U || *t.val.str != 'a') {
		sbuf_sbrk(bix + t.val.len + 2U);
		sbuf[bix + n++] = '<';
		n += memncpy(sbuf + bix + n, t.val.str, t.val.len);
		sbuf[bix + n++] = '>';
	} else {
		static const char a[] = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>";
		sbuf_sbrk(bix + strlenof(a));
		n += memncpy(sbuf + bix + n, a, strlenof(a));
	}
	return n;
}

static size_t
_swrite_lit(size_t bix, const struct _world_s *w, ttl_lit_t t)
{
	size_t n = 0U;

	t.val = ttl_dequot_str(w->c, t.val, TTL_QUOT_UTF8);
	t.val = ttl_enquot_str(w->c, t.val, TTL_QUOT_PRNT ^ TTL_QUOT_CTRL);

	sbuf_sbrk(bix + t.val.len + 2U + t.typ.val.len + 2U + t.lng.len + 2U);

	sbuf[bix + n++] = '"';
	n += memncpy(sbuf + bix + n, t.val.str, t.val.len);
	sbuf[bix + n++] = '"';
	if (t.typ.val.len) {
		sbuf[bix + n++] = '^';
		sbuf[bix + n++] = '^';
		n += _swrite_iri(bix + n, w->d, t.typ);
	}
	if (t.lng.len) {
		sbuf[bix + n++] = '@';
		n += memncpy(sbuf + bix + n, t.lng.str, t.lng.len);
	}

	ttl_codec_clear(w->c);
	return n;
}

static size_t
_swrite_bla(size_t bix, ttl_bla_t t)
{
	size_t n = 0U;

	sbuf_sbrk(bix + 19U);
	n += snprintf(sbuf + bix, 20U, "_:b%016lx", t.h[0U]);
	return n;
}

static size_t
_swrite_term(size_t bix, const struct _world_s *w, ttl_term_t t)
{
	size_t n;

	switch (t.typ) {
	case TTL_TYP_IRI:
		n = _swrite_iri(bix, w->d, t.iri);
		break;
	case TTL_TYP_LIT:
		n = _swrite_lit(bix, w, t.lit);
		break;
	case TTL_TYP_BLA:
		n = _swrite_bla(bix, t.bla);
		break;
	default:
		n = 0U;
		break;
	}
	return n;
}


static size_t
swrite_term(char **tgt, const struct _world_s *w, ttl_term_t t)
{
	size_t n = _swrite_term(0, w, t);
	*tgt = sbuf;
	return n;
}

static int
fwrite_term(const struct _world_s *w, ttl_term_t t, FILE *stream)
{
	char *s;
	size_t n = swrite_term(&s, w, t);
	return fwrite(s, 1, n, stream);
}

static size_t
swrite_stmt(char **tgt, const struct _world_s *w, const ttl_term_t s[static 3U])
{
	size_t n, m;

	n = 0U;
	goto subj;
subj:
	for (m = _swrite_term(n, w, s[TTL_SUBJ]); !m;) {
		return 0U;
	}
	n += m, sbuf[n++] = ' ';
	goto pred;

pred:
	for (m = _swrite_term(n, w, s[TTL_PRED]); !m;) {
		return 0U;
	}
	n += m, sbuf[n++] = ' ';
	goto obj;

obj:
	for (m = _swrite_term(n, w, s[TTL_OBJ]); !m;) {
		return 0U;
	}
	n += m, sbuf[n++] = ' ';
	goto fin;

fin:
	sbuf_sbrk(n + 1U);
	sbuf[n++] = '.';
	*tgt = sbuf;
	return n;
}

static ttl_str_t
ttl_rwrite_iri(ttl_iri_t iri, ttl_decl_t *d)
{
	size_t n = _swrite_iri(0, d, iri);
	return (ttl_str_t){sbuf, n};
}

static ttl_str_t
ttl_repack(ttl_term_t t)
{
/* return raw buffer coordinates of T and its surroundings, no copying */
	const char *s;
	size_t n;

	switch (t.typ) {
	case TTL_TYP_IRI:
		n = t.iri.pre.len + 1U;
		s = t.iri.val.str - n;
		n += t.iri.val.len + !t.iri.pre.len;
		break;
	case TTL_TYP_LIT:
		n = t.lit.val.len + 2U;
		s = t.lit.val.str - 1U;
		/* deal with triple quotes */
		with (unsigned int more = s[n] == s[0U]) {
			n += 2U * more, s -= more;
		}
		with (unsigned int more = s[n] == s[0U]) {
			n += 2U * more, s -= more;
		}
		with (ttl_iri_t typ = t.lit.typ) {
			/* count ^^ */
			size_t m = !!typ.val.len * 2U;
			m += typ.pre.len + 1U;
			m += typ.val.len + !typ.pre.len;
			/* mask in case there's no type */
			n += m & -!!typ.val.len;
		}
		n += t.lit.lng.len + !!t.lit.lng.len;
		break;
	default:
		s = NULL;
		n = 0UL;
		break;
	}
	return (ttl_str_t){s, n};
}


static char *tbuf;
static size_t tbsz;
static size_t *terms;
static size_t nterms;
static size_t zterms;
/* for every term in terms we track a number pred/obj pairs in beefs */
static char *bbuf;
static size_t bbsz;
static size_t bbix;
static size_t **beefs;
static size_t *nbeefs;

static void
free_terms(void)
{
	free(tbuf);
	free(terms);
	for (size_t i = 0U; i < nterms; i++) {
		free(beefs[i]);
	}
	free(beefs);
	free(nbeefs);
	free(bbuf);
	return;
}

static size_t
find_term(ttl_str_t x)
{
	size_t i;
	for (i = 0U; i < nterms; i++) {
		const size_t m = terms[i + 1U] - terms[i + 0U];
		if (m == x.len && !memcmp(tbuf + terms[i], x.str, x.len)) {
			break;
		}
	}
	return i;
}

static size_t
add_term(ttl_str_t x)
{
	for (size_t i = 0U; i < nterms; i++) {
		const size_t m = terms[i + 1U] - terms[i + 0U];
		if (m == x.len && !memcmp(tbuf + terms[i], x.str, x.len)) {
			return i;
		}
	}
	if (UNLIKELY(nterms >= zterms)) {
		const size_t oldzt = zterms;
		zterms = (zterms * 2U) ?: 64U;
		terms = realloc(terms, zterms * sizeof(*terms));
		beefs = recalloc(beefs, oldzt, zterms, sizeof(*beefs));
		nbeefs = recalloc(nbeefs, oldzt, zterms, sizeof(*nbeefs));
	}
	if (UNLIKELY(!tbsz || terms[nterms] + x.len >= tbsz)) {
		tbsz = (tbsz * 2U) ?: 1024U;
		tbuf = realloc(tbuf, tbsz * sizeof(*tbuf));
	}
	memcpy(tbuf + terms[nterms], x.str, x.len);
	terms[nterms + 1U] = terms[nterms] + x.len;
	return nterms++;
}

static void
add_beef(size_t termidx, ttl_str_t x)
{
	const size_t i = termidx;
	if (UNLIKELY(!(nbeefs[i] & (nbeefs[i] + 2U)))) {
		/* resize */
		const size_t nuz = (nbeefs[i] + 2U) * 2U;
		beefs[i] = realloc(beefs[i], nuz * sizeof(*beefs[i]));
	}
	if (UNLIKELY(bbix + x.len >= bbsz)) {
		bbsz = (bbsz * 2U) ?: 1024U;
		bbuf = realloc(bbuf, bbsz * sizeof(*bbuf));
	}
	memcpy(bbuf + bbix, x.str, x.len);
	beefs[i][nbeefs[i]++] = bbix;
	beefs[i][nbeefs[i]++] = bbix += x.len;
}

static void
fdecl(void *usr, ttl_iri_t decl)
{
	struct _world_s *w = usr;

	ttl_decl_put(w->f, decl.pre, decl.val);
	return;
}

static void
flts(void *usr, const ttl_term_t stmt[static 4U])
{
	struct _world_s *w = usr;

	if (UNLIKELY(stmt[TTL_SUBJ].typ != TTL_TYP_IRI)) {
		return;
	}

	with (ttl_str_t x = ttl_rwrite_iri(stmt[TTL_SUBJ].iri, w->f)) {
		const size_t i = add_term(x);

		add_beef(i, ttl_repack(stmt[TTL_PRED]));
		add_beef(i, ttl_repack(stmt[TTL_OBJ]));
	}
	return;
}


/* our push parser */
static void
decl(void *usr, ttl_iri_t decl)
{
	struct _world_s *w = usr;

	ttl_decl_put(w->d, decl.pre, decl.val);
#if 0
	for (size_t i = 0U; i < nterms; i++) {
		for (size_t j = 0U; j + 3U < nbeefs[i]; j += 4U) {
			const char *hays = bbuf + beefs[i][j + 2U];
			const size_t hayz = beefs[i][j + 3U] - beefs[i][j + 2U];
			if (xmemmem_(hays, hayz, decl.pre.str, decl.pre.len)) {
				printf("got `%.*s' in `%.*s'\n", (int)hayz, hays, (int)decl.pre.len, decl.pre.str);
			}
		}
	}
#endif
	return;
}

static void
stmt(void *usr, const ttl_term_t stmt[static 4U])
{
#define PRFX	"http://data.ga-group.nl/meta/mmh3/"
	static unsigned char prfx[80U] = PRFX;
	struct _world_s *w = usr;
	size_t prfn = strlenof(PRFX);
	size_t termidx;
	size_t n;
	char *s;

	if (UNLIKELY(stmt[TTL_PRED].typ != TTL_TYP_IRI)) {
		/* huh? */
		return;
	} else if (!nterms) {
		goto yep;
	}
	/* otherwise try and find him */
	with (ttl_str_t x = ttl_rwrite_iri(stmt[TTL_PRED].iri, w->d)) {
		if ((termidx = find_term(x)) >= nterms) {
			/* not found */
			return;
		}
	}

yep:
	if (UNLIKELY(!prfx[prfn])) {
		fputs("\
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n", stdout);

		for (ttl_decl_iter_t i = 0U; (i = ttl_decl_iter_next(w->f, i));) {
			ttl_iri_t p = ttl_decl_iter_get(w->f, i);

			fputs("@prefix ", stdout);
			fputs(p.pre.str, stdout);
			fputs(": <", stdout);
			fwrite(p.val.str, 1, p.val.len, stdout);
			fputs("> .\n", stdout);
		}
		fputc('\n', stdout);
	}
	n = swrite_stmt(&s, w, stmt);
	prfn += mmh3(prfx + prfn, sizeof(prfx) - prfn, s, n);
	fputc('<', stdout);
	fwrite(prfx, 1, prfn, stdout);
	fputc('>', stdout);
	fputc('\n', stdout);

	fputs("\trdf:subject\t", stdout);
	fwrite_term(w, stmt[TTL_SUBJ], stdout);
	fputs(" ;\n", stdout);

	fputs("\trdf:predicate\t", stdout);
	fwrite_term(w, stmt[TTL_PRED], stdout);
	fputs(" ;\n", stdout);

	fputs("\trdf:object\t", stdout);
	fwrite_term(w, stmt[TTL_OBJ], stdout);
	fputs(" ;\n", stdout);

	if (nterms) {
		for (size_t j = 0U; j + 3U < nbeefs[termidx]; j += 4U) {
			const size_t pbeg = beefs[termidx][j + 0U];
			const size_t pend = beefs[termidx][j + 1U];
			size_t obeg = beefs[termidx][j + 2U];
			size_t oend = beefs[termidx][j + 3U];

			fputc('\t', stdout);
			fwrite(bbuf + pbeg, 1, pend - pbeg, stdout);
			fputc('\t', stdout);
			for (size_t o;
			     (o = memchrz(bbuf, '@', obeg, oend)) < oend;
			     obeg = o) {
				fwrite(bbuf + obeg, 1, o - 1U - obeg, stdout);
				/* find replacement in D decl table */
				size_t qend;
				for (qend = o;
				     qend < oend && alnump(bbuf[qend]); qend++);
				with (ttl_str_t pre = {bbuf + o, qend - o}) {
					ttl_str_t r = ttl_decl_get(w->d, pre);

					if (UNLIKELY(!r.len)) {
						pre.str--, pre.len++;
						r = pre;
					}
					fwrite(r.str, 1, r.len, stdout);
				}
				o = qend;
			}
			fwrite(bbuf + obeg, 1, oend - obeg, stdout);
			fputs(" ;\n", stdout);
		}
	}

	fputs("\ta\trdf:Statement .\n\n", stdout);
	return;
}

static struct _world_s
make_world(void)
{
	return (struct _world_s){
		.c = ttl_make_codec(),
		.d = ttl_make_decl(),
		.f = ttl_make_decl(),
	};
}

static void
free_world(struct _world_s w)
{
	ttl_free_codec(w.c);
	ttl_free_decl(w.d);
	ttl_free_decl(w.f);
	/* also kill sbuf here */
	free(sbuf);
	return;
}


#include "ttl-fold.yucc"

int
main(int argc, char *argv[])
{
	static char buf[16U * 4096U];
	static yuck_t argi[1U];
	ttl_parser_t *p = NULL;
	struct _world_s w;
	int rc = 0;

	if (yuck_parse(argi, argc, argv) < 0) {
		rc = 1;
		goto out;
	} else if ((p = ttl_make_parser()) == NULL) {
	parser_error:
		error("\
Error: cannot set up parser");
		rc = 1;
		goto out;
	}

	w = make_world();
	if (w.c == NULL || w.d == NULL || w.f == NULL) {
		error("\
Error: cannot instantiate ttl world");
		rc = 1;
		goto out;
	}

	if (argi->nargs) {
		/* set up filter parser */
		ttl_parser_t *f;
		int fd;

		if (UNLIKELY((fd = open(*argi->args, O_RDONLY)) < 0)) {
			error("\
Error: cannot open filter file `%s'", *argi->args);
			rc = 1;
			goto out;
		} else if (UNLIKELY((f = ttl_make_parser()) == NULL)) {
			close(fd);
			goto parser_error;
		}

		f->hdl = (ttl_handler_t){fdecl, flts};
		f->usr = &w;

		for (ssize_t nrd; (nrd = read(fd, buf, sizeof(buf))) > 0;) {
			if (UNLIKELY(ttl_parse_chunk(f, buf, nrd) < 0)) {
				errno = 0, error("\
Error: cannot parse filter file `%s'", *argi->args);
				rc = 1;
				break;
			}
		}

		ttl_free_parser(f);
		close(fd);
	}

	/* interim check */
	if (UNLIKELY(rc)) {
		goto brk;
	}

	p->hdl = (ttl_handler_t){decl, stmt};
	p->usr = &w;

	for (ssize_t nrd; (nrd = read(STDIN_FILENO, buf, sizeof(buf))) > 0;) {
		if (UNLIKELY(ttl_parse_chunk(p, buf, nrd) < 0)) {
			errno = 0, error("\
Error: cannot parse input file `(stdin)'");
			rc = 1;
			break;
		}
	}

brk:
	close(STDIN_FILENO);
	free_terms();
out:
	ttl_free_parser(p);
	free_world(w);
	yuck_free(argi);
	return rc;
}

/* ttl-fold.c ends here */

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
#include "ttl.h"
#include "nifty.h"

struct _world_s {
	/* codec */
	ttl_codec_t *c;
	/* prefix buffer */
	ttl_decl_t *d;
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

static char*
xmemmem(const char *hay, const size_t hayz, const char *ndl, const size_t ndlz)
{
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
	} else if ((hay = memchr(hay, *ndl, hayz)) == NULL) {
		/* trivial */
		return NULL;
	}

	/* First characters of haystack and needle are the same now. Both are
	 * guaranteed to be at least one character long.  Now computes the sum
	 * of characters values of needle together with the sum of the first
	 * needle_len characters of haystack. */
	for (hp = hay + 1U, np = ndl + 1U, hsum = *hay, nsum = *hay, eqp = 1U;
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
		if (hsum == nsum && memcmp(cand, ndl, ndlz - 1U) == 0) {
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
swrite_iri(size_t bix, const struct _world_s *w, ttl_iri_t t)
{
	size_t n = 0U;

	if (t.pre.len) {
		ttl_str_t x = ttl_decl_get(w->d, t.pre);

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
swrite_lit(size_t bix, const struct _world_s *w, ttl_lit_t t)
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
		n += swrite_iri(bix + n, w, t.typ);
	}
	if (t.lng.len) {
		sbuf[bix + n++] = '@';
		n += memncpy(sbuf + bix + n, t.lng.str, t.lng.len);
	}

	ttl_codec_clear(w->c);
	return n;
}

static size_t
swrite_bla(size_t bix, const struct _world_s *UNUSED(w), ttl_bla_t t)
{
	size_t n = 0U;

	sbuf_sbrk(bix + 19U);
	n += snprintf(sbuf + bix, 20U, "_:b%016lx", t.h[0U]);
	return n;
}

static size_t
swrite_term(size_t bix, const struct _world_s *w, ttl_term_t t)
{
	size_t n;

	switch (t.typ) {
	case TTL_TYP_IRI:
		n = swrite_iri(bix, w, t.iri);
		break;
	case TTL_TYP_LIT:
		n = swrite_lit(bix, w, t.lit);
		break;
	case TTL_TYP_BLA:
		n = swrite_bla(bix, w, t.bla);
		break;
	default:
		n = 0U;
		break;
	}
	return n;
}

static int
fwrite_term(const struct _world_s *w, ttl_term_t t, FILE *stream)
{
	size_t n = swrite_term(0U, w, t);
	return fwrite(sbuf, 1, n, stream);
}

static size_t
swrite_stmt(char **tgt, const struct _world_s *w, const ttl_term_t s[static 3U])
{
	size_t n, m;

	n = 0U;
	goto subj;
subj:
	for (m = swrite_term(n, w, s[TTL_SUBJ]); !m;) {
		return 0U;
	}
	n += m, sbuf[n++] = ' ';
	goto pred;

pred:
	for (m = swrite_term(n, w, s[TTL_PRED]); !m;) {
		return 0U;
	}
	n += m, sbuf[n++] = ' ';
	goto obj;

obj:
	for (m = swrite_term(n, w, s[TTL_OBJ]); !m;) {
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


/* replacement of "@PREFIX" and <@PREFIX> */
#if 0
static unsigned char *rplc;
static size_t nrplc;
static size_t zrplc;

static size_t **cord;
static size_t zcord;
static size_t *ncord;
static size_t *sufxs;
static raptor_term ***cake;

static void
free_rplc(void)
{
	for (size_t i = 0U; i < zcord; i++) {
		free(cord[i]);
	}
	for (size_t i = 0U; i < zcord; i++) {
		for (size_t j = 0U; j < ncord[i]; j++) {
			raptor_free_term(cake[i][j]);
		}
		free(cake[i]);
	}
	free(cord);
	free(ncord);
	free(cake);
	free(sufxs);

	free(rplc);
	return;
}

static size_t
find_rplc(const unsigned char *str, size_t len)
{
	const char *p;
	size_t r;

	if (!(p = xmemmem((const char*)rplc, nrplc, (const char*)str, len))) {
		return -1ULL;
	} else if (p[-1] != '@' || p[len]) {
		return -1ULL;
	}
	r = ((const unsigned char*)p - rplc);
	return r / sizeof(r);
}

static size_t
add_rplc(const unsigned char *str, size_t len)
{
/* register prefix STR (sans the @) of size LEN
 * we align the strings on a sizeof(size_t) boundary */
	size_t r = nrplc;

	if (UNLIKELY(nrplc + len + sizeof(r) >= zrplc)) {
		while ((zrplc *= 2U) < nrplc + len + sizeof(r));
		rplc = realloc(rplc, zrplc);
	}
	rplc[nrplc++] = '@';
	memcpy(rplc + nrplc, str, len);
	nrplc += len;
	/* fast forward to next boundary */
	memset(rplc + nrplc, 0, sizeof(r) - (nrplc % sizeof(r)));
	nrplc += sizeof(r) - (nrplc % sizeof(r));
	return r / sizeof(r);
}

static void
add_cord(const size_t r, size_t i, size_t j, raptor_term *t, size_t sufx)
{
	if (UNLIKELY(r >= zcord)) {
		const size_t nuz = (zcord * 2U) ?: 64U;
		cord = recalloc(cord, zcord, nuz, sizeof(*cord));
		cake = recalloc(cake, zcord, nuz, sizeof(*cake));
		ncord = recalloc(ncord, zcord, nuz, sizeof(*ncord));
		sufxs = recalloc(sufxs, zcord, nuz, sizeof(*sufxs));
		zcord = nuz;
	}
	if (UNLIKELY(!(ncord[r] & (ncord[r] + 2U)))) {
		const size_t nuz = (ncord[r] + 2U) * 2U;
		cord[r] = realloc(cord[r], nuz * sizeof(*cord[r]));
		cake[r] = realloc(cake[r], nuz * sizeof(*cake[r]));
	}
	cake[r][ncord[r] / 2U] = raptor_term_copy(t);
	cord[r][ncord[r]++] = i;
	cord[r][ncord[r]++] = j;
	sufxs[r] = (sufx > sufxs[r]) ? sufx : sufxs[r];
	return;	
}
#endif


#if 0
static raptor_world *world;
static raptor_uri *base;
static raptor_term *rdfsub, *rdfpred, *rdfobj;
static raptor_term *type;
static raptor_term *stmt;
static raptor_uri *rdf;

static raptor_term **terms;
static size_t nterms;
static size_t zterms;
static raptor_term ***beefs;
static size_t *nbeefs;

static void
free_terms(void)
{
	for (size_t i = 0U; i < nterms; i++) {
		for (size_t j = 0U; j < nbeefs[i]; j++) {
			raptor_free_term(beefs[i][j]);
		}
		free(beefs[i]);
	}
	free(beefs);
	free(nbeefs);

	for (size_t i = 0U; i < nterms; i++) {
		raptor_free_term(terms[i]);
	}
	free(terms);
	return;
}

static void
nscp(void *user_data, raptor_namespace *ns)
{
	raptor_serializer *UNUSED(sfold) = user_data;
	unsigned char *uristr;
	size_t urilen;
	const unsigned char *prestr;
	size_t prelen;
	size_t r;

	prestr = raptor_namespace_get_counted_prefix(ns, &prelen);
	if (!~(r = find_rplc(prestr, prelen))) {
		return;
	}
	with (raptor_uri *uri = raptor_namespace_get_uri(ns)) {
		uristr = raptor_uri_as_counted_string(uri, &urilen);
	}
	/* a service for the suffixing later on */
	unsigned char tmp[urilen + sufxs[r]];
	if (sufxs[r]) {
		memcpy(tmp, uristr, urilen);
		uristr = tmp;
	}
	for (size_t k = 0U; k < ncord[r]; k += 2U) {
		/* replace terms */
		const size_t i = cord[r][k + 0U];
		const size_t j = cord[r][k + 1U];
		const raptor_term *proto = cake[r][k / 2U];
		size_t tmplen;

		switch (proto->type) {
			const unsigned char *prostr;
			size_t prolen;
			raptor_term *t;

		case RAPTOR_TERM_TYPE_URI:
			prostr = raptor_uri_as_counted_string(
				proto->value.uri, &prolen);
			goto sufxchck;

		case RAPTOR_TERM_TYPE_LITERAL:
			prostr = proto->value.literal.string;
			prolen = proto->value.literal.string_len;
			goto sufxchck;

		sufxchck:
			tmplen = urilen;
			if (prelen < prolen && prostr[prelen + 1U] == ':') {
				memcpy(uristr + urilen,
				       prostr + (prelen + 2U),
				       prolen - (prelen + 2U));
				tmplen += prolen - (prelen + 2U);
			}
			switch (proto->type) {
			case RAPTOR_TERM_TYPE_URI:
				t = raptor_new_term_from_counted_uri_string(
					world, uristr, tmplen);
				break;
			case RAPTOR_TERM_TYPE_LITERAL:
				t = raptor_new_term_from_counted_literal(
					world, uristr, tmplen,
					proto->value.literal.datatype,
					proto->value.literal.language,
					proto->value.literal.language_len);
				break;
			}
			/* actually do the replacing now */
			raptor_free_term(beefs[i][j]);
			beefs[i][j] = t;
		default:
			break;
		}
	}
	return;
}

static void
prnt(void *user_data, raptor_statement *triple)
{
#define PRFX	"http://data.ga-group.nl/meta/mmh3/"
	static unsigned char prfx[80U] = "http://data.ga-group.nl/meta/mmh3/";
	size_t prfn = strlenof(PRFX);
	struct ctx_s *ctx = user_data;
	raptor_world *w = triple->world;
	size_t i;

	if (!nterms) {
		goto yep;
	}
	for (i = 0U; i < nterms; i++) {
		/* have we got him? */
		if (raptor_term_equals(terms[i], triple->predicate)) {
			goto yep;
		}
	}
	/* not found */
	return;

yep:
	raptor_serializer_serialize_statement(ctx->shash, triple);
	prfn = strlenof(PRFX);
	prfn += _hash(prfx + prfn, sizeof(prfx) - prfn, ctx->b);

	raptor_term *H = raptor_new_term_from_counted_uri_string(w, prfx, prfn);

	raptor_serializer_serialize_statement(
		ctx->sfold, &(raptor_statement){w, .subject = H, type, stmt});
	raptor_serializer_serialize_statement(
		ctx->sfold, &(raptor_statement){
			w, .subject = H, rdfsub, triple->subject});
	raptor_serializer_serialize_statement(
		ctx->sfold, &(raptor_statement){
			w, .subject = H, rdfpred, triple->predicate});
	raptor_serializer_serialize_statement(
		ctx->sfold, &(raptor_statement){
			w, .subject = H, rdfobj, triple->object});

	if (nterms) {
		for (size_t j = 0U; j < nbeefs[i]; j += 2U) {
			if (UNLIKELY(!beefs[i][j + 1U]->type)) {
				continue;
			}
			raptor_serializer_serialize_statement(
				ctx->sfold, &(raptor_statement){
					w, .subject = H,
						beefs[i][j + 0U],
						beefs[i][j + 1U]});
		}
	}

	raptor_serializer_flush(ctx->sfold);
	raptor_free_term(H);
	return;
}

static void
flts(void *UNUSED(user_data), raptor_statement *triple)
{
	size_t i;

	for (i = 0U; i < nterms; i++) {
		/* have we got him? */
		if (raptor_term_equals(terms[i], triple->subject)) {
			goto yep;
		}
	}
	/* otherwise add */
	if (UNLIKELY(nterms >= zterms)) {
		zterms *= 2U;
		terms = realloc(terms, zterms * sizeof(*terms));
		beefs = recalloc(beefs, zterms / 2U, zterms, sizeof(*beefs));
		nbeefs = recalloc(nbeefs, zterms / 2U, zterms, sizeof(*nbeefs));
	}
	terms[i] = raptor_term_copy(triple->subject);
	nterms++;

yep:
	/* bang po to beefs */
	if (UNLIKELY(!(nbeefs[i] & (nbeefs[i] + 2U)))) {
		/* resize */
		const size_t nuz = (nbeefs[i] + 2U) * 2U;
		beefs[i] = realloc(beefs[i], nuz * sizeof(*beefs[i]));
	}
	beefs[i][nbeefs[i]++] = raptor_term_copy(triple->predicate);
	beefs[i][nbeefs[i]++] = raptor_term_copy(triple->object);

	/* see if triple->object contains @PREFIX semantics */
	switch (triple->object->type) {
		const unsigned char *str;
		const unsigned char *eos;
		size_t len;
		size_t r;

	case RAPTOR_TERM_TYPE_URI:
		str = raptor_uri_as_counted_string(
			triple->object->value.uri, &len);
		goto chck;
	case RAPTOR_TERM_TYPE_LITERAL:
		str = triple->object->value.literal.string;
		len = triple->object->value.literal.string_len;
		goto chck;

	chck:
		if (len--, *str++ != '@') {
			break;
		}
		eos = memchr(str, ':', len) ?: str + len;
		/* see if we've got him */
		if (!~(r = find_rplc(str, eos - str))) {
			/* nope, add him then */
			r = add_rplc(str, eos - str);
		}
		/* bang coords and keep original object term */
		{
			static raptor_term nul_term;
			const size_t j = nbeefs[i] - 1U;
			raptor_term *t = beefs[i][j];
			const size_t sufx = str + len - eos;
			add_cord(r, i, j, t, sufx);
			beefs[i][j] = &nul_term;
		}
		break;

	default:
		break;
	}
	return;
}

static int
fltr(const char *fn, raptor_serializer *sfold)
{
	raptor_parser *p = raptor_new_parser(world, "trig");
	FILE *fp;
	int r;

	if (UNLIKELY(!(fp = fopen(fn, "r")))) {
		return -1;
	}

	if (LIKELY(!zterms)) {
		zterms = 64U;
		terms = malloc(zterms * sizeof(*terms));
		beefs = calloc(zterms, sizeof(*beefs));
		nbeefs = calloc(zterms, sizeof(*nbeefs));
	}

	rplc = malloc(zrplc = 256U);

	raptor_parser_set_statement_handler(p, NULL, flts);
	raptor_parser_set_namespace_handler(p, sfold, nmsp);
	r = raptor_parser_parse_file_stream(p, fp, NULL, base);

	raptor_free_parser(p);
	fclose(fp);
	return r;
}
#endif


/* our push parser */
static void
decl(void *usr, ttl_iri_t decl)
{
	struct _world_s *w = usr;

	ttl_decl_put(w->d, decl.pre, decl.val);
	return;
}

static void
stmt(void *usr, const ttl_term_t stmt[static 4U])
{
#define PRFX	"http://data.ga-group.nl/meta/mmh3/"
	static unsigned char prfx[80U] = PRFX;
	struct _world_s *w = usr;
	size_t prfn = strlenof(PRFX);
	size_t n;
	char *s;

	if (!prfx[prfn]) {
		goto prfx;
	}
	goto yep;
prfx:
	fputs("\
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n\
\n", stdout);

"@prefix gas: <http://schema.ga-group.nl/symbology#> .\n\
@prefix dct: <http://purl.org/dc/terms/> .\n\
@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n\
@prefix prov: <http://www.w3.org/ns/prov#> .\n\
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\
\n";
yep:
	n = swrite_stmt(&s, w, stmt);
	fwrite(s, 1, n, stdout);
	puts("");
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

	fputs("\ta\trdf:Statement .\n\n", stdout);
	return;
}

static struct _world_s
make_world(void)
{
	return (struct _world_s){.c = ttl_make_codec(), .d = ttl_make_decl()};
}

static void
free_world(struct _world_s w)
{
	ttl_free_codec(w.c);
	ttl_free_decl(w.d);
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
		rc = 1;
		goto out;
	}

	w = make_world();
	if (w.c == NULL || w.d == NULL) {
		error("\
Error: cannot instantiate ttl world");
		rc = 1;
		goto out;
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
	close(STDIN_FILENO);

out:
	ttl_free_parser(p);
	free_world(w);
	yuck_free(argi);
	return rc;
}

/* ttl-fold.c ends here */

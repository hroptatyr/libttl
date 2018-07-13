/*** ttldecl.c - libttl standard decl handler
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
#include <string.h>
#include "ttl.h"
#include "nifty.h"

typedef struct {
	size_t z;
	size_t n;
	char *b;
} buf_t;

struct ttl_decl_s {
	/* prefix buffer */
	buf_t p;
	/* expansion buffer */
	buf_t x;
	/* hash table of offsets into prefix and expansion buffer */
	size_t ht;
	size_t *po;
	size_t *io;
	size_t *in;
};


static uint64_t
MurmurHash64A(const void *key, size_t len, uint64_t seed)
{
#define hash(x, y)	MurmurHash64A((x), (y), 0UL)
#define hash2(x, y, z)	MurmurHash64A((x), (y), (z))
	const uint64_t m = 0xc6a4a7935bd1e995ULL;
	const int r = 47;

	uint64_t h = seed ^ (len * m);

	const uint64_t * data = (const uint64_t *)key;
	const uint64_t * end = data + (len/8);

	while(data != end) {
		uint64_t k = *data++;

		k *= m;
		k ^= k >> r;
		k *= m;

		h ^= k;
		h *= m;
	}

	const unsigned char * data2 = (const unsigned char*)data;

	switch(len & 7) {
	case 7: h ^= (uint64_t)(data2[6]) << 48;
	case 6: h ^= (uint64_t)(data2[5]) << 40;
	case 5: h ^= (uint64_t)(data2[4]) << 32;
	case 4: h ^= (uint64_t)(data2[3]) << 24;
	case 3: h ^= (uint64_t)(data2[2]) << 16;
	case 2: h ^= (uint64_t)(data2[1]) << 8;
	case 1: h ^= (uint64_t)(data2[0]);
		h *= m;
		break;
	};

	h ^= h >> r;
	h *= m;
	h ^= h >> r;

	return h;
}

static void
rehash(struct ttl_decl_s *w, size_t nuht)
{
	const size_t olht = w->ht;
	size_t *po = calloc((1ULL << nuht), sizeof(*po));
	size_t *io = calloc((1ULL << nuht), sizeof(*io));
	size_t *in = calloc((1ULL << nuht), sizeof(*in));
	const size_t nmsk = (1ULL << nuht) - 1ULL;

	for (size_t i = 0U, oln = (1ULL << olht); i < oln; i++) {
		if (w->in[i]) {
			const char *s = w->p.b + w->po[i];
			const size_t n = strlen(s);
			const uint64_t h = hash(s, n);
			const size_t nusl = h & nmsk;

			po[nusl] = w->po[i];
			io[nusl] = w->io[i];
			in[nusl] = w->in[i];
		}
	}

	/* materialise */
	free(w->po);
	free(w->io);
	free(w->in);
	w->ht = nuht;
	w->po = po;
	w->io = io;
	w->in = in;
	return;
}


ttl_decl_t*
ttl_make_decl(void)
{
	struct ttl_decl_s *r = malloc(sizeof(*r));

	if (UNLIKELY(r == NULL)) {
		return NULL;
	}
	/* otherwise set up default tables */
	/* get a tiny little hash table */
	r->ht = 4U;
	r->po = calloc((1ULL << r->ht), sizeof(*r->po));
	r->io = calloc((1ULL << r->ht), sizeof(*r->io));
	r->in = calloc((1ULL << r->ht), sizeof(*r->in));
	/* nott too much space for prefixes and expansions either */
	r->p.b = malloc(r->p.z = 128U), r->p.n = 0U;
	r->x.b = malloc(r->x.z = 1024U), r->x.n = 0U;
	return r;
}

void
ttl_free_decl(ttl_decl_t *r)
{
	free(r->p.b);
	free(r->x.b);
	free(r->po);
	free(r->io);
	free(r->in);
	free(r);
	return;
}

ttl_str_t
ttl_decl_get(const ttl_decl_t *x, ttl_str_t pre)
{
	const uint64_t h = hash(pre.str, pre.len);
	const size_t slot = h & ((1ULL << x->ht) - 1ULL);

	return (ttl_str_t){x->x.b + x->io[slot], x->in[slot]};
}

void
ttl_decl_put(ttl_decl_t *x, ttl_str_t pre, ttl_str_t xpn)
{
	const uint64_t h = hash(pre.str, pre.len);
	size_t ht = x->ht;
	size_t msk;
	size_t slot;

redo:
	msk = (1ULL << ht) - 1ULL;
	slot = h & msk;
	if (x->in[slot] && !x->p.b[x->po[slot] + pre.len]/*\0-term'd*/ &&
	    !memcmp(pre.str, x->p.b + x->po[slot], pre.len)) {
		/* redefinition */
		if (UNLIKELY(xpn.len > x->in[slot])) {
			goto bang_xpn;
		}
		/* otherwise just glue the guy into the existing spot */
		memcpy(x->x.b + x->io[slot], xpn.str, x->in[slot] = xpn.len);
	} else if (x->in[slot]) {
		/* conflict */
		const char *c = x->p.b + x->po[slot];
		const uint64_t hc = hash(c, strlen(c));

		/* test with one more H bit */
		for (ht++;
		     (msk = ((1ULL << ht) - 1ULL), (h & msk) == (hc & msk));
		     ht++);
		rehash(x, ht);
		goto redo;
	} else {
		/* bang prefix, \0 terminate */
		x->po[slot] = x->p.n;
		if (UNLIKELY(x->p.n + pre.len >= x->p.z)) {
			while ((x->p.z *= 2U) < x->p.n + pre.len);
			x->p.b = realloc(x->p.b, x->p.z);
		}
		memcpy(x->p.b + x->p.n, pre.str, pre.len);
		x->p.b[x->p.n += pre.len] = '\0';
		x->p.n++;

	bang_xpn:
		/* bang expansion */
		x->io[slot] = x->x.n;
		x->in[slot] = xpn.len;
		if (UNLIKELY(x->x.n + xpn.len >= x->x.z)) {
			while ((x->x.z *= 2U) < x->x.n + xpn.len);
			x->x.b = realloc(x->x.b, x->x.z);
		}
		/* bang expansion */
		memcpy(x->x.b + x->x.n, xpn.str, xpn.len);
		x->x.n += xpn.len;
	}
	return;
}

ttl_decl_iter_t
ttl_decl_iter_next(const ttl_decl_t *x, ttl_decl_iter_t i)
{
	for (size_t n = (1ULL << x->ht); i < n; i++) {
		if (x->in[i]) {
			return i + 1U;
		}
	}
	return 0U;
}

ttl_iri_t
ttl_decl_iter_get(const ttl_decl_t *x, ttl_decl_iter_t i)
{
	const size_t n = 1ULL << x->ht;
	if (UNLIKELY(!i || i > n)) {
		return (ttl_iri_t){};
	}
	/* just dress the tuple in a ttl_iri_t, the pre slot won't have a size
	 * but it's \nul terminated */
	i--;
	return (ttl_iri_t){{x->x.b + x->io[i], x->in[i]}, {x->p.b + x->po[i]}};
}

/* ttldecl.c ends here */

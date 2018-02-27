/*** ttl.c - trig/turtle/ntriples/nquads reader
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
#include <stdio.h>
#include <string.h>
#include "ttl.h"
#include "nifty.h"

static unsigned int esc_utf8;
static unsigned int esc_ctrl = 1U;

typedef struct {
	size_t z;
	size_t n;
	char *b;
} buf_t;

struct _writer_s {
	/* expander buffer */
	buf_t x;
	/* prefix buffer */
	buf_t p;
	/* expansion buffer */
	buf_t i;
	/* hash table of offsets into prefix and expansion buffer */
	size_t ht;
	size_t *po;
	size_t *io;
	size_t *in;
};

static inline __attribute__((pure, const)) char
_hexc(char c)
{
	if (LIKELY((unsigned char)c < 10U)) {
		return (char)(c ^ '0');
	}
	/* no check for the upper bound of c */
	return (char)(c + 'W');
}

static __attribute__((noinline)) ttl_str_t
_enquot(struct _writer_s *ww, ttl_str_t str)
{
	size_t k;

	if (UNLIKELY(memchr(str.str, '"', str.len) != NULL)) {
		goto enq;
	}
	if (UNLIKELY(memchr(str.str, '\\', str.len) != NULL)) {
		goto enq;
	}
	if (esc_ctrl && esc_utf8) {
		for (size_t i = k = 0U; i < str.len; i++) {
			if ((char)str.str[i] < ' ') {
				goto enq;
			}
		}
	} else if (esc_ctrl) {
		for (size_t i = k = 0U; i < str.len; i++) {
			if ((unsigned char)str.str[i] < ' ') {
				goto enq;
			}
		}
	} else if (esc_utf8) {
		for (size_t i = k = 0U; i < str.len; i++) {
			if ((char)str.str[i] < '\0') {
				goto enq;
			}
		}
	}
	return str;
enq:
	if (UNLIKELY(2U * str.len >= ww->x.z)) {
		while ((ww->x.z *= 2U) < 2U * str.len);
		ww->x.b = realloc(ww->x.b, ww->x.z);
	}
	for (size_t i = k = 0U; i < str.len;) {
		/* utf8 seq ranges */
		uint_fast32_t x = 0U;

		if (!esc_utf8 && (char)str.str[i] < '\0') {
			goto literal;
		} else if (esc_ctrl && (char)str.str[i] < ' ') {
			ww->x.b[k++] = '\\';
			switch (str.str[i++]) {
			case '\t':
				ww->x.b[k++] = 't';
				break;
			case 'n':
				ww->x.b[k++] = 'n';
				break;
			case 'r':
				ww->x.b[k++] = 'r';
				break;
			case 'f':
				ww->x.b[k++] = 'f';
				break;
			case 'a':
				ww->x.b[k++] = 'a';
				break;
			case 'b':
				ww->x.b[k++] = 'b';
				break;
			case 'v':
				ww->x.b[k++] = 'v';
				break;
			default:
				i--;
				if ((unsigned char)str.str[i] < 0x80U ||
				    !esc_utf8) {
					goto literal;
				} else if ((unsigned char)str.str[i] < 0xc2U) {
					/* invalid utf8 */
					ww->x.b[k++] = '?';
				} else if ((unsigned char)str.str[i] < 0xe0U) {
					/* 110x xxxx 10xx xxxx */
					x ^= str.str[i++] & 0b11111U;
					x <<= 6U;
					x ^= str.str[i++] & 0b111111U;
					ww->x.b[k++] = 'u';
					goto pr_u;
				} else if ((unsigned char)str.str[i] < 0xf0U) {
					/* 1110 xxxx 10xx xxxx 10xx xxxx */
					x ^= str.str[i++] & 0b1111U;
					x <<= 6U;
					x ^= str.str[i++] & 0b111111U;
					x <<= 6U;
					x ^= str.str[i++] & 0b111111U;
					ww->x.b[k++] = 'u';
					goto pr_u;
				} else if ((unsigned char)str.str[i] < 0xf0U) {
					/* 1111 0xxx  10xx xxxx  10xx xxxx
					 * 10xx xxxx */
					x ^= str.str[i++] & 0b111U;
					x <<= 6U;
					x ^= str.str[i++] & 0b111111U;
					x <<= 6U;
					x ^= str.str[i++] & 0b111111U;
					x <<= 6U;
					x ^= str.str[i++] & 0b111111U;
					ww->x.b[k++] = 'U';
					goto pr_U;
				} else {
					ww->x.b[k++] = '?';
				}
				break;

			pr_U:
				ww->x.b[k++] = _hexc(x >> 28U & 0xfU);
				ww->x.b[k++] = _hexc(x >> 24U & 0xfU);
				ww->x.b[k++] = _hexc(x >> 20U & 0xfU);
				ww->x.b[k++] = _hexc(x >> 16U & 0xfU);
			pr_u:
				ww->x.b[k++] = _hexc(x >> 12U & 0xfU);
				ww->x.b[k++] = _hexc(x >> 8U & 0xfU);
				ww->x.b[k++] = _hexc(x >> 4U & 0xfU);
				ww->x.b[k++] = _hexc(x >> 0U & 0xfU);
				break;
			}
		} else if (str.str[i] == '"' || str.str[i] == '\\') {
			ww->x.b[k++] = '\\';
			goto literal;
		} else {
		literal:
			ww->x.b[k++] = str.str[i++];
		}
	}
	return (ttl_str_t){ww->x.b, k};
}

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
rehash(struct _writer_s *w, size_t nuht)
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

static ttl_str_t
get_decl(const struct _writer_s *w, ttl_str_t pre)
{
	const uint64_t h = hash(pre.str, pre.len);
	const size_t slot = h & ((1ULL << w->ht) - 1ULL);

	return (ttl_str_t){w->i.b + w->io[slot], w->in[slot]};
}

static void
put_decl(struct _writer_s *w, ttl_str_t pre, ttl_str_t xpn)
{
	const uint64_t h = hash(pre.str, pre.len);
	size_t ht = w->ht;
	size_t msk;
	size_t slot;

redo:
	msk = (1ULL << ht) - 1ULL;
	slot = h & msk;
	if (w->in[slot] && !w->p.b[w->po[slot] + pre.len]/*\0-term'd*/ &&
	    !memcmp(pre.str, w->p.b + w->po[slot], pre.len)) {
		/* redefinition */
		if (UNLIKELY(xpn.len > w->in[slot])) {
			goto bang_xpn;
		}
		/* otherwise just glue the guy into the existing spot */
		memcpy(w->i.b + w->io[slot], xpn.str, w->in[slot] = xpn.len);
	} else if (w->in[slot]) {
		/* conflict */
		const char *c = w->p.b + w->po[slot];
		const uint64_t hc = hash(c, strlen(c));

		/* test with one more H bit */
		for (ht++;
		     (msk = ((1ULL << ht) - 1ULL), (h & msk) == (hc & msk));
		     ht++);
		rehash(w, ht);
		goto redo;
	} else {
		/* bang prefix, \0 terminate */
		w->po[slot] = w->p.n;
		if (UNLIKELY(w->p.n + pre.len >= w->p.z)) {
			while ((w->p.z *= 2U) < w->p.n + pre.len);
			w->p.b = realloc(w->p.b, w->p.z);
		}
		memcpy(w->p.b + w->p.n, pre.str, pre.len);
		w->p.b[w->p.n += pre.len] = '\0';
		w->p.n++;

	bang_xpn:
		/* bang expansion */
		w->io[slot] = w->i.n;
		w->in[slot] = xpn.len;
		if (UNLIKELY(w->i.n + xpn.len >= w->i.z)) {
			while ((w->i.z *= 2U) < w->i.n + xpn.len);
			w->i.b = realloc(w->i.b, w->i.z);
		}
		/* bang expansion */
		memcpy(w->i.b + w->i.n, xpn.str, xpn.len);
		w->i.n += xpn.len;
	}
	return;
}

static void
fwrite_iri(struct _writer_s *w, ttl_iri_t t, void *stream)
{
	if (t.pre.len) {
		ttl_str_t x = get_decl(w, t.pre);

		if (x.len) {
			fputc('<', stdout);
			fwrite(x.str, 1, x.len, stream);
			fwrite(t.val.str, 1, t.val.len, stream);
			fputc('>', stdout);
		} else {
			fwrite(t.pre.str, 1, t.pre.len, stream);
			fputc(':', stream);
			fwrite(t.val.str, 1, t.val.len, stream);
		}
	} else if (t.val.len > 1U || *t.val.str != 'a') {
		fputc('<', stdout);
		fwrite(t.val.str, 1, t.val.len, stream);
		fputc('>', stdout);
	} else {
		fputc('a', stdout);
	}
	return;
}

static void
fwrite_lit(struct _writer_s *w, ttl_lit_t t, void *stream)
{
	t.val = _enquot(w, t.val);

	fputc('"', stdout);
	fwrite(t.val.str, 1, t.val.len, stream);
	fputc('"', stdout);
	if (t.typ.val.len) {
		fputc('^', stream);
		fputc('^', stream);
		fwrite_iri(w, t.typ, stream);
	}
	if (t.lng.len) {
		fputc('@', stream);
		fwrite(t.lng.str, 1, t.lng.len, stream);
	}
	return;
}

static void
fwrite_term(struct _writer_s *w, ttl_term_t t, void *stream)
{
	switch (t.typ) {
	case TTL_TYP_IRI:
		fwrite_iri(w, t.iri, stream);
		break;
	case TTL_TYP_LIT:
		fwrite_lit(w, t.lit, stream);
		break;
	case TTL_TYP_BLA:
		fprintf(stdout, "_:b%016lx", t.bla.h[0U]);
		break;
	default:
		break;
	}
	return;
}



static void
decl(void *usr, ttl_iri_t decl)
{
	struct _writer_s *w = usr;

	put_decl(w, decl.pre, decl.val);
	return;
}

static void
stmt(void *usr, const ttl_term_t stmt[static 4U])
{
	struct _writer_s *w = usr;

	fwrite_term(w, stmt[TTL_SUBJ], stdout);
	fputc('\t', stdout);
	fwrite_term(w, stmt[TTL_PRED], stdout);
	fputc('\t', stdout);
	fwrite_term(w, stmt[TTL_OBJ], stdout);
	if (stmt[TTL_GRPH].typ) {
		fputc('\t', stdout);
		fwrite_term(w, stmt[TTL_GRPH], stdout);
	}
	fputc('\t', stdout);
	fputc('.', stdout);
	fputc('\n', stdout);
	return;
}

static struct _writer_s*
make_writer(void)
{
	struct _writer_s *r = malloc(sizeof(*r));
	if (UNLIKELY(r == NULL)) {
		return NULL;
	}
	r->x.b = malloc(r->x.z = 4096U), r->x.n = 0U;
	/* get a tiny little hash table */
	r->ht = 4U;
	r->po = calloc((1ULL << r->ht), sizeof(*r->po));
	r->io = calloc((1ULL << r->ht), sizeof(*r->io));
	r->in = calloc((1ULL << r->ht), sizeof(*r->in));
	/* nott too much space for prefixes and expansions either */
	r->p.b = malloc(r->p.z = 128U), r->p.n = 0U;
	r->i.b = malloc(r->i.z = 1024U), r->i.n = 0U;
	return r;
}

static void
free_writer(struct _writer_s *w)
{
	free(w->x.b);
	free(w->p.b);
	free(w->i.b);
	free(w->po);
	free(w->io);
	free(w->in);
	free(w);
	return;
}


int
main(void)
{
	static char buf[16U * 4096U];
	ttl_parser_t *p = ttl_make_parser();
	struct _writer_s *w = make_writer();

	p->hdl = (ttl_handler_t){decl, stmt};
	p->usr = w;

	for (ssize_t nrd; (nrd = read(STDIN_FILENO, buf, sizeof(buf))) > 0;) {
		if (ttl_parse_chunk(p, buf, nrd) < 0) {
			fputs("\
Error: parsing stdin\n", stderr);
			break;
		}
	}

	ttl_free_parser(p);
	free_writer(w);
	return 0;
}

/* ttl.c ends here */

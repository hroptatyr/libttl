/*** ttl2nt.c - trig/turtle/ntriples/nquads reader
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
#include <stdarg.h>
#include <errno.h>
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
	/* codec */
	ttl_codec_t *c;
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
	t.val = ttl_dequot_str(w->c, t.val, TTL_QUOT_UTF8);
	t.val = ttl_enquot_str(w->c, t.val, TTL_QUOT_PRNT ^ TTL_QUOT_CTRL);

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

	ttl_codec_clear(w->c);
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
	/* for string massage */
	r->c = ttl_make_codec();
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
	ttl_free_codec(w->c);
	free(w->p.b);
	free(w->i.b);
	free(w->po);
	free(w->io);
	free(w->in);
	free(w);
	return;
}


#include "ttl2nt.yucc"

int
main(int argc, char *argv[])
{
	static char buf[16U * 4096U];
	static yuck_t argi[1U];
	struct _writer_s *w = NULL;
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
	} else if ((w = make_writer()) == NULL) {
		error("\
Error: cannot instantiate nt writer");
		rc = 1;
		goto out;
	}

	p->hdl = (ttl_handler_t){decl, stmt};
	p->usr = w;

	for (ssize_t nrd; (nrd = read(STDIN_FILENO, buf, sizeof(buf))) > 0;) {
		if (ttl_parse_chunk(p, buf, nrd) < 0) {
			fputs("\
Error: parsing stdin\n", stderr);
			break;
		}
	}

out:
	ttl_free_parser(p);
	free_writer(w);
	yuck_free(argi);
	return rc;
}

/* ttl2nt.c ends here */

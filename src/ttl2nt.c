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

struct _writer_s {
	/* expander buffer */
	struct {
		size_t z;
		size_t n;
		char *b;
	} x;
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


static void
fwrite_iri(struct _writer_s *UNUSED(w), ttl_iri_t t, void *stream)
{
	if (t.pre.len || t.val.len > 1U || *t.val.str != 'a') {
		fputc('<', stdout);
		fwrite(t.pre.str, 1, t.pre.len, stream);
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
		fwrite(t.typ.pre.str, 1, t.typ.pre.len, stream);
		fwrite(t.typ.val.str, 1, t.typ.val.len, stream);
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
	r->x.b = malloc(r->x.z = 4096U);
	r->x.n = 0U;
	return r;
}

static void
free_writer(struct _writer_s *w)
{
	free(w->x.b);
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

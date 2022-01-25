/*** ttlstr.c - enquoting and dequoting
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

struct _codec_s {
	/* expander buffer */
	buf_t x;
};

static __attribute__((pure, const)) uint_fast32_t
_chex(char c)
{
	if (LIKELY((unsigned char)(c ^ '0') < 10)) {
		return c ^ '0';
	} else if ((unsigned char)(c | 0x20) - 'W' < 16){
		return (unsigned char)(c | 0x20) - 'W';
	}
	/* no error code */
	return -1U;
}

static inline __attribute__((pure, const)) char
_hexc(char c)
{
	if (LIKELY((unsigned char)c < 10U)) {
		return (char)(c ^ '0');
	}
	/* no check for the upper bound of c */
	return (char)(c + 'W');
}


ttl_codec_t*
ttl_make_codec(void)
{
	struct _codec_s *r = malloc(sizeof(*r));
	if (UNLIKELY(r == NULL)) {
		return NULL;
	}
	r->x.b = malloc(r->x.z = 4096U), r->x.n = 0U;
	return r;
}

void
ttl_free_codec(ttl_codec_t *x)
{
	free(x->x.b);
	free(x);
	return;
}

void
ttl_codec_clear(ttl_codec_t *x)
{
	x->x.n = 0U;
	return;
}

ttl_str_t
ttl_dequot_str(ttl_codec_t *cc, ttl_str_t str, unsigned int what)
{
	size_t k = cc->x.n;
	size_t b;

	if (UNLIKELY(k + str.len >= cc->x.z)) {
		while ((cc->x.z *= 2U) < k + str.len);
		cc->x.b = realloc(cc->x.b, cc->x.z);
	}
	if (!what || memchr(str.str, '\\', str.len) == NULL) {
		return str;
	}
	/* copy no quotes, or single quotes or triple quotes */
	b = 1U;
	b -= str.str[0 - b] <= ' ';
	b += str.str[0 - b] == str.str[0 - b - 1];
	b += str.str[0 - b] == str.str[0 - b - 1];
	memcpy(cc->x.b + k, str.str - b, b);
	k += b;
	for (size_t i = 0U; i < str.len; i++, k++) {
		/* utf8 seq ranges */
		uint_fast32_t x = 0U;

		if (LIKELY(str.str[i] != '\\')) {
			goto literal;
		}
		switch (str.str[++i]) {
		case 't':
			if (!(what & TTL_QUOT_CTRL)) {
				goto escaped;
			}
			cc->x.b[k] = '\t';
			break;
		case 'n':
			if (!(what & TTL_QUOT_CTRL)) {
				goto escaped;
			}
			cc->x.b[k] = '\n';
			break;
		case 'r':
			if (!(what & TTL_QUOT_CTRL)) {
				goto escaped;
			}
			cc->x.b[k] = '\r';
			break;
		case 'U':
			if (!(what & TTL_QUOT_UTF8)) {
				goto escaped;
			}
			x ^= _chex(str.str[++i]);
			x <<= 4U;
			x ^= _chex(str.str[++i]);
			x <<= 4U;
			x ^= _chex(str.str[++i]);
			x <<= 4U;
			x ^= _chex(str.str[++i]);
		case 'u':
			if (!(what & TTL_QUOT_UTF8)) {
				goto escaped;
			}
			x ^= _chex(str.str[++i]);
			x <<= 4U;
			x ^= _chex(str.str[++i]);
			x <<= 4U;
			x ^= _chex(str.str[++i]);
			x <<= 4U;
			x ^= _chex(str.str[++i]);

#define CAP0	(1U << (7U))
#define CAP1	(1U << (11U))
#define CAP2	(1U << (16U))
#define CAP3	(1U << (21U))

			if (x < CAP0) {
				cc->x.b[k] = x;
			} else if (x < CAP1) {
				/* 110x xxxx  10xx xxxx */
				cc->x.b[k++] = 0xc0U ^ (x >> 6U);
				cc->x.b[k] = 0x80U ^ (x & 0b111111U);
			} else if (x < CAP2) {
				/* 1110 xxxx  10xx xxxx  10xx xxxx */
				cc->x.b[k++] = 0xe0U | (x >> 12U);
				cc->x.b[k++] = 0x80U | ((x >> 6U) & 0b111111U);
				cc->x.b[k] = 0x80U | (x & 0b111111U);
			} else if (x < CAP3) {
				/* 1111 0xxx  10xx xxxx  10xx xxxx  10xx xxxx*/
				cc->x.b[k++] = 0xf0U | (x >> 18U);
				cc->x.b[k++] = 0x80U | ((x >> 12U) & 0b111111U);
				cc->x.b[k++] = 0x80U | ((x >> 6U) & 0b111111U);
				cc->x.b[k] = 0x80U | (x & 0b111111U);
			} else {
				cc->x.b[k] = '?';
			}
			break;
		case 'f':
			if (!(what & TTL_QUOT_CTRL)) {
				goto escaped;
			}
			cc->x.b[k] = '\f';
			break;
		case 'a':
			if (!(what & TTL_QUOT_CTRL)) {
				goto escaped;
			}
			cc->x.b[k] = '\a';
			break;
		case 'b':
			if (!(what & TTL_QUOT_CTRL)) {
				goto escaped;
			}
			cc->x.b[k] = '\b';
			break;
		case 'v':
			if (!(what & TTL_QUOT_CTRL)) {
				goto escaped;
			}
			cc->x.b[k] = '\v';
			break;
		default:
			if (!(what & TTL_QUOT_BKSL)) {
			escaped:
				cc->x.b[k++] = '\\';
			}
		literal:
			cc->x.b[k] = str.str[i];
			break;
		}
	}
	/* return pointer to x buffer */
	str.str = cc->x.b + cc->x.n + b;
	/* new length */
	str.len = k - (cc->x.n + b);
	/* up the use counter */
	cc->x.n = k;
	return str;
}

ttl_str_t
ttl_enquot_str(ttl_codec_t *cc, ttl_str_t str, unsigned int what)
{
	size_t k = cc->x.n;
	size_t b;

	if (what & TTL_QUOT_PRNT && memchr(str.str, '"', str.len) != NULL) {
		goto enq;
	}
	if (what & TTL_QUOT_BKSL && memchr(str.str, '\\', str.len) != NULL) {
		goto enq;
	}
	if (what & TTL_QUOT_CTRL && what & TTL_QUOT_UTF8) {
		for (size_t i = k = 0U; i < str.len; i++) {
			if ((char)str.str[i] < ' ') {
				goto enq;
			}
		}
	}
	if (what & TTL_QUOT_CTRL) {
		for (size_t i = k = 0U; i < str.len; i++) {
			if ((unsigned char)str.str[i] < ' ') {
				goto enq;
			}
		}
	}
	if (what & TTL_QUOT_UTF8) {
		for (size_t i = k = 0U; i < str.len; i++) {
			if ((char)str.str[i] < '\0') {
				goto enq;
			}
		}
	}
	return str;
enq:
	if (UNLIKELY(k + 2U * str.len + 3U/*for """*/ >= cc->x.z)) {
		while ((cc->x.z *= 2U) < k + 2U * str.len + 3U/*for """*/);
		cc->x.b = realloc(cc->x.b, cc->x.z);
	}

	/* copy no quotes, or single quotes or triple quotes */
	b = 1U;
	b -= str.str[0 - b] <= ' ';
	b += str.str[0 - b] == str.str[0 - b - 1];
	b += str.str[0 - b] == str.str[0 - b - 1];
	memcpy(cc->x.b + k, str.str - b, b);
	k += b;

	for (size_t i = 0U; i < str.len;) {
		/* utf8 seq ranges */
		uint_fast32_t x = 0U;

		if (what & TTL_QUOT_UTF8 &&
		    (unsigned char)str.str[i] >= 0x80U) {
			cc->x.b[k++] = '\\';
			if (0) {
				;
			} else if ((unsigned char)str.str[i] < 0xc2U) {
				/* invalid utf8 */
				cc->x.b[k++] = '?';
			} else if ((unsigned char)str.str[i] < 0xe0U) {
				/* 110x xxxx 10xx xxxx */
				x ^= str.str[i++] & 0b11111U;
				x <<= 6U;
				x ^= str.str[i++] & 0b111111U;
				cc->x.b[k++] = 'u';
				goto pr_u;
			} else if ((unsigned char)str.str[i] < 0xf0U) {
				/* 1110 xxxx 10xx xxxx 10xx xxxx */
				x ^= str.str[i++] & 0b1111U;
				x <<= 6U;
				x ^= str.str[i++] & 0b111111U;
				x <<= 6U;
				x ^= str.str[i++] & 0b111111U;
				cc->x.b[k++] = 'u';
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
				cc->x.b[k++] = 'U';
				goto pr_U;
			} else {
				cc->x.b[k++] = '?';
			}
			continue;

		pr_U:
			cc->x.b[k++] = _hexc(x >> 28U & 0xfU);
			cc->x.b[k++] = _hexc(x >> 24U & 0xfU);
			cc->x.b[k++] = _hexc(x >> 20U & 0xfU);
			cc->x.b[k++] = _hexc(x >> 16U & 0xfU);
		pr_u:
			cc->x.b[k++] = _hexc(x >> 12U & 0xfU);
			cc->x.b[k++] = _hexc(x >> 8U & 0xfU);
			cc->x.b[k++] = _hexc(x >> 4U & 0xfU);
			cc->x.b[k++] = _hexc(x >> 0U & 0xfU);
		} else if (what & TTL_QUOT_CTRL &&
			   (unsigned char)str.str[i] < ' ') {
			cc->x.b[k++] = '\\';
			switch (str.str[i++]) {
			case '\t':
				cc->x.b[k++] = 't';
				break;
			case '\n':
				cc->x.b[k++] = 'n';
				break;
			case '\r':
				cc->x.b[k++] = 'r';
				break;
			case '\f':
				cc->x.b[k++] = 'f';
				break;
			case '\a':
				cc->x.b[k++] = 'a';
				break;
			case '\b':
				cc->x.b[k++] = 'b';
				break;
			case '\v':
				cc->x.b[k++] = 'v';
				break;
			default:
				cc->x.b[k++] = '?';
				cc->x.b[k++] = str.str[i - 1];
				cc->x.b[k++] = '?';
				break;
			}
		} else if (what & TTL_QUOT_BKSL &&
			   (unsigned char)str.str[i] == '\\') {
			cc->x.b[k++] = '\\';
			goto literal;
		} else if (what & TTL_QUOT_PRNT &&
			   (unsigned char)str.str[i] == '\\') {
			cc->x.b[k++] = str.str[i++];
			/* already quoted */
			goto literal;
		} else if (what & TTL_QUOT_PRNT &&
			   (unsigned char)str.str[i] == '"') {
			cc->x.b[k++] = '\\';
			goto literal;
		} else {
		literal:
			cc->x.b[k++] = str.str[i++];
		}
	}
	/* return pointer to x buffer */
	str.str = cc->x.b + cc->x.n + b;
	/* new length */
	str.len = k - (cc->x.n + b);
	/* up the use counter */
	cc->x.n = k;
	return str;
}

/* ttlstr.c ends here */

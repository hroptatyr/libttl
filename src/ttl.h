/*** ttl.h - trig/turtle/ntriples/nquads reader
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
#if !defined INCLUDED_ttl_h_
#define INCLUDED_ttl_h_
#include <stddef.h>
#include <stdint.h>

typedef struct {
	const char *str;
	size_t len;
} ttl_str_t;

typedef enum {
	TTL_TYP_UNK,
	TTL_TYP_IRI,
	TTL_TYP_LIT,
	TTL_TYP_BLA,
	TTL_TYP_DIR,
} ttl_typ_t;

enum {
	TTL_SUBJ,
	TTL_PRED,
	TTL_OBJ,
	TTL_GRPH,
};

typedef struct {
	ttl_str_t val;
	/* for the prefixed form this holds the prefix string */
	ttl_str_t pre;
} ttl_iri_t;

typedef struct {
	ttl_str_t val;
	ttl_iri_t typ;
	ttl_str_t lng;
} ttl_lit_t;

typedef struct {
	uint64_t h[2U];
} ttl_bla_t;

typedef struct {
	ttl_typ_t typ;
	union {
		ttl_iri_t iri;
		ttl_lit_t lit;
		ttl_bla_t bla;
		ttl_iri_t dir;
	};
} ttl_term_t;

typedef struct {
	void(*decl)(void *usr, ttl_iri_t decl);
	void(*stmt)(void *usr, const ttl_term_t stmt[static 4U]);
} ttl_handler_t;

typedef struct {
	ttl_handler_t hdl;
	void *usr;
} ttl_parser_t;


extern ttl_parser_t*
ttl_make_parser(void);

extern void
ttl_free_parser(ttl_parser_t*);

extern int
ttl_parse_chunk(ttl_parser_t*, const char *buf, size_t len);

#endif	/* INCLUDED_ttl_h_ */

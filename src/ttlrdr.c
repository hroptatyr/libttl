/*** ttlrdr.c - trig/turtle/ntriples/nquads reader
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

/* using a petrinet we have
 * STATE 0, S, G, P, O, Q, GS, GP, GO
 * TRANS 0  + DIR -> 0
 *       0  + IRI -> S
 *       S  + IRI -> P
 *       S  + {   -> G
 *       P  + IRI -> O
 *       P  + LIT -> O
 *       P  + [   -> push + BS
 *       G  + }   -> 0
 *       G  + IRI -> GS
 *       G  + DIR -> G
 *       O  + .   -> 0
 *       O  + ,   -> P
 *       O  + ;   -> S
 *       O  + IRI -> Q
 *       GS + IRI -> GP
 *       GP + IRI -> GO
 *       GO + .   -> G
 *       GO + ,   -> GP
 *       GO + ;   -> GS
 *       GO + }   -> 0
 *       BS + IRI -> BP
 *       BP + IRI -> BO
 *       BP + LIT -> BO
 *       BP + ]   -> pop
 *       Q  + .   -> 0 */
typedef enum {
	STATE_0,
	STATE_S,
	STATE_P,
	STATE_O,
	STATE_Q,
	STATE_G,
	STATE_GS,
	STATE_GP,
	STATE_GO,
	STATE_BS,
	STATE_BP,
	STATE_BO,
} state_t;

typedef enum {
	TRANS_ERR = -1,
	TRANS_UNK = 0,
	TRANS_DIR,
	TRANS_IRI,
	TRANS_LIT,
	/* { */
	TRANS_BRO,
	/* } */
	TRANS_BRC,
	/* [ */
	TRANS_SQO,
	/* ] */
	TRANS_SQC,
	TRANS_DOT,
	TRANS_COM,
	TRANS_SEM,
} trans_t;

typedef struct {
	size_t z;
	size_t n;
	char *b;
} buf_t;

struct _parser_s {
	/* public view on things, must be first member */
	ttl_parser_t public;
	/* parser buffer */
	buf_t p;
	/* expander buffer */
	buf_t x;
	/* blank node counter */
	size_t b;
	/* state stack */
	size_t S;
	size_t Z;
	struct {
		/* current state */
		state_t state;
		/* statement, room for a quad */
		ttl_term_t stmt[4U];
	} *s;
};

static inline __attribute__((pure, const)) size_t
min_z(size_t a, size_t b)
{
	return a < b ? a : b;
}


static const char*
__parse_prefix(const char *bp, const char *const ep)
{
	const char *const sp = bp;

	if (*bp == ':') {
		/* finished */
		return bp + 1U;
	} else if (*bp == '_') {
		/* bnode innit? */
		;
	} else if (*bp >= '\0' &&
		   *bp < 'A' || *bp > 'Z' &&
		   *bp < 'a' || *bp > 'z') {
		/* invalid */
		return NULL;
	}
	while (bp < ep && (unsigned char)*bp > ' ' && *bp++ != ':');
	return bp[-1] == ':' && bp < ep ? bp : sp;
}

static const char*
__parse_local(const char *bp, const char *const ep)
{
	const char *const sp = bp;

	for (; bp < ep && (unsigned char)*bp > ' '; bp++);
	return bp < ep ? bp : sp;
}

static const char*
_parse_iri(ttl_iri_t *t, const char *bp, const char *const ep)
{
	const char *const sp = bp;

	if (UNLIKELY(*bp++ != '<')) {
		/* not an iri */
		return NULL;
	}
	while (bp < ep && *bp++ != '>');
	if (LIKELY(bp <= ep)) {
		*t = (ttl_iri_t){{sp + 1U, bp - 1U - (sp + 1U)}};
		return bp;
	}
	return sp;
}

static const char*
parse_qname(ttl_iri_t *tt, const char *bp, const char *const ep)
{
	const char *const sp = bp;
	const char *xp;

	if (UNLIKELY((bp = __parse_prefix(sp, ep)) == NULL)) {
		/* propagate error */
		return NULL;
	}
	xp = bp;
	if (UNLIKELY((bp = __parse_local(xp, ep)) == NULL)) {
		/* propagate error */
		return NULL;
	} else if (bp == xp) {
		/* rollback */
		return sp;
	}
	tt->val = (ttl_str_t){xp, bp - xp};
	tt->pre = (ttl_str_t){sp, xp - sp - (xp > sp)};
	return bp;
}

static const char*
parse_datatype(ttl_iri_t *t, const char *bp, const char *const ep)
{
	const char *const sp = bp;

	if (UNLIKELY(*bp++ != '^')) {
		/* nope */
		return NULL;
	} else if (ep > bp && *bp++ != '^') {
		/* still nope */
		return NULL;
	} else if (ep <= bp) {
		/* recall */
		return sp;
	}
	/* could be IRI now or a qname */
	switch (*bp) {
		const char *xp;
	case '<':
		/* iri */
		xp = bp;
		if (UNLIKELY((bp = _parse_iri(t, xp, ep)) == NULL)) {
			/* error */
			return NULL;
		} else if (UNLIKELY(bp == xp || bp >= ep)) {
			/* rollback */
			return sp;
		}
		break;
	default:
		/* qname */
		xp = bp;
		if (UNLIKELY((bp = parse_qname(t, xp, ep)) == NULL)) {
			/* propagate */
			return NULL;
		} else if (UNLIKELY(bp == xp || bp >= ep)) {
			/* rollback */
			return sp;
		}
		break;
	}
	return bp < ep ? bp : sp;
}

static const char*
parse_language(ttl_str_t *t, const char *bp, const char *const ep)
{
	const char *const sp = bp;

	if (UNLIKELY(*bp++ != '@')) {
		/* not a language */
		return NULL;
	}
	/* wait for non-alpha */
	for (; bp < ep &&
		     (unsigned char)*bp > ' ' &&
		     *bp != '.' && *bp != ',' && *bp != ';'; bp++);
	*t = (ttl_str_t){sp + 1U, bp - (sp + 1U)};
	return bp < ep ? bp : sp;
}

static const char*
_parse_lit(ttl_lit_t *tt, const char *bp, const char *const ep)
{
	const char *const sp = bp;
	const char *bosp = bp, *eosp = bp;

	switch (*bp) {
	case '"':
		if (ep > ++bp && *bp == '"') {
			/* could be """ or "" */
			bosp++, eosp++;
			if (ep > ++bp && *bp == '"') {
				/* aah triple-quotes */
				bp++;
				do {
					while (bp < ep &&
					       (*bp == '\\' && (bp += 2U, 1) ||
						*bp++ != '"'));
				} while (bp < ep && *bp++ != '"' ||
					 bp < ep && *bp++ != '"');
				eosp = bp;
				bosp += 2U, eosp -= 3U;
			}
		} else {
			while (bp < ep &&
			       (*bp == '\\' && (bp += 2U, 1) || *bp++ != '"'));
			eosp = bp;
			bosp++, eosp--;
		}
		break;
	case '\'':
		if (ep > ++bp && *bp == '\'') {
			/* could be ''' or '' */
			bosp++, eosp++;
			if (ep > ++bp && *bp == '\'') {
				/* aah triple-quotes */
				bp++;
				do {
					while (bp < ep &&
					       (*bp == '\\' && (bp += 2U, 1) ||
						*bp++ != '\''));
				} while (bp < ep && *bp++ != '\'' ||
					 bp < ep && *bp++ != '\'');
				eosp = bp;
				bosp += 2U, eosp -= 3U;
			}
		} else {
			while (bp < ep &&
			       (*bp == '\\' && (bp += 2U, 1) || *bp++ != '\''));
			eosp = bp;
			bosp++, eosp--;
		}
		break;
	default:
		/* that's just wrong */
		return NULL;
	}
	if (UNLIKELY(bp >= ep)) {
		/* rollback */
		return sp;
	}
	tt->val = (ttl_str_t){bosp, eosp - bosp};
	tt->typ = (ttl_iri_t){};
	tt->lng = (ttl_str_t){};
	switch (*bp) {
		const char *xp;

	case '^':
		/* datatype */
		xp = bp;
		if (UNLIKELY((bp = parse_datatype(&tt->typ, xp, ep)) == NULL)) {
			/* shit */
			return NULL;
		} else if (UNLIKELY(bp == xp || bp >= ep)) {
			/* rollback */
			return sp;
		}
		break;
	case '@':
		/* language */
		xp = bp;
		if (UNLIKELY((bp = parse_language(&tt->lng, xp, ep)) == NULL)) {
			/* shit */
			return NULL;
		} else if (UNLIKELY(bp == xp || bp >= ep)) {
			/* rollback */
			return sp;
		}
		break;
	default:
		break;
	}
	return bp < ep ? bp : sp;
}

static const char*
_parse_dir(ttl_iri_t *t, const char *bp, const char *const ep)
{
	static const char prfx[] = "@prefix";
	static const char base[] = "@base";
	const char *const sp = bp;
	enum {
		DTYP_UNK,
		DTYP_PRFX,
		DTYP_BASE,
	} dtyp = DTYP_UNK;

	if (0) {
		;
	} else if (LIKELY(ep > bp + strlenof(base)) &&
		   !memcmp(bp, base, strlenof(base))) {
		dtyp = DTYP_BASE;
		bp += strlenof(base);
	} else if (LIKELY(ep > bp + strlenof(prfx)) &&
		   !memcmp(bp, prfx, strlenof(prfx))) {
		dtyp = DTYP_PRFX;
		bp += strlenof(prfx);
	} else if (ep <= bp + strlenof(prfx)) {
		/* best to be on the safe side and recall this guy */
		return sp;
	} else {
		/* error */
		return NULL;
	}
	/* any form of whitespace */
	if (UNLIKELY(!((unsigned char)*bp++ <= ' '))) {
		return NULL;
	}
	switch (dtyp) {
	case DTYP_PRFX:
		/* whitespace */
		for (; bp < ep && (unsigned char)*bp <= ' '; bp++);
		if (UNLIKELY(bp >= ep)) {
			return sp;
		}

		/* read to colon */
		with (const char *tp = bp) {
			if (UNLIKELY((bp = __parse_prefix(tp, ep)) == NULL)) {
				/* just great */
				return NULL;
			} else if (UNLIKELY(bp == tp || bp >= ep)) {
				/* rollback */
				return sp;
			}
			t->pre = (ttl_str_t){tp, bp - tp - 1U};
		}

		/* fallthrough */
	case DTYP_BASE:
		/* whitespace */
		for (; bp < ep && (unsigned char)*bp <= ' '; bp++);
		if (UNLIKELY(bp >= ep)) {
			return sp;
		}

		with (const char *tp = bp) {
			ttl_iri_t tmp;
			if (UNLIKELY((bp = _parse_iri(&tmp, tp, ep)) == NULL)) {
				/* error */
				return NULL;
			} else if (UNLIKELY(bp == tp || bp >= ep)) {
				/* rollback */
				return sp;
			}
			/* assemble the target */
			t->val = tmp.val;
		}
		break;
	}
	/* whitespace */
	for (; bp < ep && (unsigned char)*bp <= ' '; bp++);
	if (UNLIKELY(bp >= ep)) {
		/* couldn't finish this */
		return sp;
	} else if (*bp++ != '.') {
		/* error, need a DOT */
		return NULL;
	}
	/* otherwise we're good to go */
	return bp;
}

static trans_t
parse_trans(ttl_term_t *tt, const char **tp, const char *bp, const char *const ep)
{
	const char *const sp = bp;
	trans_t r;

	tt->typ = TTL_TYP_UNK;
	switch (*bp) {
	case '@':
		/* directive */
		if (UNLIKELY((bp = _parse_dir(&tt->dir, sp, ep)) == NULL)) {
			return TRANS_ERR;
		} else if (UNLIKELY(bp == sp || bp > ep)) {
			goto rollback;
		}
		tt->typ = TTL_TYP_DIR;
		r = TRANS_DIR;
		break;
	case '.':
		r = TRANS_DOT;
		bp++;
		break;
	case ',':
		r = TRANS_COM;
		bp++;
		break;
	case ';':
		r = TRANS_SEM;
		bp++;
		break;
	case '<':
		/* iri */
		if (UNLIKELY((bp = _parse_iri(&tt->iri, sp, ep)) == NULL)) {
			return TRANS_ERR;
		} else if (UNLIKELY(bp == sp || bp >= ep)) {
			goto rollback;
		}
		tt->typ = TTL_TYP_IRI;
		r = TRANS_IRI;
		break;
	case '[':
		/* blank */
		r = TRANS_SQO;
		bp++;
		break;
	case ']':
		r = TRANS_SQC;
		bp++;
		break;
	case '(':
		/* collection */
	case ')':
		/* unsupported at the moment */
		r = TRANS_ERR;
		break;
	case '"':
	case '\'':
		/* literal */
		if (UNLIKELY((bp = _parse_lit(&tt->lit, sp, ep)) == NULL)) {
			return TRANS_ERR;
		} else if (UNLIKELY(bp == sp || bp >= ep)) {
			goto rollback;
		}
		tt->typ = TTL_TYP_LIT;
		r = TRANS_LIT;
		break;
	case '{':
		r = TRANS_BRO;
		bp++;
		break;
	case '}':
		r = TRANS_BRC;
		bp++;
		break;
	default:
		/* something */
		if (UNLIKELY((bp = parse_qname(&tt->iri, sp, ep)) == NULL)) {
			return TRANS_ERR;
		} else if (UNLIKELY(bp == sp || bp >= ep)) {
			goto rollback;
		}
		tt->typ = TTL_TYP_IRI;
		r = TRANS_IRI;
		break;

	rollback:
		/* no progress */
		*tp = sp;
		return TRANS_UNK;
	}
	*tp = bp;
	return r;
}


/* type opers */
static __attribute__((pure, const)) size_t
_strlen(ttl_str_t x)
{
	return x.len;
}

static size_t
_strcpy(char *b, ttl_str_t *x)
{
	if (UNLIKELY(x->str == b)) {
		/* moved already */
		;
	} else if (UNLIKELY(x->str >= b && x->str <= b + x->len)) {
		/* careful */
		memmove(b, x->str, x->len);
	} else {
		memcpy(b, x->str, x->len);
	}
	x->str = b;
	return x->len;
}

static __attribute__((pure, const)) size_t
irilen(ttl_iri_t x)
{
	size_t len = 0U;
	len += _strlen(x.pre);
	len += _strlen(x.val);
	return len;
}

static size_t
iricpy(char *b, ttl_iri_t *x)
{
	size_t len = 0U;

	len += _strcpy(b + len, &x->pre);
	len += _strcpy(b + len, &x->val);
	return len;
}

static size_t
litlen(ttl_lit_t x)
{
	size_t len = 0U;
	len += x.val.len;
	len += irilen(x.typ);
	len += x.lng.len;
	return len;
}

static size_t
litcpy(char *b, ttl_lit_t *x)
{
	size_t len = 0U;

	len += _strcpy(b + len, &x->val);
	len += iricpy(b + len, &x->typ);
	len += _strcpy(b + len, &x->lng);
	return len;
}


/* buffer stuff */
static size_t
termlen(ttl_term_t tt)
{
	switch (tt.typ) {
	case TTL_TYP_IRI:
		return irilen(tt.iri);
	case TTL_TYP_LIT:
		return litlen(tt.lit);
	default:
		break;
	}
	return 0U;
}

static size_t
termcpy(char *b, ttl_term_t *tt)
{
	switch (tt->typ) {
	case TTL_TYP_IRI:
		return iricpy(b, &tt->iri);
	case TTL_TYP_LIT:
		return litcpy(b, &tt->lit);
	default:
		break;
	}
	return 0U;
}

static size_t
_sbrkmv(struct _parser_s *pp)
{
/* move the statement into expander space
 * for simple states the state itself mandates what to save:
 * STATE_0 -> {}
 * STATE_S -> {0}
 * STATE_P -> {0,1}
 * STATE_O -> {0,1,2}
 * STATE_Q -> {0,1,2,3}
 * STATE_G -> {3}
 * STATE_GS -> {0,3}
 * STATE_GP -> {0,1,3}
 * STATE_GO -> {0,1,2,3}
 * STATE_BS -> {0}
 * STATE_BP -> {0,1}
 * STATE_BO -> {0,1,2}
 */
	size_t tot = 0U;

	/* save everything in the bottom of the stack */
	for (size_t j = 0U; j < pp->S; j++) {
		for (size_t i = TTL_SUBJ; i <= TTL_GRPH; i++) {
			tot += termlen(pp->s[j].stmt[i]);
		}
	}
	if (pp->s[pp->S].state < STATE_G) {
		for (size_t i = 0; i < pp->s[pp->S].state; i++) {
			tot += termlen(pp->s[pp->S].stmt[i]);
		}
	} else if (pp->s[pp->S].state < STATE_BS) {
		tot += termlen(pp->s[pp->S].stmt[TTL_GRPH]);
		for (size_t i = 0; i < pp->s[pp->S].state - STATE_G; i++) {
			tot += termlen(pp->s[pp->S].stmt[i]);
		}
	} else {
		for (size_t i = 0; i <= pp->s[pp->S].state - STATE_BS; i++) {
			tot += termlen(pp->s[pp->S].stmt[i]);
		}
	}

	if (UNLIKELY(pp->x.n + tot >= pp->x.z)) {
		while ((pp->x.z *= 2U) < pp->x.n + tot);
		pp->x.b = realloc(pp->x.b, pp->x.z);
	}

	for (size_t j = tot = 0U; j < pp->S; j++) {
		for (size_t i = TTL_SUBJ; i <= TTL_GRPH; i++) {
			tot += termcpy(pp->x.b + tot, &pp->s[j].stmt[i]);
		}
	}
	if (pp->s[pp->S].state < STATE_G) {
		for (size_t i = 0; i < pp->s[pp->S].state; i++) {
			tot += termcpy(pp->x.b + tot, &pp->s[pp->S].stmt[i]);
		}
	} else if (pp->s[pp->S].state < STATE_BS) {
		tot += termcpy(pp->x.b + tot, &pp->s[pp->S].stmt[TTL_GRPH]);
		for (size_t i = 0; i < pp->s[pp->S].state - STATE_G; i++) {
			tot += termcpy(pp->x.b + tot, &pp->s[pp->S].stmt[i]);
		}
	} else {
		for (size_t i = 0; i <= pp->s[pp->S].state - STATE_BS; i++) {
			tot += termcpy(pp->x.b + tot, &pp->s[pp->S].stmt[i]);
		}
	}
	return tot;
}

static size_t
_append(struct _parser_s *pp, const char *str, size_t len)
{
	char *const buf = pp->p.b, *const eob = pp->p.b + pp->p.z;

	if (UNLIKELY(len >= pp->p.z)) {
		while ((pp->p.z *= 2U) < len);
		pp->p.b = realloc(pp->p.b, pp->p.z);
	}
	if (buf == str) {
		/* do nothing */
		;
	} else {
		if (LIKELY(buf < str && str < eob)) {
			/* there could have been the realloc, rebase str */
			str += pp->p.b - buf;
		}
		memmove(pp->p.b, str, len);
	}
	return len;
}


ttl_parser_t*
ttl_make_parser(void)
{
	struct _parser_s *r = malloc(sizeof(*r));
	if (UNLIKELY(r == NULL)) {
		return NULL;
	}
	r->p.b = malloc(r->p.z = 4096U);
	r->x.b = malloc(r->x.z = 4096U);
	r->p.n = r->x.n = 0U;
	r->b = 1U;
	r->S = 0U;
	r->s = malloc((r->Z = 4U) * sizeof(*r->s));
	r->s[r->S].state = STATE_0;
	r->s[r->S].stmt[TTL_GRPH] = (ttl_term_t){};
	return &r->public;
}

void
ttl_free_parser(ttl_parser_t *p)
{
	struct _parser_s *pp = (void*)p;

	free(pp->s);
	free(pp->p.b);
	free(pp->x.b);
	free(p);
	return;
}

int
ttl_parse_chunk(ttl_parser_t *p, const char *buf, size_t len)
{
	struct _parser_s *pp = (void*)p;
	/* pointer we mustn't exceed */
	const char *ep;
	/* current buffer pointer */
	const char *bp;
	/* pointer after one transition */
	const char *tp;
	ttl_term_t tt;

rfll:
	if (pp->p.n) {
		/* fill him up */
		const size_t lesser = min_z(len, pp->p.z - pp->p.n);
		memcpy(pp->p.b + pp->p.n, buf, lesser);
		bp = pp->p.b;
		ep = pp->p.b + pp->p.n + lesser;
		buf += lesser;
		len -= lesser;
	} else {
		bp = buf;
		ep = buf += len;
		len = 0U;
	}
more:
	for (; bp < ep && (unsigned char)*bp <= ' '; bp++);
	if (UNLIKELY(bp >= ep)) {
		tp = ep;
		goto trans_unk;
	}
	switch (parse_trans(&tt, &tp, bp, ep)) {
	case TRANS_ERR:
		return -1;
	case TRANS_UNK:
	trans_unk:
		pp->x.n = _sbrkmv(pp);
		pp->p.n = _append(pp, tp, ep - tp);

		if (len) {
			/* refill */
			goto rfll;
		}
		return 0;
	case TRANS_DIR:
		switch (pp->s[pp->S].state) {
		case STATE_0:
			/* 0 + DIR -> 0 */
			break;
		case STATE_G:
			/* G + DIR -> G */
			break;
		default:
			return -1;
		}
		if (pp->public.hdl.decl) {
			pp->public.hdl.decl(pp->public.usr, tt.iri);
		}
		break;
	case TRANS_IRI:
		switch (pp->s[pp->S].state) {
			state_t b;
		case STATE_0:
			/* 0 + IRI -> S */
		case STATE_S:
			/* S + IRI -> P */
		case STATE_P:
			/* P + IRI -> O */
		case STATE_O:
			/* O + IRI -> Q */
			b = STATE_0;
			goto bang_iri;
		case STATE_G:
			/* G + IRI -> GS */
		case STATE_GS:
			/* GS + IRI -> GP */
		case STATE_GP:
			/* GP + IRI -> GO */
			b = STATE_G;
			goto bang_iri;
		case STATE_BS:
			/* BS + IRI -> BP */
		case STATE_BP:
			/* BP + IRI -> BO */
			b = STATE_BS;
			b--;
			goto bang_iri;
		bang_iri:
			pp->s[pp->S].stmt[pp->s[pp->S].state++ - b] = tt;
			break;
		default:
			return -1;
		}
		break;
	case TRANS_LIT:
		switch (pp->s[pp->S].state) {
		case STATE_P:
			/* P + LIT -> O */
		case STATE_GP:
			/* GP + LIT -> GO */
		case STATE_BP:
			/* BP + LIT -> BO */
			pp->s[pp->S].stmt[TTL_OBJ] = tt;
			pp->s[pp->S].state++;
			break;
		default:
			return -1;
		}
		break;
	case TRANS_BRO:
		switch (pp->s[pp->S].state) {
		case STATE_S:
			/* S + { -> G */
			pp->s[pp->S].stmt[TTL_GRPH] =
				pp->s[pp->S].stmt[TTL_SUBJ];
			pp->s[pp->S].state = STATE_G;
			break;
		default:
			return -1;
		}
		break;
	case TRANS_BRC:
		switch (pp->s[pp->S].state) {
		case STATE_G:
			/* G + } -> 0 */
		case STATE_GO:
			/* GO + } -> 0 */
			pp->s[pp->S].stmt[TTL_GRPH] = (ttl_term_t){};
			pp->s[pp->S].state = STATE_0;
			break;
		default:
			return -1;
		}
		break;
	case TRANS_SQO:
		switch (pp->s[pp->S].state) {
		case STATE_P:
			/* P + [ -> BS */
		case STATE_GP:
			/* GP + [ -> BS */
		case STATE_BP:
			/* BP + [ -> BS */
			pp->s[pp->S].stmt[TTL_OBJ] =
				(ttl_term_t){TTL_TYP_BLA, .bla = {{pp->b++}}};
			pp->s[pp->S].state++;
			/* push */
			if (UNLIKELY(++pp->S >= pp->Z)) {
				pp->Z *= 2U;
				pp->s = realloc(pp->s, pp->Z * sizeof(*pp->s));
			}
			pp->s[pp->S].stmt[TTL_SUBJ] =
				pp->s[pp->S - 1U].stmt[TTL_OBJ];
			pp->s[pp->S].stmt[TTL_GRPH] =
				pp->s[pp->S - 1U].stmt[TTL_GRPH];
			pp->s[pp->S].state = STATE_BS;
			break;
		default:
			return -1;
		}
		break;
	case TRANS_SQC:
		switch (pp->s[pp->S].state) {
		case STATE_BO:
			/* BO + ] -> pop */

			/* issue statement, as ] implies TRANS_DOT */
			if (pp->public.hdl.stmt) {
				pp->public.hdl.stmt(
					pp->public.usr, pp->s[pp->S].stmt);
			}
			/* do the popping */
		case STATE_BS:
			/* BS + ] -> pop */
			pp->S--;
			break;
		default:
			return -1;
		}
		break;
	case TRANS_DOT:
		switch (pp->s[pp->S].state) {
		case STATE_O:
			/* O + . -> 0 */
		case STATE_Q:
			/* Q + . -> 0 */
			pp->s[pp->S].state = STATE_0;
			break;
		case STATE_GO:
			/* GO + . -> G */
			pp->s[pp->S].state = STATE_G;
			break;
		default:
			return -1;
		}
		goto stmt;
	case TRANS_COM:
		switch (pp->s[pp->S].state) {
		case STATE_O:
			/* O + , -> P */
		case STATE_GO:
			/* GO + , -> GP */
		case STATE_BO:
			/* BO + , -> BP */
			pp->s[pp->S].state--;
			break;
		default:
			return -1;
		}
		goto stmt;
	case TRANS_SEM:
		switch (pp->s[pp->S].state) {
		case STATE_O:
			/* O  + ; -> S */
		case STATE_GO:
			/* GO + ; -> GS */
		case STATE_BO:
			/* BO + ; -> BS */
			pp->s[pp->S].state--;
			pp->s[pp->S].state--;
			break;
		default:
			return -1;
		}
		goto stmt;

	stmt:
		if (pp->public.hdl.stmt) {
			pp->public.hdl.stmt(pp->public.usr, pp->s[pp->S].stmt);
		}
		if (!pp->s[pp->S].state) {
			pp->s[pp->S].stmt[TTL_GRPH] = (ttl_term_t){};
		}
		break;		
	default:
		abort();
	}
	bp = tp;
	goto more;
}

/* ttl.c ends here */

/*** ttl.c - trig/turtle/ntriples/nquads reader */
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
} state_t;

typedef enum {
	TRANS_ERR = -1,
	TRANS_UNK = 0,
	TRANS_DIR,
	TRANS_IRI,
	TRANS_LIT,
	TRANS_BRO,
	TRANS_BRC,
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
	ttl_parser_t public;
	state_t state;
	/* parser buffer */
	buf_t p;
	/* expander buffer */
	buf_t x;
	/* statement, room for a quad */
	ttl_term_t stmt[4U];
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

	switch (*bp) {
	case '"':
		if (ep > ++bp && *bp == '"') {
			/* could be """ or "" */
			if (ep > ++bp && *bp == '"') {
				/* aah triple-quotes */
				bp++;
				do {
					while (bp < ep &&
					       (*bp == '\\' && (bp += 2U, 1) ||
						*bp++ != '"'));
				} while (bp < ep && *bp++ != '"' ||
					 bp < ep && *bp++ != '"');
			}
		} else {
			while (bp < ep &&
			       (*bp == '\\' && (bp += 2U, 1) || *bp++ != '"'));
		}
		break;
	case '\'':
		if (ep > ++bp && *bp == '\'') {
			/* could be ''' or '' */
			if (ep > ++bp && *bp == '\'') {
				/* aah triple-quotes */
				bp++;
				do {
					while (bp < ep &&
					       (*bp == '\\' && (bp += 2U, 1) ||
						*bp++ != '\''));
				} while (bp < ep && *bp++ != '\'' ||
					 bp < ep && *bp++ != '\'');
			}
		} else {
			while (bp < ep &&
			       (*bp == '\\' && (bp += 2U, 1) || *bp++ != '\''));
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
	tt->val = (ttl_str_t){sp, bp - sp};
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
parse_blank(const char *bp, const char *const ep)
{
	const char *const sp = bp;

	if (UNLIKELY(*bp++ != '[')) {
		/* not a blank */
		return NULL;
	}
	while (bp < ep && *bp++ != ']');
	return bp < ep ? bp : sp;
}

static const char*
parse_collection(const char *bp, const char *const ep)
{
	const char *const sp = bp;

	if (UNLIKELY(*bp++ != '(')) {
		/* not a blank */
		return NULL;
	}
	while (bp < ep && *bp++ != ')');
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
		if (UNLIKELY((bp = parse_blank(sp, ep)) == NULL)) {
			return TRANS_ERR;
		} else if (UNLIKELY(bp == sp || bp >= ep)) {
			goto rollback;
		}
	case ']':
		/* unsupported at the moment */
		r = TRANS_ERR;
		break;
	case '(':
		/* collection */
		if (UNLIKELY((bp = parse_collection(sp, ep)) == NULL)) {
			return TRANS_ERR;
		} else if (UNLIKELY(bp == sp || bp >= ep)) {
			goto rollback;
		}
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
/* move the statement into expander space */
	const state_t base = pp->state >= STATE_G ? STATE_G : STATE_0;
	size_t tot = 0U;

	for (size_t i = 0; i < pp->state - base; i++) {
		tot += termlen(pp->stmt[i]);
	}
	if (UNLIKELY(pp->x.n + tot >= pp->x.z)) {
		while ((pp->x.z *= 2U) < pp->x.n + tot);
		pp->x.b = realloc(pp->x.b, pp->x.z);
	}
	for (size_t i = tot = 0; i < pp->state - base; i++) {
		tot += termcpy(pp->x.b + tot, &pp->stmt[i]);
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
	r->state = STATE_0;
	r->stmt[TTL_GRPH] = (ttl_term_t){};
	return &r->public;
}

void
ttl_free_parser(ttl_parser_t *p)
{
	struct _parser_s *pp = (void*)p;
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
		switch (pp->state) {
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
		switch (pp->state) {
		case STATE_0:
			/* 0 + IRI -> S */
		case STATE_S:
			/* S + IRI -> P */
		case STATE_P:
			/* P + IRI -> O */
		case STATE_O:
			/* O + IRI -> Q */
			pp->stmt[pp->state++] = tt;
			break;
		case STATE_G:
			/* G + IRI -> GS */
		case STATE_GS:
			/* GS + IRI -> GP */
		case STATE_GP:
			/* GP + IRI -> GO */
			pp->stmt[pp->state++ - STATE_G] = tt;
			break;
		default:
			return -1;
		}
		break;
	case TRANS_LIT:
		switch (pp->state) {
		case STATE_P:
			/* P + LIT -> O */
			pp->stmt[pp->state++] = tt;
			break;
		case STATE_GP:
			/* GP + LIT -> GO */
			pp->stmt[pp->state++ - STATE_G] = tt;
			break;
		default:
			return -1;
		}
		break;
	case TRANS_BRO:
		switch (pp->state) {
		case STATE_S:
			/* S + { -> G */
			pp->stmt[TTL_GRPH] = pp->stmt[TTL_SUBJ];
			pp->state = STATE_G;
			break;
		default:
			return -1;
		}
		break;
	case TRANS_BRC:
		switch (pp->state) {
		case STATE_G:
			/* G + } -> 0 */
		case STATE_GO:
			/* GO + } -> 0 */
			pp->stmt[TTL_GRPH] = (ttl_term_t){};
			pp->state = STATE_0;
			break;
		default:
			return -1;
		}
		break;
	case TRANS_DOT:
		switch (pp->state) {
		case STATE_O:
			/* O + . -> 0 */
		case STATE_Q:
			/* Q + . -> 0 */
			pp->state = STATE_0;
			break;
		case STATE_GO:
			/* GO + . -> G */
			pp->state = STATE_G;
			break;
		default:
			return -1;
		}
		goto stmt;
	case TRANS_COM:
		switch (pp->state) {
		case STATE_O:
			/* O + , -> P */
		case STATE_GO:
			/* GO + , -> GP */
			pp->state--;
			break;
		default:
			return -1;
		}
		goto stmt;
	case TRANS_SEM:
		switch (pp->state) {
		case STATE_O:
			/* O  + ; -> S */
		case STATE_GO:
			/* GO + ; -> GS */
			pp->state--;
			pp->state--;
			break;
		default:
			return -1;
		}
		goto stmt;

	stmt:
		if (pp->public.hdl.stmt) {
			pp->public.hdl.stmt(pp->public.usr, pp->stmt);
		}
		break;		
	default:
		abort();
	}
	bp = tp;
	goto more;
}

/* ttl.c ends here */

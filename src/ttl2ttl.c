/*** ttl2ttl.c - trig/turtle/ntriples/nquads reader
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
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>
#include <errno.h>
#include "ttl.h"
#include "nifty.h"

static unsigned int iri_xpnd = 1U;
static unsigned int iri_xgen = 1U;
static unsigned int sortable = 0U;

struct _writer_s {
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


static void
fwrite_iri(struct _writer_s *w, ttl_iri_t t, void *stream)
{
	if (UNLIKELY(!t.pre.len && t.val.len == 1U && *t.val.str == 'a')) {
		fputc('a', stream);
	} else if (t.pre.str) {
		ttl_str_t x;

		if (UNLIKELY(iri_xpnd) &&
		    LIKELY((x = ttl_decl_get(w->d, t.pre)).len)) {
			fputc('<', stream);
			fwrite(x.str, 1, x.len, stream);
			fwrite(t.val.str, 1, t.val.len, stream);
			fputc('>', stream);
		} else if (UNLIKELY(iri_xgen) &&
			   LIKELY((x = ttl_decl_get(w->d, t.pre)).len) &&
			   t.pre.str[0U] == 'n' && t.pre.str[1U] == 's' &&
			   t.pre.str[2U] >= '1' && t.pre.str[2U] <= '9') {
			fputc('<', stream);
			fwrite(x.str, 1, x.len, stream);
			fwrite(t.val.str, 1, t.val.len, stream);
			fputc('>', stream);
		} else {
			fwrite(t.pre.str, 1, t.pre.len, stream);
			fputc(':', stream);
			fwrite(t.val.str, 1, t.val.len, stream);
		}
	} else {
		fputc('<', stream);
		fwrite(t.val.str, 1, t.val.len, stream);
		fputc('>', stream);
	}
	return;
}

static void
fwrite_lit(struct _writer_s *w, ttl_lit_t t, void *stream)
{
	size_t i = 1U;

	if (UNLIKELY(sortable)) {
		t.val = ttl_enquot_str(w->c, t.val, TTL_QUOT_PRNT ^ TTL_QUOT_CTRL);
	}

	i -= t.val.str[0 - i] <= ' ';
	i += t.val.str[0 - i] == t.val.str[0 - i - 1];
	i += t.val.str[0 - i] == t.val.str[0 - i - 1];

	fwrite(t.val.str - i, 1, i, stream);
	fwrite(t.val.str, 1, t.val.len, stream);
	fwrite(t.val.str - i, 1, i, stream);
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
		fprintf(stream, "_:b%016lx", t.bla.h[0U]);
		break;
	default:
		break;
	}
	return;
}

static bool
irieqp(struct _writer_s *w, ttl_iri_t i1, ttl_iri_t i2)
{
	if (i1.pre.len) {
		ttl_str_t x = ttl_decl_get(w->d, i1.pre);

		if (UNLIKELY(!x.len)) {
			x = (ttl_str_t){i1.pre.str, i1.pre.len};
		}
		return x.len <= i2.val.len &&
			!memcmp(x.str, i2.val.str, x.len) &&
			x.len + i1.val.len == i2.val.len &&
			!memcmp(i1.val.str, i2.val.str + x.len, i1.val.len);
	} else if (i1.val.len > 1U || *i1.val.str != 'a') {
		return i1.val.len == i2.val.len &&
			!memcmp(i1.val.str, i2.val.str, i1.val.len);
	}
	return *i1.val.str == *i2.val.str;
}

static bool
termeqp(struct _writer_s *w, ttl_term_t t1, ttl_term_t t2)
{
	return t1.typ == t2.typ &&
		(t1.typ == TTL_TYP_IRI && irieqp(w, t1.iri, t2.iri) ||
		 t1.typ == TTL_TYP_BLA && t1.bla.h[0U] == t2.bla.h[0U]);
}

static bool
tblaeqp(struct _writer_s *w, ttl_term_t t1, ttl_term_t t2)
{
	(void)w;
	return t1.typ == t2.typ &&
		t1.typ == TTL_TYP_BLA &&
		t1.bla.h[0U] == t2.bla.h[0U];
}


static size_t zbuf[2U];
static char *lbuf[2U];
static ttl_term_t last[3U];

static ttl_iri_t
clon_iri(struct _writer_s *w, ttl_iri_t i, unsigned int which)
{
	size_t ilen = i.val.len;
	ttl_str_t x = {.len = 0U};

	if (i.pre.len) {
		x = ttl_decl_get(w->d, i.pre);
		ilen += x.len ?: i.pre.len;
	}

	if (UNLIKELY(ilen > zbuf[which])) {
		size_t nu;
		for (nu = 2U * zbuf[which]; nu <= ilen; nu *= 2U);
		lbuf[which] = realloc(lbuf[which], countof(lbuf) * nu);
		zbuf[which] = nu;
	}
	if (x.len) {
		memcpy(lbuf[which], x.str, x.len);
	} else if (i.pre.len) {
		memcpy(lbuf[which], i.pre.str, x.len = i.pre.len);
	}
	memcpy(lbuf[which] + x.len, i.val.str, i.val.len);
	return (ttl_iri_t){{lbuf[which], ilen}};
}

static ttl_term_t
clon(struct _writer_s *w, ttl_term_t t, unsigned int which)
{
	switch (t.typ) {
	case TTL_TYP_IRI:
		return (ttl_term_t){TTL_TYP_IRI, clon_iri(w, t.iri, which)};
	case TTL_TYP_BLA:
		return t;
	default:
		break;
	}
	return (ttl_term_t){};
}

static ttl_term_t
cbla(ttl_term_t t, size_t where)
{
	return (ttl_term_t){TTL_TYP_BLA, .bla = {t.bla.h[0U], where}};
}


static void
decl(void *usr, ttl_iri_t decl)
{
	struct _writer_s *w = usr;

	ttl_decl_put(w->d, decl.pre, decl.val);
	if (!iri_xpnd) {
		if (last[TTL_SUBJ].typ) {
			fputc('.', stdout);
			fputc('\n', stdout);
			last[TTL_SUBJ] = (ttl_term_t){};
		}
		fwrite("@prefix ", 1, 8U, stdout);
		fwrite(decl.pre.str, 1, decl.pre.len, stdout);
		fputc(':', stdout);
		fputc(' ', stdout);
		fputc('<', stdout);
		fwrite(decl.val.str, 1, decl.val.len, stdout);
		fputc('>', stdout);
		fputc(' ', stdout);
		fputc('.', stdout);
		fputc('\n', stdout);
	}
	return;
}

static void
stmt_old(void *usr, const ttl_stmt_t *stmt, size_t where)
{
	struct _writer_s *w = usr;

	if (UNLIKELY(stmt == NULL)) {
		/* last statement */
		if (last[TTL_SUBJ].typ) {
			fputc('.', stdout);
			fputc('\n', stdout);
		}
		last[TTL_SUBJ] = (ttl_term_t){};
	} else if (!termeqp(w, stmt[where].subj, last[TTL_SUBJ])) {
		if (last[TTL_SUBJ].typ) {
			fputc('.', stdout);
			fputc('\n', stdout);
		}
		fputc('\n', stdout);
		fwrite_term(w, stmt[where].subj, stdout);
		fputc('\n', stdout);
		fputc('\t', stdout);
		fwrite_term(w, stmt[where].pred, stdout);
		fputc('\t', stdout);
		fwrite_term(w, stmt[where].obj, stdout);
		fputc(' ', stdout);
		last[TTL_SUBJ] = clon(w, stmt[where].subj, TTL_SUBJ);
		last[TTL_PRED] = (ttl_term_t){};
	} else if (!termeqp(w, stmt[where].pred, last[TTL_PRED])) {
		fputc(';', stdout);
		fputc('\n', stdout);
		fputc('\t', stdout);
		fwrite_term(w, stmt[where].pred, stdout);
		fputc('\t', stdout);
		fwrite_term(w, stmt[where].obj, stdout);
		fputc(' ', stdout);
		last[TTL_PRED] = clon(w, stmt[where].pred, TTL_PRED);
	} else {
		fputc(',', stdout);
		fputc(' ', stdout);
		fwrite_term(w, stmt[where].obj, stdout);
		fputc(' ', stdout);
	}
	return;
}

static void
stmt_x(void *usr, const ttl_stmt_t *stmt, size_t where)
{
	struct _writer_s *w = usr;
	static size_t j;

	if (UNLIKELY(j > where)) {
		for (; j > where; j--) {
			fputc(';', stdout);
			fputc('\n', stdout);
			for (size_t i = 0U; i < j; i++) {
				fputc('\t', stdout);
			}
			fputc(']', stdout);
			fputc(' ', stdout);
		}
		last[TTL_SUBJ] = clon(w, stmt[j].subj, TTL_SUBJ);
		last[TTL_PRED] = clon(w, stmt[j].pred, TTL_PRED);
		last[TTL_OBJ] = (ttl_term_t){};
		if (LIKELY(stmt != NULL)) {
			return;
		}
	}
	if (UNLIKELY(stmt == NULL)) {
		/* last statement */
		if (last[TTL_SUBJ].typ) {
			fputc('.', stdout);
			fputc('\n', stdout);
		}
		last[TTL_SUBJ] = (ttl_term_t){};
	} else if (!termeqp(w, stmt[j].subj, last[TTL_SUBJ])) {
		if (last[TTL_SUBJ].typ) {
			fputc('.', stdout);
			fputc('\n', stdout);
		}
		fputc('\n', stdout);
		fwrite_term(w, stmt[j].subj, stdout);
		fputc('\n', stdout);
		fputc('\t', stdout);
		for (size_t i = 0U; i < j; i++) {
			fputc('\t', stdout);
		}
		fwrite_term(w, stmt[j].pred, stdout);
		fputc('\t', stdout);
		if (LIKELY(stmt[j].obj.typ != TTL_TYP_BLA)) {
			fwrite_term(w, stmt[j].obj, stdout);
			fputc(' ', stdout);
		} else {
			goto blank;
		}
		last[TTL_SUBJ] = clon(w, stmt[j].subj, TTL_SUBJ);
		last[TTL_PRED] = (ttl_term_t){};
	} else if (!termeqp(w, stmt[j].pred, last[TTL_PRED])) {
		fputc(';', stdout);
		fputc('\n', stdout);
		fputc('\t', stdout);
		for (size_t i = 0U; i < j; i++) {
			fputc('\t', stdout);
		}
		fwrite_term(w, stmt[j].pred, stdout);
		fputc('\t', stdout);
		if (LIKELY(stmt[j].obj.typ != TTL_TYP_BLA)) {
			fwrite_term(w, stmt[j].obj, stdout);
			fputc(' ', stdout);
		} else {
			goto blank;
		}
		last[TTL_PRED] = clon(w, stmt[j].pred, TTL_PRED);
		last[TTL_OBJ] = cbla(stmt[j].obj, where);
	} else {
		fputc(',', stdout);
		fputc(' ', stdout);
		if (LIKELY(stmt[j].obj.typ != TTL_TYP_BLA)) {
			fwrite_term(w, stmt[j].obj, stdout);
			fputc(' ', stdout);
		} else {
			goto blank;
		}
	}
	return;
blank:
	fputc('[', stdout);
	fputc('\n', stdout);
	last[TTL_OBJ] = cbla(stmt[j].obj, where);
	/* ascend */
	j++;
	fputc('\t', stdout);
	for (size_t i = 0U; i < j; i++) {
		fputc('\t', stdout);
	}
	fwrite_term(w, stmt[j].pred, stdout);
	fputc('\t', stdout);
	if (LIKELY(stmt[j].obj.typ != TTL_TYP_BLA)) {
		fwrite_term(w, stmt[j].obj, stdout);
		fputc(' ', stdout);
	} else {
		goto blank;
	}
	last[TTL_SUBJ] = clon(w, stmt[j].subj, TTL_SUBJ);
	last[TTL_PRED] = clon(w, stmt[j].pred, TTL_PRED);
	last[TTL_OBJ] = cbla(stmt[j].obj, where);
	return;
}

static void
stnt(void *usr, const ttl_stmt_t *stmt, size_t where)
{
	struct _writer_s *w = usr;

	if (UNLIKELY(stmt == NULL)) {
		;
	} else if (UNLIKELY(tblaeqp(w, stmt[where].obj, last[TTL_OBJ]) &&
			    last[TTL_OBJ].bla.h[1U] >= where + 1U)) {
		/* we're finished printing the blank node, go deeper */
		last[2U] = stmt[where].subj;
		last[2U].bla.h[1U] = where;
	} else {
		if (last[TTL_SUBJ].typ) {
			goto dot;
		}
		fwrite_term(w, stmt[0U].subj, stdout);
		fputc('\t', stdout);
		fwrite_term(w, stmt[0U].pred, stdout);
		fputc('\t', stdout);
		fwrite_term(w, stmt[0U].obj, stdout);
		for (size_t i = 1U; i <= where; i++) {
			fputc(' ', stdout);
			fputc('.', stdout);
			fwrite_term(w, stmt[i].subj, stdout);
			fputc('\t', stdout);
			fwrite_term(w, stmt[i].pred, stdout);
			fputc('\t', stdout);
			fwrite_term(w, stmt[i].obj, stdout);
		}
		if (UNLIKELY(where > last[TTL_OBJ].bla.h[1U])) {
			last[2U] = stmt[where].subj;
			last[2U].bla.h[1U] = where;
		}
		fputc(' ', stdout);
	dot:
		fputc('.', stdout);
		fputc('\n', stdout);
	}
	return;
}

static struct _writer_s
make_writer(void)
{
	return (struct _writer_s){.c = ttl_make_codec(), .d = ttl_make_decl()};
}

static void
free_writer(struct _writer_s w)
{
	ttl_free_codec(w.c);
	ttl_free_decl(w.d);
	return;
}


#include "ttl2ttl.yucc"

int
main(int argc, char *argv[])
{
	static char buf[16U * 4096U];
	static yuck_t argi[1U];
	struct _writer_s w;
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
	}

	iri_xpnd = argi->expand_flag;
	iri_xgen = argi->expand_generic_flag;
	sortable = argi->sortable_flag;

	w = make_writer();
	if (w.c == NULL || w.d == NULL) {
		error("\
Error: cannot instantiate nt writer");
		rc = 1;
		goto out;
	}
	/* instantiate last buffer */
	for (size_t i = 0U; i < countof(lbuf); i++) {
		lbuf[i] = malloc(zbuf[i] = 128U);
		if (lbuf[i] == NULL) {
			error("\
Error: cannot instantiate buffer for previous statement");
			rc = 1;
			goto out;
		}
	}

	p->hdl = (ttl_handler_t){decl, !sortable ? stmt_x : stnt};
	p->usr = &w;

	for (size_t i = 0U; i < argi->nargs + (!argi->nargs); i++) {
		const char *fn = argi->args[i];
		int fd;

		if (!fn) {
			fd = STDIN_FILENO;
			fn = "(stdin)";
		} else if ((fd = open(fn, O_RDONLY)) < 0) {
			error("\
Error: cannot open file `%s'", fn);
			continue;
		}
		/* otherwise read chunks thereof */
		for (ssize_t nrd; (nrd = read(fd, buf, sizeof(buf))) > 0;) {
			if (ttl_parse_chunk(p, buf, nrd) < 0) {
				errno = 0, error("\
Error: cannot parse `%s'", fn);
				break;
			}
		}
		/* give us closure */
		close(fd);
		p->hdl.stmt(&w, NULL, 0U);
	}

out:
	ttl_free_parser(p);
	for (size_t i = 0U; i < countof(lbuf); i++) {
		free(lbuf[i]);
	}
	free_writer(w);
	yuck_free(argi);
	return rc;
}

/* ttl2ttl.c ends here */

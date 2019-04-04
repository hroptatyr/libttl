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
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include "ttl.h"
#include "nifty.h"

static unsigned int esc_utf8;
static unsigned int esc_ctrl = 1U;

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
		static const char a[] =
			"<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>";
		fwrite(a, 1, countof(a), stream);
	} else if (t.pre.str) {
		ttl_str_t x = ttl_decl_get(w->d, t.pre);

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
	} else {
		fputc('<', stdout);
		fwrite(t.val.str, 1, t.val.len, stream);
		fputc('>', stdout);
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

	ttl_decl_put(w->d, decl.pre, decl.val);
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


#include "ttl2nt.yucc"

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

	w = make_writer();
	if (w.c == NULL || w.d == NULL) {
		error("\
Error: cannot instantiate nt writer");
		rc = 1;
		goto out;
	}

	p->hdl = (ttl_handler_t){decl, stmt};
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
	}

out:
	ttl_free_parser(p);
	free_writer(w);
	yuck_free(argi);
	return rc;
}

/* ttl2nt.c ends here */

/*** ttl.c - trig/turtle/ntriples/nquads reader */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif	/* HAVE_CONFIG_H */
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include "ttl.h"
#include "nifty.h"

static void
fwrite_iri(ttl_iri_t t, void *stream)
{
	fwrite(t.pre.str, 1, t.pre.len, stream);
	fwrite(t.val.str, 1, t.val.len, stream);
	return;
}

static void
fwrite_lit(ttl_lit_t t, void *stream)
{
	fwrite(t.val.str, 1, t.val.len, stream);
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
fwrite_term(ttl_term_t t, void *stream)
{
	switch (t.typ) {
	case TTL_TYP_IRI:
		fwrite_iri(t.iri, stream);
		break;
	case TTL_TYP_LIT:
		fwrite_lit(t.lit, stream);
		break;
	default:
		break;
	}
	return;
}

static void
decl(void *usr, ttl_iri_t decl)
{
	fputs("DECL\t", stdout);
	fwrite_iri(decl, stdout);
	fputc('\n', stdout);
	return;
}

static void
stmt(void *usr, const ttl_term_t stmt[static 4U])
{
	fputs("STMT\t", stdout);
	fwrite_term(stmt[TTL_SUBJ], stdout);
	fputc(' ', stdout);
	fwrite_term(stmt[TTL_PRED], stdout);
	fputc(' ', stdout);
	fwrite_term(stmt[TTL_OBJ], stdout);
	fputc(' ', stdout);
	fwrite_term(stmt[TTL_GRPH], stdout);
	fputc(' ', stdout);
	fputc('.', stdout);
	fputc('\n', stdout);
	return;
}

int
main(void)
{
	static char buf[16U * 4096U];
	ttl_parser_t *p = ttl_make_parser();

	p->hdl.decl = decl;
	p->hdl.stmt = stmt;

	for (ssize_t nrd; (nrd = read(STDIN_FILENO, buf, sizeof(buf))) > 0;) {
		printf("read %zd\n", nrd);
		if (ttl_parse_chunk(p, buf, nrd) < 0) {
			puts("FUCK");
			break;
		}
	}

	ttl_free_parser(p);
	return 0;
}

/* ttl.c ends here */

/*** ttl.h - trig/turtle/ntriples/nquads reader */
#if !defined INCLUDED_ttl_h_
#define INCLUDED_ttl_h_
#include <stddef.h>

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
	ttl_str_t val;
} ttl_blk_t;

typedef struct {
	ttl_typ_t typ;
	union {
		ttl_iri_t iri;
		ttl_lit_t lit;
		ttl_blk_t blk;
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

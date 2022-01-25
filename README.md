libttl
======

[![Licence](http://img.shields.io/badge/licence-BSD3c-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)

A no-frills turtle/trig/ntriples/nquads parser.

Red tape
--------

- licensed under [BSD3c][1]
- dependencies: POSIX system, C11 compiler


Motivation
----------

Being a heavy user of (and occasional contributor) to raptor, the
[Redland Raptor RDF syntax library][2], has laid bare the short-comings
of a reference implementation.  Parsing feels heavy with so many knobs
and levers that have to be pulled and turned: Input format has to be
specified, input is always canonicalised, prefixes are expanded.


Show me parser code
-------------------

The user facing bits of the parser are set up with 2 handlers:

    static void prefix_handler(void *usr, ttl_iri_t nsdecl)
    {
            ...
    }
    
    static void statement_handler(void *usr, const ttl_stmt_t *stmt, size_t i)
    {
            /* do something with subjects */
            stmt[i].subj;
            /* do something with predicates */
            stmt[i].pred;
            /* do something with objects */
            stmt[i].obj;
            /* do something with the graph, might be (ttl_term_t){} */
            stmt[i].grph;
            ...
    }
    
    ttl_handler_t my_parser = {prefix_handler, statement_handler};

Then:

    ttl_parser_t *p = ttl_make_parser();
    p->hdl = my_parser;
    ttl_parse_chunk(p, buffer, size);
    ...
    ttl_free_parser(p);

The parser can switch between ntriples/nquads and turtle/trig on the
fly which allows to consume a concatenation of `.nt` and `.ttl` files.

Namespaces don't need to be declared upfront because qnames are returned
as such (i.e. unexpanded).


  [1]: http://opensource.org/licenses/BSD-3-Clause
  [2]: http://librdf.org/raptor/

<!--
  Local variables:
  mode: auto-fill
  fill-column: 72
  filladapt-mode: t
  End:
-->

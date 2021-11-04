#ifndef CC_PP_H
#define CC_PP_H

#include "lex.h"
#include "helpers.h"

#define INCLUDE_MAX_SZ 16
#define MAX_MACRO_NESTING 16

struct pp {
	struct lexer *lexers[INCLUDE_MAX_SZ];
	size_t cur_lexer;
	struct token peek1, peek2;
	bool peekf1, peekf2;
	struct symtab defs;
	struct var_ref *macros[MAX_MACRO_NESTING];
	size_t cur_macro;
};

void pp_init(struct pp *pp, struct lexer *l);

struct token pp_next(struct pp *pp);
struct token pp_peek(struct pp *pp);
struct token pp_peek2(struct pp *pp);
void pp_skip(struct pp *pp);

#endif

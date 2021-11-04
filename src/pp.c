#include <stdlib.h>

#include "pp.h"

#define CUR_LEXER(pp) ((pp)->lexers[(pp)->cur_lexer])

static void add_lexer(struct pp *pp, struct lexer *l) {
	if (pp->cur_lexer + 1 >= INCLUDE_MAX_SZ) {
		printf("can only nest %d includes\n", INCLUDE_MAX_SZ);
		exit(EXIT_FAILURE);
	}

	pp->lexers[++pp->cur_lexer] = l;
}

static void handle_define(struct pp *pp) {
	struct token id = lex_next(CUR_LEXER(pp));
	if (id.t != TOKEN_ID) {
		printf("expected name after define");
		exit(EXIT_FAILURE);
	}
	struct var_ref *insert = add_var(&pp->defs, id.v.id);
	vec_init(&insert->tokens);
	while (!lex_peek(CUR_LEXER(pp)).sol) {
		struct token tok = lex_next(CUR_LEXER(pp));
		VEC_PUSH(&insert->tokens, &tok, struct token);
	}
}

static void add_macro(struct pp *pp, struct var_ref *ref) {
	if (pp->cur_macro + 1 >= MAX_MACRO_NESTING) {
		printf("can only nest %d macros\n", MAX_MACRO_NESTING);
		exit(EXIT_FAILURE);
	}
	ref->token_idx = 0;
	pp->macros[(++pp->cur_macro) - 1] = ref;
}

static struct token  pp_get(struct pp *pp);
static struct token macro_token(struct pp *pp) {
	struct var_ref *macro = pp->macros[pp->cur_macro - 1];
	if (macro->token_idx >= VEC_SIZE(macro->tokens, struct token)) {
		pp->cur_macro--;
		return pp_get(pp);
	}
	struct token *tok = VEC_INDEX(&macro->tokens, macro->token_idx++, struct token);
	return *tok;
}

static void skip_to_endif(struct pp *pp) {
	for (;;) {
		while (lex_next(CUR_LEXER(pp)).t != TOKEN_POUND) ;
		struct token tok;
		if ((tok = lex_next(CUR_LEXER(pp))).t == TOKEN_ID && strrange(tok.v.id, "endif"))
			return;
	}
}

static struct token  pp_get(struct pp *pp) {
	struct token tok;
	if (pp->cur_macro ==  0) {
		tok = lex_next(CUR_LEXER(pp));
	} else {
		tok = macro_token(pp);
	}

	if (tok.t == TOKEN_POUND && tok.sol) {
		struct token direct = lex_next(CUR_LEXER(pp));
		if (direct.t != TOKEN_ID) {
			printf("expected directive after '#'\n");
			exit(EXIT_FAILURE);
		}

		if (strrange(direct.v.id, "include")) {
			lex_enable_include(CUR_LEXER(pp));
			struct token filename = lex_next(CUR_LEXER(pp));
			if (filename.t != TOKEN_STR) {
				printf("expected filename\n");
				exit(EXIT_FAILURE);
			}
			lex_disable_include(CUR_LEXER(pp));
			
			struct src_file *file = src_file_open_include(filename.v.str.buf, filename.v.str.len);
			
			if (file == NULL) {
				printf("cannot open file '%.*s'\n", (int)filename.v.str.len, filename.v.str.buf);
				exit(EXIT_FAILURE);
			}
			
			struct lexer *new_l = lex_new(file);
			add_lexer(pp, new_l);
			return pp_get(pp);
		}
		if (strrange(direct.v.id, "define")) {
			handle_define(pp);
			return pp_get(pp);
		}
		if (strrange(direct.v.id, "ifndef")) {
			struct token sym = lex_next(CUR_LEXER(pp));
			if (sym.t != TOKEN_ID) {
				printf("expected identifier referring to macro definition\n");
				exit(EXIT_FAILURE);
			}

			struct var_ref *ref = find_var(&pp->defs,sym.v.id);
			if (ref != NULL) {
				skip_to_endif(pp);
			}
			return pp_get(pp);
		}
		print_range(stdout, &direct.v.id);
	}

	if (tok.t == TOKEN_EOF) {
		if (pp->cur_lexer == 0) {
			return tok;
		}
		pp->cur_lexer--;
		
		return pp_get(pp);
	}
	if (tok.t == TOKEN_ID) {
		struct var_ref *ref = find_var(&pp->defs, tok.v.id);
		if (ref == NULL) {
			return tok;
		}
		add_macro(pp, ref);
		return pp_get(pp);
	}
	return tok;
}

void pp_init(struct pp *pp, struct lexer *l) {
	for (size_t i = 0; i < INCLUDE_MAX_SZ; i++) {
		pp->lexers[i] = NULL;
	}
	pp->lexers[0] = l;
	pp->cur_lexer = 0;
	pp->cur_macro = 0;
	pp->peekf1 = pp->peekf2 = false;
	init_symtab(&pp->defs, NULL);
}

struct token pp_next(struct pp *pp) {
	if (!pp->peekf1 && !pp->peekf2) {
		return pp_get(pp);
	}
	if (pp->peekf1 && !pp->peekf2) {
		pp->peekf1 = false;
		return pp->peek1;
	}
	struct token ret = pp->peek1;
	pp->peek1 = pp->peek2;
	pp->peekf2 = false;
	return ret;
}

struct token pp_peek(struct pp *pp) {
        if (pp->peekf1)
                return pp->peek1;
        pp->peekf1 = true;
        return pp->peek1 = pp_get(pp);
}

struct token pp_peek2(struct pp *pp) {
	if (pp->peekf2) {
		return pp->peek2;
	} else if (pp->peekf1) {
		pp->peekf2 = true;
		return pp->peek2 = pp_get(pp);
	} else {
		pp->peekf1 = true;
		pp->peek1 = pp_get(pp);
		pp->peekf2 = true;
		return pp->peek2 = pp_get(pp);
	}
}

void pp_skip(struct pp *pp) {
	pp_next(pp);
	(void) add_lexer;
}

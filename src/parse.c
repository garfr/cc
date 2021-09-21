#include <stdlib.h>

#include "parse.h"

#define NEXTT(p) (lex_next(p->lex))
#define SKIPT(p) (lex_skip(p->lex))
#define PEEKT(p) (lex_peek(p->lex))

struct parser {
    struct lexer* lex;
};

static enum num_type intlit_to_num[] = {
    [INTLIT_I] = NUM_INT,
    [INTLIT_L] = NUM_LONG,
    [INTLIT_LL] = NUM_LONG_LONG,
};

static void
fill_num_expr(struct num *num, struct tok_num *tnum) {
    num->type = intlit_to_num[tnum->t];
    num->sign = tnum->sign ? NUM_SIGNED : NUM_UNSIGNED;

    if (tnum->sign) {
	num->sign = NUM_SIGNED;
	num->v.i = tnum->v.s;
    } else {
	num->sign = NUM_UNSIGNED;
	num->v.u = tnum->v.u;
    }
}

struct expr *
parse_expr(struct parser *p) {
    struct expr *expr;
    struct token tok = NEXTT(p);
    switch (tok.t) {
    case TOKEN_INTLIT:
	expr = build_expr(EXPR_NUM, tok.pos);
	fill_num_expr(&expr->v.num, &tok.v.num);
	break;
    case TOKEN_ID:
	expr = build_expr(EXPR_VAR, tok.pos);
	expr->v.var.text = tok.pos;
	break;
    case TOKEN_STR:
	expr = build_expr(EXPR_STR, tok.pos);
	expr->v.str.buf = (const uint8_t *) tok.v.str.buf;
	expr->v.str.len = tok.v.str.len;
	break;
    case TOKEN_LPAREN:
	expr = parse_expr(p);
	SKIPT(p);
	break;
    default:
	printf("expected expression\n");
	exit(EXIT_FAILURE);
    }
    return expr;
}

struct trans_unit
parse_translation_unit(struct lexer *lex) {
    struct parser p;
    p.lex = lex;
    struct trans_unit tunit;
    tunit.file = lex_get_file(lex);
    tunit.expr = parse_expr(&p);

    return tunit;
}

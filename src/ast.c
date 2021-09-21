#include <stdlib.h>

#include "ast.h"

struct expr *
build_expr(enum expr_kind t, struct src_range pos) {
    struct expr *node = calloc(1, sizeof(struct expr));
    node->t = t;
    node->pos = pos;
    return node;
}

static void
print_indents(FILE *file, int l) {
    for (int i = 0; i < l; i++) {
	fprintf(file, "\t");
    }
}

const char *expr_kind_str[] = {
    [EXPR_NUM] = "EXPR_NUM",
    [EXPR_STR] = "EXPR_STR",
    [EXPR_VAR] = "EXPR_VAR",
};

static void
display_expr(FILE *file, struct expr* expr, int l) {
    print_indents(file, l);
    fprintf(file, "%s : ", expr_kind_str[expr->t]);

    switch (expr->t) {
    case EXPR_NUM:
	switch (expr->v.num.type) {
	case NUM_INT:
	case NUM_LONG:
	case NUM_LONG_LONG:
	    if (expr->v.num.sign == NUM_SIGNED)
		fprintf(file, "%ld", expr->v.num.v.i);
	    else
		fprintf(file, "%lu", expr->v.num.v.u);
	    break;
	case NUM_FLOAT:
	    fprintf(file, "%f", expr->v.num.v.f);
	}
	break;
    case EXPR_STR:
	fprintf(file, "%.*s", (int)expr->v.str.len, (char*)expr->v.str.buf);
	break;
    case EXPR_VAR:
	print_range(file, &expr->v.var.text);
	break;
    }

    fprintf(file, "\n");
}

void
print_expr(FILE *file, struct expr *expr) {
    display_expr(file, expr, 0);
}

void print_trans_unit(FILE *file, struct trans_unit tunit);

#include <stdlib.h>

#include "ast.h"

struct expr *
build_expr(enum expr_kind t, struct src_range pos) {
    struct expr *node = calloc(1, sizeof(struct expr));
    node->t = t;
    node->pos = pos;
    return node;
}

struct stmt *
build_stmt(enum stmt_kind t, struct src_range pos) {
    struct stmt *node = calloc(1, sizeof(struct stmt));
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
    [EXPR_DEREF] = "EXPR_DEREF",
    [EXPR_ADDR] = "EXPR_ADDR",
    [EXPR_UNOP] = "EXPR_UNOP",
    [EXPR_NUM] = "EXPR_NUM",
    [EXPR_STR] = "EXPR_STR",
    [EXPR_VAR] = "EXPR_VAR",
    [EXPR_BINOP] = "EXPR_BINOP",
    [EXPR_SUF_INC] = "EXPR_SUF_INC",
    [EXPR_SUF_DEC] = "EXPR_SUF_DEC",
    [EXPR_PRE_INC] = "EXPR_PRE_INC",
    [EXPR_PRE_DEC] = "EXPR_PRE_DEC",
    [EXPR_SUBSCRIPT] = "EXPR_SUBSCRIPT",
    [EXPR_MEMBER] = "EXPR_MEMBER",
    [EXPR_PTR_MEMBER] = "EXPR_PTR_MEMBER",
    [EXPR_FUNCALL] = "EXPR_FUNCALL",
    [EXPR_COND] = "EXPR_COND",
    [EXPR_ASSN] = "EXPR_ASSN",
    [EXPR_COMMA] = "EXPR_COMMA",
};

const char *binop_kind_str[] = {
    [BINOP_AND] = "BINOP_AND",
    [BINOP_OR] = "BINOP_OR",
   
    [BINOP_BW_OR] = "BINOP_BW_OR",
    [BINOP_BW_XOR] = "BINOP_BW_XOR",
    [BINOP_BW_AND] = "BINOP_BW_AND",
    
    [BINOP_EQ] = "BINOP_EQ",
    [BINOP_NEQ] = "BINOP_NEQ",
    
    [BINOP_LT] = "BINOP_LT",
    [BINOP_LTE] = "BINOP_LTE",
    [BINOP_GT] = "BINOP_GT",
    [BINOP_GTE] = "BINOP_GTE",
    
    [BINOP_LSH] = "BINOP_LSH",
    [BINOP_RSH] = "BINOP_RSH",
    
    [BINOP_ADD] = "BINOP_ADD",
    [BINOP_SUB] = "BINOP_SUB",
    
    [BINOP_MUL] = "BINOP_MUL",
    [BINOP_DIV] = "BINOP_DIV",
    [BINOP_MOD] = "BINOP_MOD",
};

const char *unop_kind_str[] = {
    [UNOP_POS] = "UNOP_POS",
    [UNOP_NEG] = "UNOP_NEG",
    [UNOP_BW_NOT] = "UNOP_BW_NOT",
    [UNOP_NOT] = "UNOP_NOT",
};

const char *assn_kind_str[] = {
    [ASSN] = "ASSN",
    [ASSN_ADD] = "ASSN_ADD",
    [ASSN_SUB] = "ASSN_SUB",
    [ASSN_MUL] = "ASSN_MUL",
    [ASSN_DIV] = "ASSN_DIV",
    [ASSN_MOD] = "ASSN_MOD",
    [ASSN_LSH] = "ASSN_LSH",
    [ASSN_RSH] = "ASSN_RSH",
    [ASSN_AND] = "ASSN_AND",
    [ASSN_OR] = "ASSN_OR",
    [ASSN_XOR] = "ASSN_XOR",
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
    case EXPR_SUF_INC:
    case EXPR_SUF_DEC:
    case EXPR_PRE_INC:
    case EXPR_PRE_DEC:
    case EXPR_DEREF:
    case EXPR_ADDR:
	fprintf(file, "\n");
	display_expr(file, expr->v.unary, l + 1);
	break;
    case EXPR_UNOP:
	fprintf(file, "%s\n", unop_kind_str[expr->v.unop.op]);
	display_expr(file, expr->v.unop.val, l + 1);
	break;
    case EXPR_MEMBER:
    case EXPR_PTR_MEMBER:
	fprintf(file, "\n");
	display_expr(file, expr->v.member.val, l + 1);
	fprintf(file, "\n");
	print_indents(file, l + 1);
	fprintf(file, "MEMBER_NAME: ");
	print_range(file, &expr->v.member.member.name);
	break;
    case EXPR_SUBSCRIPT:
	fprintf(file, "\n");
	display_expr(file, expr->v.subscript.arr, l + 1);
	fprintf(file, "\n");
	display_expr(file, expr->v.subscript.idx, l + 1);
	break;
    case EXPR_BINOP:
	fprintf(file, "%s\n", binop_kind_str[expr->v.binop.op]);
	display_expr(file, expr->v.binop.l, l + 1);
	fprintf(file, "\n");
	display_expr(file, expr->v.binop.r, l + 1);
	break;
    case EXPR_FUNCALL: {
	fprintf(file, "\n");
	display_expr(file, expr->v.funcall.fun, l + 1);
	for (size_t i = 0; i < VEC_SIZE(expr->v.funcall.args, struct expr *); i++) {
	    fprintf(file, "\n");
	    display_expr(file, *VEC_INDEX(&expr->v.funcall.args, i, struct expr*), l + 1);
	}
	break;
    }
    case EXPR_COND:
	fprintf(file, "\n");
	display_expr(file, expr->v.cond.cond, l + 1);
	fprintf(file, "\n");
	display_expr(file, expr->v.cond.t, l + 1);
	fprintf(file, "\n");
	display_expr(file, expr->v.cond.f, l + 1);
	break;
    case EXPR_ASSN:
	fprintf(file, "%s\n", assn_kind_str[expr->v.assn.op]);
	display_expr(file, expr->v.assn.lhs, l + 1);
	fprintf(file, "\n");
	display_expr(file, expr->v.assn.rhs, l + 1);
	break;
    case EXPR_COMMA:
	fprintf(file, "\n");
	display_expr(file, expr->v.comma.lhs, l + 1);
	fprintf(file, "\n");
	display_expr(file, expr->v.comma.lhs, l + 1);
	break;
    }
}

void
print_expr(FILE *file, struct expr *expr) {
    display_expr(file, expr, 0);
    fprintf(file, "\n");
}

static const char * stmt_kind_str[] = {
    [STMT_IF] = "STMT_IF",
    [STMT_SWITCH] = "STMT_SWITCH",
    [STMT_FOR] = "STMT_FOR",
    [STMT_WHILE] = "STMT_WHILE",
    [STMT_DO] = "STMT_DO",
    [STMT_BLOCK] = "STMT_BLOCK",
    [STMT_EXPR] = "STMT_EXPR",
    [STMT_RETURN] = "STMT_RETURN",
    [STMT_NULL] = "STMT_NULL",
};

void
display_stmt(FILE *file, struct stmt* stmt, int i) {
    print_indents(file, i);
    fprintf(file, "%s : ", stmt_kind_str[stmt->t]);
    switch (stmt->t) {
    case STMT_NULL:
	break;
    case STMT_IF:
	fprintf(file, "\n");
	display_expr(file, stmt->v._if.cond, i + 1);
	fprintf(file, "\n");
	display_stmt(file, stmt->v._if.t, i + 1);
	if (stmt->v._if.f != NULL) {
	    fprintf(file, "\n");
	    display_stmt(file, stmt->v._if.f, i + 1);
	}
	break;
    case STMT_SWITCH:
	fprintf(file, "\n");
	display_expr(file, stmt->v._switch.cond, i + 1);
	fprintf(file, "\n");
	display_stmt(file, stmt->v._switch.body, i + 1);
	break;
    case STMT_FOR:
	fprintf(file, "\n");
	display_expr(file, stmt->v._for.init, i + 1);
	fprintf(file, "\n");
	display_expr(file, stmt->v._for.cond, i + 1);
	fprintf(file, "\n");
	display_expr(file, stmt->v._for.inc, i + 1);
	fprintf(file, "\n");
	display_stmt(file, stmt->v._for.body, i + 1);
	break;
    case STMT_WHILE:
	fprintf(file, "\n");
	display_expr(file, stmt->v._while.cond, i + 1);
	fprintf(file, "\n");
	display_stmt(file, stmt->v._while.body, i + 1);
	break;
    case STMT_DO:
	fprintf(file, "\n");
	display_expr(file, stmt->v._do.cond, i + 1);
	fprintf(file, "\n");
	display_stmt(file, stmt->v._do.body, i + 1);
	break;
    case STMT_EXPR:
	fprintf(file, "\n");
	display_expr(file, stmt->v.expr, i + 1);
	break;
    case STMT_RETURN:
	fprintf(file, "\n");
	display_expr(file, stmt->v.ret, i + 1);
	break;
    case STMT_BLOCK: {
	for (size_t i = 0; i < VEC_SIZE(stmt->v.block.stmts, struct stmt *); i++) {
	    struct stmt *tmp = *VEC_INDEX(&stmt->v.block.stmts, i, struct stmt *);
	    fprintf(file, "\n");
	    display_stmt(file, tmp, i);
	}
    }
    }
}

void
print_stmt(FILE *file, struct stmt *stmt) {
    display_stmt(file, stmt, 0);
    fprintf(file, "\n");
}

void print_trans_unit(FILE *file, struct trans_unit tunit);

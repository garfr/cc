#include <stdlib.h>

#include "ast.h"

/* exposed handles */
struct type *type_void;
struct type *type_char;
struct type *type_short;
struct type *type_int;
struct type *type_long;
struct type *type_uchar;
struct type *type_ushort;
struct type *type_uint;
struct type *type_ulong;

struct type _type_void;
struct type _type_char;
struct type _type_short;
struct type _type_int;
struct type _type_long;
struct type _type_uchar;
struct type _type_ushort;
struct type _type_uint;
struct type _type_ulong;

void init_builtin_types() {
        _type_void.t = TYPE_VOID;
        _type_char.t = _type_uchar.t = TYPE_CHAR;
        _type_short.t = _type_ushort.t = TYPE_SHORT;
        _type_int.t = _type_uint.t = TYPE_INT;
        _type_long.t = _type_ulong.t = TYPE_LONG;

        _type_uchar.sign = _type_ushort.sign = _type_uint.sign =
            _type_ulong.sign = NUM_UNSIGNED;

        _type_char.sign = _type_short.sign = _type_int.sign = _type_long.sign =
            NUM_SIGNED;

        type_void = &_type_void;
        type_char = &_type_char;
        type_short = &_type_short;
        type_int = &_type_int;
        type_long = &_type_long;
        type_uchar = &_type_uchar;
        type_ushort = &_type_ushort;
        type_uint = &_type_uint;
        type_ulong = &_type_ulong;
}

void init_symtab(struct symtab *out, struct symtab *up) {
	out->up = up;
	out->buckets = calloc(8, sizeof(struct var_ref *));
	out->sz = 8;
}

static size_t hash_name(struct src_range name) {
	size_t total = 0;
	for (size_t i = name.s1; i < name.s2; i++) {
		total += name.f->buf[i];
		total *= 10;
	}
	return total;
}

struct var_ref *add_var(struct symtab *tab, struct src_range name) {
	size_t idx = hash_name(name) % tab->sz;

	struct var_ref *iter = tab->buckets[idx];
	while (iter != NULL) {
		if (rangeeq(iter->name, name)) {
			printf("var already exists in scope.");
						exit(EXIT_FAILURE);
		}
		iter = iter->next;
	}
	struct var_ref *new = calloc(1, sizeof(struct var_ref));
	new->next = tab->buckets[idx];
	tab->buckets[idx] = new;
	new->name = name;
	return new;
}

struct var_ref *find_var(struct symtab *tab, struct src_range name) {
	size_t idx = hash_name(name) % tab->sz;

	for (; tab != NULL; tab = tab->up) {
		struct var_ref *iter = tab->buckets[idx];
		while (iter != NULL) {
			if (rangeeq(iter->name, name)) {
				return iter;
			}
			iter = iter->next;
		}
	}
	return NULL;
}

struct expr *build_expr(enum expr_kind t, struct src_range pos) {
        struct expr *node = calloc(1, sizeof(struct expr));
        node->t = t;
        node->pos = pos;
        return node;
}

struct stmt *build_stmt(enum stmt_kind t, struct src_range pos) {
        struct stmt *node = calloc(1, sizeof(struct stmt));
        node->t = t;
        node->pos = pos;
        return node;
}

static void print_indents(FILE *file, int l) {
        for (int i = 0; i < l; i++) {
                fprintf(file, "\t");
        }
}

const char *expr_kind_str[] = {
    [EXPR_DEREF] = "EXPR_DEREF",     [EXPR_ADDR] = "EXPR_ADDR",
    [EXPR_UNOP] = "EXPR_UNOP",       [EXPR_NUM] = "EXPR_NUM",
    [EXPR_STR] = "EXPR_STR",         [EXPR_VAR] = "EXPR_VAR",
    [EXPR_BINOP] = "EXPR_BINOP",     [EXPR_SUF_INC] = "EXPR_SUF_INC",
    [EXPR_SUF_DEC] = "EXPR_SUF_DEC", [EXPR_PRE_INC] = "EXPR_PRE_INC",
    [EXPR_PRE_DEC] = "EXPR_PRE_DEC", [EXPR_SUBSCRIPT] = "EXPR_SUBSCRIPT",
    [EXPR_MEMBER] = "EXPR_MEMBER",   [EXPR_PTR_MEMBER] = "EXPR_PTR_MEMBER",
    [EXPR_FUNCALL] = "EXPR_FUNCALL", [EXPR_COND] = "EXPR_COND",
    [EXPR_ASSN] = "EXPR_ASSN",       [EXPR_COMMA] = "EXPR_COMMA",
};

const char *binop_kind_str[] = {
    [BINOP_AND] = "BINOP_AND",       [BINOP_OR] = "BINOP_OR",

    [BINOP_BW_OR] = "BINOP_BW_OR",   [BINOP_BW_XOR] = "BINOP_BW_XOR",
    [BINOP_BW_AND] = "BINOP_BW_AND",

    [BINOP_EQ] = "BINOP_EQ",         [BINOP_NEQ] = "BINOP_NEQ",

    [BINOP_LT] = "BINOP_LT",         [BINOP_LTE] = "BINOP_LTE",
    [BINOP_GT] = "BINOP_GT",         [BINOP_GTE] = "BINOP_GTE",

    [BINOP_LSH] = "BINOP_LSH",       [BINOP_RSH] = "BINOP_RSH",

    [BINOP_ADD] = "BINOP_ADD",       [BINOP_SUB] = "BINOP_SUB",

    [BINOP_MUL] = "BINOP_MUL",       [BINOP_DIV] = "BINOP_DIV",
    [BINOP_MOD] = "BINOP_MOD",
};

const char *unop_kind_str[] = {
    [UNOP_POS] = "UNOP_POS",
    [UNOP_NEG] = "UNOP_NEG",
    [UNOP_BW_NOT] = "UNOP_BW_NOT",
    [UNOP_NOT] = "UNOP_NOT",
};

const char *assn_kind_str[] = {
    [ASSN] = "ASSN",         [ASSN_ADD] = "ASSN_ADD", [ASSN_SUB] = "ASSN_SUB",
    [ASSN_MUL] = "ASSN_MUL", [ASSN_DIV] = "ASSN_DIV", [ASSN_MOD] = "ASSN_MOD",
    [ASSN_LSH] = "ASSN_LSH", [ASSN_RSH] = "ASSN_RSH", [ASSN_AND] = "ASSN_AND",
    [ASSN_OR] = "ASSN_OR",   [ASSN_XOR] = "ASSN_XOR",
};

static void display_expr(FILE *file, struct expr *expr, int l) {
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
                fprintf(file, "%.*s", (int)expr->v.str.len,
                        (char *)expr->v.str.buf);
                break;
        case EXPR_VAR:
                print_range(file, &expr->v.var->name);
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
                for (size_t i = 0;
                     i < VEC_SIZE(expr->v.funcall.args, struct expr *); i++) {
                        fprintf(file, "\n");
                        display_expr(
                            file,
                            *VEC_INDEX(&expr->v.funcall.args, i, struct expr *),
                            l + 1);
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

void print_expr(FILE *file, struct expr *expr) {
        display_expr(file, expr, 0);
        fprintf(file, "\n");
}

static const char *stmt_kind_str[] = {
    [STMT_IF] = "STMT_IF",     [STMT_SWITCH] = "STMT_SWITCH",
    [STMT_FOR] = "STMT_FOR",   [STMT_WHILE] = "STMT_WHILE",
    [STMT_DO] = "STMT_DO",     [STMT_BLOCK] = "STMT_BLOCK",
    [STMT_EXPR] = "STMT_EXPR", [STMT_RETURN] = "STMT_RETURN",
    [STMT_NULL] = "STMT_NULL", [STMT_DEFAULT] = "STMT_DEFAULT",
    [STMT_LABEL] = "STMT_LABEL", [STMT_CASE] = "STMT_CASE",
    [STMT_GOTO] = "STMT_GOTO", [STMT_BREAK] = "STMT_BREAK",
    [STMT_CONTINUE] = "STMT_CONTINUE",
};

static const char *type_kind_str[] = {
    [TYPE_VOID] = "TYPE_VOID",   [TYPE_CHAR] = "TYPE_CHAR",
    [TYPE_SHORT] = "TYPE_SHORT", [TYPE_INT] = "TYPE_INT",
    [TYPE_LONG] = "TYPE_LONG",   [TYPE_PTR] = "TYPE_PTR",
};

void display_type(FILE *file, struct type *type, int i) {
        print_indents(file, i);
        fprintf(file, "%s : ", type_kind_str[type->t]);

        switch (type->t) {
        case TYPE_CHAR:
        case TYPE_SHORT:
        case TYPE_INT:
        case TYPE_LONG:
                fprintf(file, "%s",
                        type->sign == NUM_SIGNED ? "SIGNED" : "UNSIGNED");
		break;
	case TYPE_PTR:
		fprintf(file, "\n");
		display_type(file, type->v.ptr, i + 1);
		break;
        default:
                break;
        }
}

void print_type(FILE *file, struct type *type) {
	display_type(file, type, 0);
}

void display_stmt(FILE *file, struct stmt *stmt, int i) {
        print_indents(file, i);
        fprintf(file, "%s : ", stmt_kind_str[stmt->t]);
        switch (stmt->t) {
        case STMT_NULL:
                break;
	case STMT_LABEL:
		fprintf(file, "\n");
		print_indents(file, i + 1);
		print_range(file, &stmt->v.label.name->name);
		fprintf(file, "\n");
		display_stmt(file, stmt->v.label.stmt, i + 1);
		break;
	case STMT_DEFAULT:
		fprintf(file, "\n");
		display_stmt(file, stmt->v._default, i + 1);
		break;
	case STMT_CASE:
		fprintf(file, "\n");
		display_expr(file, stmt->v._case.val, i + 1);
		fprintf(file, "\n");
		display_stmt(file, stmt->v._case.stmt, i + 1);
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
		if (stmt->v._for.init) {
			fprintf(file, "\n");
			display_expr(file, stmt->v._for.init, i + 1);
		}
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
	case STMT_BREAK:
	case STMT_CONTINUE:
		break;
        case STMT_RETURN:
                fprintf(file, "\n");
                display_expr(file, stmt->v.ret, i + 1);
                break;
	case STMT_GOTO:
		if (stmt->v._goto.ref) {
			print_range(file, &stmt->v._goto.ref->name);
		} else {
			fprintf(file, "<label not filled in>");
		}
		break;
        case STMT_BLOCK: {
                for (size_t idx = 0;
                     idx < VEC_SIZE(stmt->v.block.items, struct stmt*); idx++) {
                        struct stmt *tmp =
                            *VEC_INDEX(&stmt->v.block.items, idx, struct stmt*);
                        fprintf(file, "\n");
                        display_stmt(file, tmp, i + 1);
                }

		fprintf(file, "\n");
		
		struct symtab *tab = &stmt->v.block.vars;
		for (size_t idx = 0; idx < tab->sz; idx++) {
			struct var_ref *iter = tab->buckets[idx];
			for (; iter != NULL; iter = iter->next) {
				print_indents(file, i + 1);
				print_range(file, &iter->name);
				fprintf(file, " : \n");
				display_type(file, iter->type, i + 2);
				fprintf(file, "\n");
			}
		}
        }
        }
}

void print_stmt(FILE *file, struct stmt *stmt) {
        display_stmt(file, stmt, 0);
        fprintf(file, "\n");
}

void print_fun(FILE *file, struct fun *fun) {
	fprintf(file, "FUN: ");
	print_range(file, &fun->name->name);
	fprintf(file, "\n");
	display_stmt(file, fun->body, 1);
}

void print_trans_unit(FILE *file, struct trans_unit tunit) {
	for (size_t i = 0; i < VEC_SIZE(tunit.funs, struct fun *); i++) {
		struct fun *fun = *VEC_INDEX(&tunit.funs, i, struct fun *);
		print_fun(file, fun);
		fprintf(file, "\n");
	}
}

struct type *clone_type(struct type *type) {
	struct type *new = calloc(1, sizeof(struct type));
	*new = *type;
	return new;
}

struct type *build_type(enum type_kind t) {
	struct type *ret = calloc(1, sizeof(struct type));
	ret->t = t;
	return ret;
}

struct type *build_ptr(struct type *type) {
	struct type *ptr = build_type(TYPE_PTR);
	ptr->v.ptr = type;
	return ptr;
}

static void recurse_visit_stmt(struct stmt *stmt,
			       struct stmt *parent,
			       stmt_visit callback, void *ud) {
	callback(stmt, parent, ud);
	switch(stmt->t) {
	case STMT_LABEL:
		recurse_visit_stmt(stmt->v.label.stmt, stmt, callback, ud);
		break;
	case STMT_CASE:
		recurse_visit_stmt(stmt->v._case.stmt, stmt, callback, ud);
		break;
	case STMT_DEFAULT:
		recurse_visit_stmt(stmt->v._default, stmt, callback, ud);
		break;
        case STMT_IF:
		recurse_visit_stmt(stmt->v._if.t, stmt, callback, ud);
		if (stmt->v._if.f)
			recurse_visit_stmt(stmt->v._if.f, stmt, callback, ud);
		break;
        case STMT_SWITCH:
		recurse_visit_stmt(stmt->v._switch.body, stmt, callback, ud);
		break;
        case STMT_FOR:
		recurse_visit_stmt(stmt->v._for.body, stmt, callback, ud);
		break;
        case STMT_WHILE:
		recurse_visit_stmt(stmt->v._while.body, stmt, callback, ud);
		break;
        case STMT_DO:
		recurse_visit_stmt(stmt->v._do.body, stmt, callback, ud);
		break;
        case STMT_BLOCK: {
		for (size_t i = 0; i < VEC_SIZE(stmt->v.block.items, struct stmt *); i++) {
			struct stmt**tmp = VEC_INDEX(&stmt->v.block.items, i, struct stmt *);
			recurse_visit_stmt(*tmp, stmt, callback, ud);
		}
		break;
	}
        case STMT_NULL:
	case STMT_GOTO:
	case STMT_EXPR:
	case STMT_RETURN:
	case STMT_BREAK:
	case STMT_CONTINUE:
		break;
	}
}

void visit_stmts(struct fun *fun, stmt_visit callback, void *ud) {
	recurse_visit_stmt(fun->body, NULL, callback, ud);
}

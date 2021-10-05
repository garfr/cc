#ifndef CC_AST_H
#define CC_AST_H

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include "helpers.h"

enum num_sign {
    NUM_SIGNED,
    NUM_UNSIGNED,
};

enum num_type {
    NUM_INT,
    NUM_LONG,
    NUM_LONG_LONG,
    NUM_FLOAT,
};

struct num {
    enum num_type type;
    enum num_sign sign;
    
    union {
	int64_t i;
	uint64_t u;
	float f;
    } v;
};

struct var_ref {
    struct src_range text;
    /* TODO: add symbol table information */
};

struct strlit {
    const uint8_t *buf;
    size_t len;
};

enum binop_op {
    /* boolean */
    BINOP_OR,
    BINOP_AND,
    /* bitwise */
    BINOP_BW_OR,
    BINOP_BW_XOR,
    BINOP_BW_AND,
    /* equality */
    BINOP_EQ,
    BINOP_NEQ,
    /* relational */
    BINOP_LT,
    BINOP_LTE,
    BINOP_GT,
    BINOP_GTE,
    /* bit shift */
    BINOP_LSH,
    BINOP_RSH,
    /* additive */
    BINOP_ADD,
    BINOP_SUB,
    /* multiplicative */
    BINOP_MUL,
    BINOP_DIV,
    BINOP_MOD,    
};

enum unop_op {
    UNOP_POS,
    UNOP_NEG,
    UNOP_BW_NOT,
    UNOP_NOT,
};

enum assn_op {
    ASSN,
    ASSN_MUL,
    ASSN_DIV,
    ASSN_MOD,
    ASSN_ADD,
    ASSN_SUB,
    ASSN_LSH,
    ASSN_RSH,
    ASSN_AND,
    ASSN_XOR,
    ASSN_OR,
};

enum expr_kind {
    EXPR_DEREF,
    EXPR_ADDR,
    EXPR_UNOP,
    EXPR_VAR,
    EXPR_NUM,
    EXPR_STR,
    EXPR_BINOP,
    EXPR_SUF_INC,
    EXPR_SUF_DEC,
    EXPR_PRE_INC,
    EXPR_PRE_DEC,
    EXPR_SUBSCRIPT,
    EXPR_FUNCALL,
    EXPR_MEMBER,
    EXPR_PTR_MEMBER,
    EXPR_COND,
    EXPR_ASSN,
    EXPR_COMMA,
};

struct member {
    struct src_range name;
    /* TODO: add position information */
};

struct expr {
    enum expr_kind t;
    struct src_range pos;
    
    union {
	struct var_ref var;
	struct num num;
	struct strlit str;
	struct {
	    struct expr *l;
	    struct expr *r;
	    enum binop_op op;
	} binop;
	struct {
	    struct expr *val;
	    enum unop_op op;
	} unop;
	struct {
	    struct expr *cond;
	    struct expr *t;
	    struct expr *f;
	} cond;
	struct {
	    struct expr *lhs;
	    struct expr *rhs;
	    enum assn_op op;
	} assn;
	struct expr *unary;
	struct {
	    struct expr *arr;
	    struct expr *idx;
	} subscript;
	struct {
	    struct expr *val;
	    struct member member;
	} member;
	struct {
	    struct expr *fun;
	    struct vec args; // struct expr *
	} funcall;
	struct {
	    struct expr *lhs;
	    struct expr *rhs;
	} comma;
    } v;
};

enum stmt_kind {
    STMT_EXPR,
    STMT_RETURN,
    STMT_IF,
    STMT_SWITCH,
    STMT_FOR,
    STMT_WHILE,
    STMT_DO,
    STMT_BLOCK,
    STMT_NULL, /* semicolon with no other tokens */
};

struct stmt {
    enum stmt_kind t;
    struct src_range pos;

    union {
	struct expr *ret;
	struct expr *expr;
	struct {
	    struct expr *cond;
	    struct stmt *t;
	    struct stmt *f; /* NULL if no else clause. */
	} _if;
	struct {
	    struct expr *cond;
	    struct stmt *body;
	} _switch;
	struct {
	    struct expr *init;
	    struct expr *cond;
	    struct expr *inc;
	    struct stmt *body;
	} _for;
	struct {
	    struct expr *cond;
	    struct stmt *body;
	} _while;
	struct {
	    struct expr *cond;
	    struct stmt *body;
	} _do;
	struct {
	    struct vec stmts; /* struct stmt * */
	} block;
    } v;
};

struct trans_unit {
    struct src_file *file;
    struct stmt *stmt;
};

struct stmt *build_stmt(enum stmt_kind t, struct src_range pos);
struct expr *build_expr(enum expr_kind t, struct src_range pos);

void print_expr(FILE *file, struct expr *expr);
void print_stmt(FILE *file, struct stmt *stmt);
void print_trans_unit(FILE *file, struct trans_unit tunit);

#endif

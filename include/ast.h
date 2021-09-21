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

enum expr_kind {
    EXPR_VAR,
    EXPR_NUM,
    EXPR_STR,
};

struct expr {
    enum expr_kind t;
    struct src_range pos;
    
    union {
	struct var_ref var;
	struct num num;
	struct strlit str;
    } v;
};

struct trans_unit {
    struct src_file *file;
    struct expr *expr;
};

struct expr *build_expr(enum expr_kind t, struct src_range pos);

void print_expr(FILE *file, struct expr *expr);
void print_trans_unit(FILE *file, struct trans_unit tunit);

#endif

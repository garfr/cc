#ifndef CC_AST_H
#define CC_AST_H

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include "helpers.h"

struct type;

struct var_ref {
        struct src_range name;
	struct type *type;
	struct stmt *label_loc;
	struct var_ref *next;
};

struct symtab {
	struct symtab *up;
	struct var_ref **buckets;
	size_t sz;
};

enum num_sign {
        NUM_SIGNED,
        NUM_UNSIGNED,
};

enum type_kind {
        TYPE_VOID,
        TYPE_CHAR,
        TYPE_SHORT,
        TYPE_INT,
        TYPE_LONG,
        TYPE_PTR,
	TYPE_FUN,
};

struct type {
        enum type_kind t;
        enum num_sign sign;

        union {
                struct type *ptr;
		struct {
			struct type *ret;
			struct vec params; /* struct type * */
		} fun;
        } v;
};

/* Preallocated types to represent builtin types.
 * When a type is specified like 'const int', these are copied into a newly
 * allocated type and the const flag is set.
 */
extern struct type *type_void;

extern struct type *type_char;
extern struct type *type_short;
extern struct type *type_int;
extern struct type *type_long;

extern struct type *type_uchar;
extern struct type *type_ushort;
extern struct type *type_uint;
extern struct type *type_ulong;

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
        /* TODO: add member reference information */
};

struct expr {
        enum expr_kind t;
        struct src_range pos;

        union {
                struct var_ref *var;
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
	STMT_LABEL,
	STMT_CASE,
	STMT_DEFAULT,
        STMT_EXPR,
        STMT_RETURN,
	STMT_CONTINUE,
	STMT_BREAK,
        STMT_IF,
        STMT_SWITCH,
        STMT_FOR,
        STMT_WHILE,
        STMT_DO,
        STMT_BLOCK,
        STMT_NULL, /* semicolon with no other tokens */
	STMT_GOTO,
};

struct stmt {
        enum stmt_kind t;
        struct src_range pos;

        union {
                struct expr *ret;
                struct expr *expr;
		struct {
			struct var_ref *name;
			struct stmt *stmt;
		} label;
		struct {
			struct var_ref *ref;
			struct src_range name;
		} _goto;
		struct {
			struct expr *val;
			struct stmt *stmt;
		} _case;
		struct stmt *_default;
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
                        struct vec items; /* struct stmt * */
			struct symtab vars;
                } block;
		
        } v;
};

struct param {
	struct type *type;
	struct var_ref *ref;
	struct src_range name;
};

struct fun {
	struct stmt *body; /* MUST be a STMT_BLOCK. */
	struct symtab labels;
	struct var_ref *name;
	struct vec params; /* struct param */
};

struct trans_unit {
        struct src_file *file;
	struct vec funs; /* struct fun * */
};

/* allocates space for builtin types */
void init_builtin_types(void);

struct stmt *build_stmt(enum stmt_kind t, struct src_range pos);
struct expr *build_expr(enum expr_kind t, struct src_range pos);
struct type *build_type(enum type_kind t);
struct type *build_ptr(struct type *type);
void print_expr(FILE *file, struct expr *expr);
void print_stmt(FILE *file, struct stmt *stmt);
void print_type(FILE *file, struct type *type);
void print_fun(FILE *file, struct fun *fun);
void print_trans_unit(FILE *file, struct trans_unit tunit);

struct type *clone_type(struct type *type);

void init_symtab(struct symtab *tab, struct symtab *up);
struct var_ref *add_var(struct symtab *tab, struct src_range name);
struct var_ref *find_var(struct symtab *tab, struct src_range name);

typedef void (*stmt_visit)(struct stmt *, struct stmt *, void *);

void visit_stmts(struct fun *fun, stmt_visit callback, void *ud);

#endif

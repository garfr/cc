#ifndef CC_LEX_H
#define CC_LEX_H

#include <stdio.h>

#include "helpers.h"

enum token_kind {
    /* keywords */
    TOKEN_AUTO,
    TOKEN_BREAK,
    TOKEN_CASE,
    TOKEN_CHAR,
    TOKEN_CONST,
    TOKEN_CONTINUE,
    TOKEN_DEFAULT,
    TOKEN_DO,
    TOKEN_DOUBLE,
    TOKEN_ELSE,
    TOKEN_ENUM,
    TOKEN_EXTERN,
    TOKEN_FLOAT,
    TOKEN_FOR,
    TOKEN_GOTO,
    TOKEN_IF,
    TOKEN_INLINE,
    TOKEN_INT,
    TOKEN_LONG,
    TOKEN_REGISTER,
    TOKEN_RESTRICT,
    TOKEN_RETURN,
    TOKEN_SHORT,
    TOKEN_SIGNED,
    TOKEN_SIZEOF,
    TOKEN_STATIC,
    TOKEN_STRUCT,
    TOKEN_SWITCH,
    TOKEN_TYPEDEF,
    TOKEN_UNION,
    TOKEN_UNSIGNED,
    TOKEN_VOID,
    TOKEN_VOLATILE,
    TOKEN_WHILE,

    /* punctuation */
    TOKEN_LBRACK,
    TOKEN_RBRACK,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LCURLY,
    TOKEN_RCURLY,
    TOKEN_PERIOD,
    TOKEN_ARROW,
    TOKEN_BW_AND,
    TOKEN_STAR,
    TOKEN_ADD,
    TOKEN_SUB,
    TOKEN_BW_NEG,
    TOKEN_BL_NEG,
    TOKEN_DIV,
    TOKEN_MOD,
    TOKEN_INC,
    TOKEN_DEC,
    TOKEN_LSH,
    TOKEN_RSH,
    TOKEN_LT,
    TOKEN_GT,
    TOKEN_LTE,
    TOKEN_GTE,
    TOKEN_DEQ,
    TOKEN_NEQ,
    TOKEN_XOR,
    TOKEN_BW_OR,
    TOKEN_BL_AND,
    TOKEN_BL_OR,
    TOKEN_QMARK,
    TOKEN_COLON,
    TOKEN_SEMICOLON,
    TOKEN_ELIPSIS,
    TOKEN_ASSN,
    TOKEN_MUL_ASSN,
    TOKEN_DIV_ASSN,
    TOKEN_MOD_ASSN,
    TOKEN_ADD_ASSN,
    TOKEN_SUB_ASSN,
    TOKEN_LSH_ASSN,
    TOKEN_RSH_ASSN,
    TOKEN_AND_ASSN,
    TOKEN_XOR_ASSN,
    TOKEN_OR_ASSN,
    TOKEN_COMMA,
    TOKEN_POUND,
    TOKEN_DPOUND,

    /* constants */
    TOKEN_INTLIT,
    TOKEN_FLOATLIT,
    TOKEN_STR,
    TOKEN_CHARLIT,

    /* misc. */
    TOKEN_ID,
    TOKEN_EOF,
};

struct token {
    enum token_kind t;
    struct src_range pos;

    union {
	struct src_range id;
	struct src_range string;
	struct src_range num;
    } v;
};

struct lexer;

struct lexer *lex_new(struct src_file *file);
void lex_free(struct lexer *l);

struct token lex_next(struct lexer *l);
struct token lex_peek(struct lexer *l);
void lex_skip(struct lexer *l);

void lex_print(FILE *f, struct token tok);

#endif

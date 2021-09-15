#ifndef CC_LEX_H
#define CC_LEX_H

#include <stdio.h>

#include "helpers.h"

enum token_kind {
    /* keywords */
    TOKEN_KW_AUTO,
    TOKEN_KW_BREAK,
    TOKEN_KW_CASE,
    TOKEN_KW_CHAR,
    TOKEN_KW_CONST,
    TOKEN_KW_CONTINUE,
    TOKEN_KW_DEFAULT,
    TOKEN_KW_DO,
    TOKEN_KW_DOUBLE,
    TOKEN_KW_ELSE,
    TOKEN_KW_ENUM,
    TOKEN_KW_EXTERN,
    TOKEN_KW_FLOAT,
    TOKEN_KW_FOR,
    TOKEN_KW_GOTO,
    TOKEN_KW_IF,
    TOKEN_KW_INLINE,
    TOKEN_KW_INT,
    TOKEN_KW_LONG,
    TOKEN_KW_REGISTER,
    TOKEN_KW_RESTRICT,
    TOKEN_KW_RETURN,
    TOKEN_KW_SHORT,
    TOKEN_KW_SIGNED,
    TOKEN_KW_SIZEOF,
    TOKEN_KW_STATIC,
    TOKEN_KW_STRUCT,
    TOKEN_KW_SWITCH,
    TOKEN_KW_TYPEDEF,
    TOKEN_KW_UNION,
    TOKEN_KW_UNSIGNED,
    TOKEN_KW_VOID,
    TOKEN_KW_VOLATILE,
    TOKEN_KW_WHILE,

    /* punctuation */
    TOKEN_PN_LBRACK,
    TOKEN_PN_RBRACK,
    TOKEN_PN_LPAREN,
    TOKEN_PN_RPAREN,
    TOKEN_PN_LCURLY,
    TOKEN_PN_RCURLY,
    TOKEN_PN_PERIOD,
    TOKEN_PN_ARROW,
    TOKEN_PN_BW_AND,
    TOKEN_PN_STAR,
    TOKEN_PN_ADD,
    TOKEN_PN_SUB,
    TOKEN_PN_BW_NEG,
    TOKEN_PN_BL_NEG,
    TOKEN_PN_DIV,
    TOKEN_PN_MODULO,
    TOKEN_PN_LSH,
    TOKEN_PN_RSH,
    TOKEN_PN_LT,
    TOKEN_PN_GT,
    TOKEN_PN_LTE,
    TOKEN_PN_GTE,
    TOKEN_PN_DEQ,
    TOKEN_PN_NEQ,
    TOKEN_PN_BW_XOR,
    TOKEN_PN_BW_OR,
    TOKEN_PN_BL_AND,
    TOKEN_PN_BL_OR,
    TOKEN_PN_QMARK,
    TOKEN_PN_COLON,
    TOKEN_PN_SEMICOLON,
    TOKEN_PN_ELIPSIS,
    TOKEN_PN_ASSN,
    TOKEN_PN_MUL_ASSN,
    TOKEN_PN_DIV_ASSN,
    TOKEN_PN_MOD_ASSN,
    TOKEN_PN_ADD_ASSN,
    TOKEN_PN_SUB_ASSN,
    TOKEN_PN_LSH_ASSN,
    TOKEN_PN_RSH_ASSN,
    TOKEN_PN_AND_ASSN,
    TOKEN_PN_XOR_ASSN,
    TOKEN_PN_OR_ASSN,
    TOKEN_PN_COMMA,
    TOKEN_PN_POUND,
    TOKEN_PN_DPOUND,

    /* constants */
    TOKEN_CN_DECINT,
    TOKEN_CN_FLOAT,
    TOKEN_CN_HEXINT,
    TOKEN_CN_STR,
    TOKEN_CN_CHAR,

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

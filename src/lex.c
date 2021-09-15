#include <stdlib.h>
#include <ctype.h>

#include "lex.h"

#define NEXTC(l) (l->file->buf[l->e++])
#define PEEKC(l) (l->file->buf[l->e])
#define SKIPC(l) (l->e++)
#define IS_EOF(l) (l->e >= l->file->sz)
#define RESET(l) (l->s = l->e)

struct lexer {
    struct src_file *file;
    size_t s, e;
    bool peekf;
    struct token peek;
};

struct lexer *
lex_new(struct src_file *file) {
    struct lexer *l = calloc(sizeof(struct lexer), 1);

    l->file = file;
    l->s = l->e = 0;
    l->peekf = false;

    return l;
}

void
lex_free(struct lexer *l) {
    free(l);
}

static void
skip_whitespace(struct lexer *l) {
    while (!IS_EOF(l) && isspace(PEEKC(l))) {
	SKIPC(l);
    }
    RESET(l);
}

static struct src_range
make_range(struct lexer *l) {
    struct src_range rng;
    rng.f = l->file;
    rng.s1 = l->s, rng.s2 = l->e;
    return rng;
}

static struct token
make_tok(struct lexer *l, enum token_kind k) {
    SKIPC(l);
    struct token t;
    t.t = k;
    t.pos = make_range(l);
    return t;
}

static struct token
make_tok_inplace(struct lexer *l, enum token_kind k) {
    struct token t;
    t.t = k;
    t.pos = make_range(l);
    return t;
}

static struct token
lex_id(struct lexer *l) {
    int c;
    while (!IS_EOF(l) && (isalpha(c = PEEKC(l)) || isdigit(c) || c == '_')) {
	SKIPC(l);
    }
    struct token ret = make_tok(l, TOKEN_ID);
    ret.v.id = make_range(l);
    return ret;
}

static struct token
lex_num(struct lexer *l) {
    while (!IS_EOF(l) && isdigit(PEEKC(l))) {
	SKIPC(l);
    }
    struct token ret = make_tok_inplace(l, TOKEN_CN_DECINT);
    ret.v.num = make_range(l);
    return ret;
}

static struct token
get_tok(struct lexer *l) {
    skip_whitespace(l);

    int c = PEEKC(l);

    if (IS_EOF(l)) {
	return make_tok(l, TOKEN_EOF);
    }
    
    if (isalpha(c) || c == '_') {
	return lex_id(l);
    }
    
    if (isdigit(c)) {
	return lex_num(l);
    }
    
    switch (c) {
    case '[':
	return make_tok(l, TOKEN_PN_LBRACK);
    case ']':
	return make_tok(l, TOKEN_PN_RBRACK);
    case '(':
	return make_tok(l, TOKEN_PN_LPAREN);
    case ')':
	return make_tok(l, TOKEN_PN_RPAREN);
    case '{':
	return make_tok(l, TOKEN_PN_LCURLY);
    case '}':
	return make_tok(l, TOKEN_PN_RCURLY);
    case '.':
	return make_tok(l, TOKEN_PN_RBRACK);
    case '=':
	return make_tok(l, TOKEN_PN_ASSN);
    case ';':
	return make_tok(l, TOKEN_PN_SEMICOLON);
    case ':':
	return make_tok(l, TOKEN_PN_COLON);
    case '+':
	return make_tok(l, TOKEN_PN_ADD);
    case '-':
	return make_tok(l, TOKEN_PN_SUB);
    case '*':
	return make_tok(l, TOKEN_PN_STAR);
    case '/':
	return make_tok(l, TOKEN_PN_DIV);
    default:
	printf("invalid char '%c'\n", c);
	exit(EXIT_FAILURE);
    }
}

struct token
lex_next(struct lexer *l) {
    if (l->peekf) {
	l->peekf = false;
	return l->peek;
    }
    return get_tok(l);
}
    
struct token
lex_peek(struct lexer *l) {
    if (l->peekf)
	return l->peek;
    l->peekf = true;
    return l->peek = get_tok(l);
}

void lex_skip(struct lexer *l) {
    if (l->peekf) {
	l->peekf = false;
	return;
    }
    get_tok(l);
}

const char *tok_kind_name[] = {
    [TOKEN_KW_AUTO] = "TOKEN_KW_AUTO",
    [TOKEN_KW_BREAK] = "TOKEN_KW_BREAK",
    [TOKEN_KW_CASE] = "TOKEN_KW_CASE",
    [TOKEN_KW_CHAR] = "TOKEN_KW_CHAR",
    [TOKEN_KW_CONST] = "TOKEN_KW_CONST",
    [TOKEN_KW_CONTINUE] = "TOKEN_KW_CONTINUE",
    [TOKEN_KW_DEFAULT] = "TOKEN_KW_DEFAULT",
    [TOKEN_KW_DO] = "TOKEN_KW_DO",
    [TOKEN_KW_DOUBLE] = "TOKEN_KW_DOUBLE",
    [TOKEN_KW_ELSE] = "TOKEN_KW_ELSE",
    [TOKEN_KW_ENUM] = "TOKEN_KW_ENUM",
    [TOKEN_KW_EXTERN] = "TOKEN_KW_EXTERN",
    [TOKEN_KW_FLOAT] = "TOKEN_KW_FLOAT",
    [TOKEN_KW_FOR] = "TOKEN_KW_FOR",
    [TOKEN_KW_GOTO] = "TOKEN_KW_GOTO",
    [TOKEN_KW_IF] = "TOKEN_KW_IF",
    [TOKEN_KW_INLINE] = "TOKEN_KW_INLINE",
    [TOKEN_KW_INT] = "TOKEN_KW_INT",
    [TOKEN_KW_LONG] = "TOKEN_KW_LONG",
    [TOKEN_KW_REGISTER] = "TOKEN_KW_REGISTER",
    [TOKEN_KW_RESTRICT] = "TOKEN_KW_RESTRICT",
    [TOKEN_KW_RETURN] = "TOKEN_KW_RETURN",
    [TOKEN_KW_SHORT] = "TOKEN_KW_SHORT",
    [TOKEN_KW_SIGNED] = "TOKEN_KW_SIGNED",
    [TOKEN_KW_SIZEOF] = "TOKEN_KW_SIZEOF",
    [TOKEN_KW_STATIC] = "TOKEN_KW_STATIC",
    [TOKEN_KW_STRUCT] = "TOKEN_KW_STRUCT",
    [TOKEN_KW_SWITCH] = "TOKEN_KW_SWITCH",
    [TOKEN_KW_TYPEDEF] = "TOKEN_KW_TYPEDEF",
    [TOKEN_KW_UNION] = "TOKEN_KW_UNION",
    [TOKEN_KW_UNSIGNED] = "TOKEN_KW_UNSIGNED",
    [TOKEN_KW_VOID] = "TOKEN_KW_VOID",
    [TOKEN_KW_VOLATILE] = "TOKEN_KW_VOLATILE",
    [TOKEN_KW_WHILE] = "TOKEN_KW_WHILE",
    [TOKEN_PN_LBRACK] = "TOKEN_PN_LBRACK",
    [TOKEN_PN_RBRACK] = "TOKEN_PN_RBRACK",
    [TOKEN_PN_LPAREN] = "TOKEN_PN_LPAREN",
    [TOKEN_PN_RPAREN] = "TOKEN_PN_RPAREN",
    [TOKEN_PN_LCURLY] = "TOKEN_PN_LCURLY",
    [TOKEN_PN_RCURLY] = "TOKEN_PN_RCURLY",
    [TOKEN_PN_PERIOD] = "TOKEN_PN_PERIOD",
    [TOKEN_PN_ARROW] = "TOKEN_PN_ARROW",
    [TOKEN_PN_BW_AND] = "TOKEN_PN_BW",
    [TOKEN_PN_STAR] = "TOKEN_PN_STAR",
    [TOKEN_PN_ADD] = "TOKEN_PN_ADD",
    [TOKEN_PN_SUB] = "TOKEN_PN_SUB",
    [TOKEN_PN_BW_NEG] = "TOKEN_PN_BW",
    [TOKEN_PN_BL_NEG] = "TOKEN_PN_BL",
    [TOKEN_PN_DIV] = "TOKEN_PN_DIV",
    [TOKEN_PN_MODULO] = "TOKEN_PN_MODULO",
    [TOKEN_PN_LSH] = "TOKEN_PN_LSH",
    [TOKEN_PN_RSH] = "TOKEN_PN_RSH",
    [TOKEN_PN_LT] = "TOKEN_PN_LT",
    [TOKEN_PN_GT] = "TOKEN_PN_GT",
    [TOKEN_PN_LTE] = "TOKEN_PN_LTE",
    [TOKEN_PN_GTE] = "TOKEN_PN_GTE",
    [TOKEN_PN_DEQ] = "TOKEN_PN_DEQ",
    [TOKEN_PN_NEQ] = "TOKEN_PN_NEQ",
    [TOKEN_PN_BW_XOR] = "TOKEN_PN_BW",
    [TOKEN_PN_BW_OR] = "TOKEN_PN_BW",
    [TOKEN_PN_BL_AND] = "TOKEN_PN_BL",
    [TOKEN_PN_BL_OR] = "TOKEN_PN_BL",
    [TOKEN_PN_QMARK] = "TOKEN_PN_QMARK",
    [TOKEN_PN_COLON] = "TOKEN_PN_COLON",
    [TOKEN_PN_SEMICOLON] = "TOKEN_PN_SEMICOLON",
    [TOKEN_PN_ELIPSIS] = "TOKEN_PN_ELIPSIS",
    [TOKEN_PN_ASSN] = "TOKEN_PN_ASSN",
    [TOKEN_PN_MUL_ASSN] = "TOKEN_PN_MUL",
    [TOKEN_PN_DIV_ASSN] = "TOKEN_PN_DIV",
    [TOKEN_PN_MOD_ASSN] = "TOKEN_PN_MOD",
    [TOKEN_PN_ADD_ASSN] = "TOKEN_PN_ADD",
    [TOKEN_PN_SUB_ASSN] = "TOKEN_PN_SUB",
    [TOKEN_PN_LSH_ASSN] = "TOKEN_PN_LSH",
    [TOKEN_PN_RSH_ASSN] = "TOKEN_PN_RSH",
    [TOKEN_PN_AND_ASSN] = "TOKEN_PN_AND",
    [TOKEN_PN_XOR_ASSN] = "TOKEN_PN_XOR",
    [TOKEN_PN_OR_ASSN] = "TOKEN_PN_OR",
    [TOKEN_PN_COMMA] = "TOKEN_PN_COMMA",
    [TOKEN_PN_POUND] = "TOKEN_PN_POUND",
    [TOKEN_PN_DPOUND] = "TOKEN_PN_DPOUND",
    [TOKEN_CN_DECINT] = "TOKEN_CN_DECINT",
    [TOKEN_CN_FLOAT] = "TOKEN_CN_FLOAT",
    [TOKEN_CN_HEXINT] = "TOKEN_CN_HEXINT",
    [TOKEN_CN_STR] = "TOKEN_CN_STR",
    [TOKEN_CN_CHAR] = "TOKEN_CN_CHAR",
    [TOKEN_ID] = "TOKEN_ID",
    [TOKEN_EOF] = "TOKEN_EOF",
};

void lex_print(FILE *f, struct token tok) {
    fprintf(f, "%s\n", tok_kind_name[tok.t]);
}

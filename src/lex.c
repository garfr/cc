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
    struct token ret = make_tok_inplace(l, TOKEN_ID);
    ret.v.id = make_range(l);
    return ret;
}

static int
lookahead_char(struct lexer *l) {
    SKIPC(l);
    if (IS_EOF(l))
	return -1;
    return PEEKC(l);
}

static void
skip_comment_line(struct lexer *l) {
    while (!IS_EOF(l) && PEEKC(l) != '\n')
	SKIPC(l);
}

static struct token
get_tok(struct lexer *l) {
    skip_whitespace(l);

    int c = PEEKC(l);
    int ec;

    if (IS_EOF(l)) {
	return make_tok(l, TOKEN_EOF);
    }

    if (isalpha(c) || c == '_') {
	return lex_id(l);
    }

    switch (c) {
    case '[':
	return make_tok(l, TOKEN_LBRACK);
    case ']':
	return make_tok(l, TOKEN_RBRACK);
    case '(':
	return make_tok(l, TOKEN_LPAREN);
    case ')':
	return make_tok(l, TOKEN_RPAREN);
    case '{':
	return make_tok(l, TOKEN_LCURLY);
    case '}':
	return make_tok(l, TOKEN_RCURLY);
    case '.':
	if (l->e + 2 <= l->file->sz
	    && l->file->buf[l->e+1] == '.'
	    && l->file->buf[l->e+2] == '.') {
	    SKIPC(l);
	    SKIPC(l);
	    return make_tok(l, TOKEN_ELIPSIS);
	}
	return make_tok(l, TOKEN_PERIOD);
    case '-':
	if ((ec = lookahead_char(l)) == '>')
	    return make_tok(l, TOKEN_ARROW);
	else if (ec == '=')
	    return make_tok(l, TOKEN_SUB_ASSN);
	else if (ec == '-')
	    return make_tok(l, TOKEN_DEC);
	return make_tok_inplace(l, TOKEN_SUB);
    case '&':
	if ((ec = lookahead_char(l)) == '&')
	    return make_tok(l, TOKEN_BL_AND);
	else if (ec == '=')
	    return make_tok(l, TOKEN_AND_ASSN);
	return make_tok_inplace(l, TOKEN_BW_AND);
    case '*':
	if (lookahead_char(l) == '=')
	    return make_tok(l, TOKEN_MUL_ASSN);
	return make_tok_inplace(l, TOKEN_STAR);
    case '+':
	if ((ec = lookahead_char(l)) == '=')
	    return make_tok(l, TOKEN_ADD_ASSN);
	else if (ec == '+')
	    return make_tok(l, TOKEN_INC);
	return make_tok_inplace(l, TOKEN_ADD);
    case '/':
	if ((ec = lookahead_char(l)) == '=')
	    return make_tok(l, TOKEN_DIV_ASSN);
	if (ec == '/') {
	    skip_comment_line(l);
	    return get_tok(l);
	}
	return make_tok_inplace(l, TOKEN_DIV);
    case '~':
	return make_tok(l, TOKEN_BW_NEG);
    case '!':
	if ((ec = lookahead_char(l)) == '=')
	    return make_tok(l, TOKEN_NEQ);
	return make_tok_inplace(l, TOKEN_BL_NEG);
    case '%':
	if ((ec = lookahead_char(l)) == '=')
	    return make_tok(l, TOKEN_MOD_ASSN);
	return make_tok_inplace(l, TOKEN_MOD);
    case '<':
	if ((ec = lookahead_char(l)) == '=')
	    return make_tok(l, TOKEN_LTE);
	else if (ec == '<') {
	    if (lookahead_char(l) == '=')
		return make_tok(l, TOKEN_LSH_ASSN);
	    return make_tok_inplace(l, TOKEN_LSH);
	}
	return make_tok_inplace(l, TOKEN_LT);
    case '>':
	if ((ec = lookahead_char(l)) == '=')
	    return make_tok(l, TOKEN_GTE);
	else if (ec == '>') {
	    if (lookahead_char(l) == '=')
		return make_tok(l, TOKEN_RSH_ASSN);
	    return make_tok_inplace(l, TOKEN_RSH);
	}
	return make_tok_inplace(l, TOKEN_GT);
    case '=':
	if (lookahead_char(l) == '=')
	    return make_tok(l, TOKEN_DEQ);
	return make_tok_inplace(l, TOKEN_ASSN);
    case '^':
	if (lookahead_char(l) == '=')
	    return make_tok(l, TOKEN_XOR_ASSN);
	return make_tok_inplace(l, TOKEN_XOR);
    case '|':
	if ((ec = lookahead_char(l)) == '=')
	    return make_tok(l, TOKEN_OR_ASSN);
	else if (ec == '|')
	    return make_tok(l, TOKEN_BL_OR);
	return make_tok_inplace(l, TOKEN_BW_OR);
    case '?':
	return make_tok(l, TOKEN_QMARK);
    case ':':
	return make_tok(l, TOKEN_COLON);
    case ';':
	return make_tok(l, TOKEN_SEMICOLON);
    case ',':
	return make_tok(l, TOKEN_COMMA);
    case '#':
	if (lookahead_char(l) == '#')
	    return make_tok(l, TOKEN_DPOUND);
	return make_tok_inplace(l, TOKEN_POUND);
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
    [TOKEN_AUTO] = "TOKEN_AUTO",
    [TOKEN_BREAK] = "TOKEN_BREAK",
    [TOKEN_CASE] = "TOKEN_CASE",
    [TOKEN_CHAR] = "TOKEN_CHAR",
    [TOKEN_CONST] = "TOKEN_CONST",
    [TOKEN_CONTINUE] = "TOKEN_CONTINUE",
    [TOKEN_DEFAULT] = "TOKEN_DEFAULT",
    [TOKEN_DO] = "TOKEN_DO",
    [TOKEN_DOUBLE] = "TOKEN_DOUBLE",
    [TOKEN_ELSE] = "TOKEN_ELSE",
    [TOKEN_ENUM] = "TOKEN_ENUM",
    [TOKEN_EXTERN] = "TOKEN_EXTERN",
    [TOKEN_FLOAT] = "TOKEN_FLOAT",
    [TOKEN_FOR] = "TOKEN_FOR",
    [TOKEN_GOTO] = "TOKEN_GOTO",
    [TOKEN_IF] = "TOKEN_IF",
    [TOKEN_INLINE] = "TOKEN_INLINE",
    [TOKEN_INT] = "TOKEN_INT",
    [TOKEN_LONG] = "TOKEN_LONG",
    [TOKEN_REGISTER] = "TOKEN_REGISTER",
    [TOKEN_RESTRICT] = "TOKEN_RESTRICT",
    [TOKEN_RETURN] = "TOKEN_RETURN",
    [TOKEN_SHORT] = "TOKEN_SHORT",
    [TOKEN_SIGNED] = "TOKEN_SIGNED",
    [TOKEN_SIZEOF] = "TOKEN_SIZEOF",
    [TOKEN_STATIC] = "TOKEN_STATIC",
    [TOKEN_STRUCT] = "TOKEN_STRUCT",
    [TOKEN_SWITCH] = "TOKEN_SWITCH",
    [TOKEN_TYPEDEF] = "TOKEN_TYPEDEF",
    [TOKEN_UNION] = "TOKEN_UNION",
    [TOKEN_UNSIGNED] = "TOKEN_UNSIGNED",
    [TOKEN_VOID] = "TOKEN_VOID",
    [TOKEN_VOLATILE] = "TOKEN_VOLATILE",
    [TOKEN_WHILE] = "TOKEN_WHILE",
    [TOKEN_LBRACK] = "TOKEN_LBRACK",
    [TOKEN_RBRACK] = "TOKEN_RBRACK",
    [TOKEN_LPAREN] = "TOKEN_LPAREN",
    [TOKEN_RPAREN] = "TOKEN_RPAREN",
    [TOKEN_LCURLY] = "TOKEN_LCURLY",
    [TOKEN_RCURLY] = "TOKEN_RCURLY",
    [TOKEN_PERIOD] = "TOKEN_PERIOD",
    [TOKEN_ARROW] = "TOKEN_ARROW",
    [TOKEN_BW_AND] = "TOKEN_BW_AND",
    [TOKEN_STAR] = "TOKEN_STAR",
    [TOKEN_ADD] = "TOKEN_ADD",
    [TOKEN_SUB] = "TOKEN_SUB",
    [TOKEN_BW_NEG] = "TOKEN_BW_NEG",
    [TOKEN_BL_NEG] = "TOKEN_BL_NEG",
    [TOKEN_DIV] = "TOKEN_DIV",
    [TOKEN_MOD] = "TOKEN_MOD",
    [TOKEN_LSH] = "TOKEN_LSH",
    [TOKEN_RSH] = "TOKEN_RSH",
    [TOKEN_LT] = "TOKEN_LT",
    [TOKEN_GT] = "TOKEN_GT",
    [TOKEN_LTE] = "TOKEN_LTE",
    [TOKEN_GTE] = "TOKEN_GTE",
    [TOKEN_DEQ] = "TOKEN_DEQ",
    [TOKEN_NEQ] = "TOKEN_NEQ",
    [TOKEN_XOR] = "TOKEN_XOR",
    [TOKEN_INC] = "TOKEN_INC",
    [TOKEN_DEC] = "TOKEN_DEC",
    [TOKEN_BW_OR] = "TOKEN_BW_OR",
    [TOKEN_BL_AND] = "TOKEN_BL_AND",
    [TOKEN_BL_OR] = "TOKEN_BL_OR",
    [TOKEN_QMARK] = "TOKEN_QMARK",
    [TOKEN_COLON] = "TOKEN_COLON",
    [TOKEN_SEMICOLON] = "TOKEN_SEMICOLON",
    [TOKEN_ELIPSIS] = "TOKEN_ELIPSIS",
    [TOKEN_ASSN] = "TOKEN_ASSN",
    [TOKEN_MUL_ASSN] = "TOKEN_MUL_ASSN",
    [TOKEN_DIV_ASSN] = "TOKEN_DIV_ASSN",
    [TOKEN_MOD_ASSN] = "TOKEN_MOD_ASSN",
    [TOKEN_ADD_ASSN] = "TOKEN_ADD_ASSN",
    [TOKEN_SUB_ASSN] = "TOKEN_SUB_ASSN",
    [TOKEN_LSH_ASSN] = "TOKEN_LSH_ASSN",
    [TOKEN_RSH_ASSN] = "TOKEN_RSH_ASSN",
    [TOKEN_AND_ASSN] = "TOKEN_AND_ASSN",
    [TOKEN_XOR_ASSN] = "TOKEN_XOR_ASSN",
    [TOKEN_OR_ASSN] = "TOKEN_OR_ASSN",
    [TOKEN_COMMA] = "TOKEN_COMMA",
    [TOKEN_POUND] = "TOKEN_POUND",
    [TOKEN_DPOUND] = "TOKEN_DPOUND",
    [TOKEN_INTLIT] = "TOKEN_INTLIT",
    [TOKEN_FLOATLIT] = "TOKEN_FLOATLIT",
    [TOKEN_STR] = "TOKEN_STR",
    [TOKEN_CHARLIT] = "TOKEN_CHARLIT",
    [TOKEN_ID] = "TOKEN_ID",
    [TOKEN_EOF] = "TOKEN_EOF",
};

void lex_print(FILE *f, struct token tok) {
    fprintf(f, "'");
    print_range(f, &tok.pos);
    fprintf(f, "' : %s", tok_kind_name[tok.t]);

    if (tok.t == TOKEN_ID) {
	fprintf(f, " : (%zd, %zd) '", tok.v.id.s1, tok.v.id.s2);
	print_range(f, &tok.v.id);
	fprintf(f, "'");
    }
    fprintf(f, "\n");
}

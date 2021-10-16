#include <ctype.h>
#include <stdlib.h>

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

struct lexer *lex_new(struct src_file *file) {
        struct lexer *l = calloc(sizeof(struct lexer), 1);

        l->file = file;
        l->s = l->e = 0;
        l->peekf = false;

        return l;
}

void lex_free(struct lexer *l) { free(l); }

struct src_file *lex_get_file(struct lexer *l) {
        return l->file;
}

static void skip_whitespace(struct lexer *l) {
        int c;
        while (!IS_EOF(l)) {
                c = PEEKC(l);
                if (c == '\\') {
                        SKIPC(l);
                        if (PEEKC(l) == '\r') {
                                if (SKIPC(l), PEEKC(l) == '\n') {
                                        SKIPC(l);
                                }
                        } else {
                                RESET(l);
                                return;
                        }
                } else if (isspace(c))
                        SKIPC(l);
                else {
                        RESET(l);
                        return;
                }
        }
}

enum token_kind keyword_map_start = TOKEN_AUTO;
enum token_kind keyword_map_end = TOKEN_WHILE;

const char *keyword_map[] = {
    [TOKEN_AUTO] = "auto",
    [TOKEN_BREAK] = "break",
    [TOKEN_CASE] = "case",
    [TOKEN_CHAR] = "char",
    [TOKEN_CONST] = "const",
    [TOKEN_CONTINUE] = "continue",
    [TOKEN_DEFAULT] = "default",
    [TOKEN_DO] = "do",
    [TOKEN_DOUBLE] = "double",
    [TOKEN_ELSE] = "else",
    [TOKEN_ENUM] = "enum",
    [TOKEN_EXTERN] = "extern",
    [TOKEN_FLOAT] = "float",
    [TOKEN_FOR] = "for",
    [TOKEN_GOTO] = "goto",
    [TOKEN_IF] = "if",
    [TOKEN_INLINE] = "inline",
    [TOKEN_INT] = "int",
    [TOKEN_LONG] = "long",
    [TOKEN_REGISTER] = "register",
    [TOKEN_RESTRICT] = "restrict",
    [TOKEN_RETURN] = "return",
    [TOKEN_SHORT] = "short",
    [TOKEN_SIGNED] = "signed",
    [TOKEN_SIZEOF] = "sizeof",
    [TOKEN_STATIC] = "static",
    [TOKEN_STRUCT] = "struct",
    [TOKEN_SWITCH] = "switch",
    [TOKEN_TYPEDEF] = "typedef",
    [TOKEN_UNION] = "union",
    [TOKEN_UNSIGNED] = "unsigned",
    [TOKEN_VOID] = "void",
    [TOKEN_VOLATILE] = "volatile",
    [TOKEN_WHILE] = "while",
    [TOKEN_BOOL] = "_Bool",
    [TOKEN_COMPLEX] = "_Complex",
    [TOKEN_IMAGINARY] = "_Imaginary",

};

/* returns TOKEN_ID if not a keyword, otherwise the token variant */
static enum token_kind find_keywords(struct lexer *l) {
        for (size_t i = keyword_map_start; i < keyword_map_end; i++) {
                if (strneq(keyword_map[i], strlen(keyword_map[i]),
                           (const char *)l->file->buf + l->s, l->e - l->s)) {
                        return (enum token_kind)i;
                }
        }
        return TOKEN_ID;
}

static struct src_range make_range(struct lexer *l) {
        struct src_range rng;
        rng.f = l->file;
        rng.s1 = l->s, rng.s2 = l->e;
        return rng;
}

static struct token make_tok(struct lexer *l, enum token_kind k) {
        SKIPC(l);
        struct token t;
        t.t = k;
        t.pos = make_range(l);
        return t;
}

static struct token make_tok_inplace(struct lexer *l, enum token_kind k) {
        struct token t;
        t.t = k;
        t.pos = make_range(l);
        return t;
}

static struct token lex_id(struct lexer *l) {
        int c;
        while (!IS_EOF(l) &&
               (isalpha(c = PEEKC(l)) || isdigit(c) || c == '_')) {
                SKIPC(l);
        }
        struct token ret = make_tok_inplace(l, find_keywords(l));
        if (ret.t == TOKEN_ID)
                ret.v.id = make_range(l);
        return ret;
}

#define IS_DECINT(c) (c <= '9' && c >= '0')
#define IS_HEXINT(c)                                                           \
        ((c <= 'f' && c >= 'a') || (c <= 'F' && c >= 'A') || IS_DECINT(c))
#define IS_OCTINT(c) (c <= '7' && c >= '0')

static void lex_int_suffix(struct lexer *l, uint8_t *sign,
                           enum intlit_type *t) {
        int c;
        if (IS_EOF(l)) {
                *sign = true;
                *t = INTLIT_I;
        } else if ((c = PEEKC(l)) == 'u' || c == 'U') {
                *sign = false;
                SKIPC(l);
                if ((c = PEEKC(l)) == 'l' || c == 'L') {
                        *t = INTLIT_L;
                        SKIPC(l);
                        if ((c = PEEKC(l)) == 'l' || c == 'L') {
                                SKIPC(l);
                                *t = INTLIT_LL;
                        }
                }
        } else {
                *t = INTLIT_I;
        }
}

static int char_to_hex(char c) {
        if (c <= '9' && c >= '0')
                return c - '0';
        if (c <= 'f' && c >= 'a')
                return (c - 'a') + 10;
        if (c <= 'F' && c >= 'A') {
                return (c - 'A') + 10;
        }
        return 0;
}

static struct token lex_octint(struct lexer *l) {
        size_t s = l->e;
        while (!IS_EOF(l) && IS_OCTINT(PEEKC(l))) {
                SKIPC(l);
        }
        uint8_t sign;
        enum intlit_type t;
        size_t e = l->e;

        lex_int_suffix(l, &sign, &t);

        struct token tok = make_tok_inplace(l, TOKEN_INTLIT);
        tok.v.num.sign = sign;
        tok.v.num.t = t;

        if (sign) {
                int64_t total = 0;
                for (size_t i = s; i < e; i++) {
                        total = total * 8 + l->file->buf[i] - '0';
                }
                tok.v.num.v.s = total;
        } else {
                uint64_t total = 0;
                for (size_t i = s; i < e; i++) {
                        total = total * 8 + l->file->buf[i] - '0';
                }
                tok.v.num.v.u = total;
        }
        return tok;
}

static struct token lex_hexint(struct lexer *l) {
        SKIPC(l);
        size_t s = l->e;
        while (!IS_EOF(l) && IS_HEXINT(PEEKC(l))) {
                SKIPC(l);
        }
        uint8_t sign;
        enum intlit_type t;
        size_t e = l->e;

        lex_int_suffix(l, &sign, &t);

        struct token tok = make_tok_inplace(l, TOKEN_INTLIT);
        tok.v.num.sign = sign;
        tok.v.num.t = t;

        if (sign) {
                int64_t total = 0;
                for (size_t i = s; i < e; i++) {
                        total = total * 16 + char_to_hex(l->file->buf[i]);
                }
                tok.v.num.v.s = total;
        } else {
                uint64_t total = 0;
                for (size_t i = s; i < e; i++) {
                        total = total * 16 + char_to_hex(l->file->buf[i]);
                }
                tok.v.num.v.u = total;
        }
        return tok;
}

static struct token lex_decint(struct lexer *l) {
        SKIPC(l); // skip first 1-9
        while (!IS_EOF(l) && IS_DECINT(PEEKC(l))) {
                SKIPC(l);
        }
        size_t e = l->e;
        uint8_t sign;
        enum intlit_type t;
        lex_int_suffix(l, &sign, &t);

        struct token tok = make_tok_inplace(l, TOKEN_INTLIT);
        tok.v.num.sign = sign;
        tok.v.num.t = t;

        if (sign) {
                int64_t total = 0;
                for (size_t i = l->s; i < e; i++) {
                        total = total * 10 + (l->file->buf[i] - '0');
                }
                tok.v.num.v.s = total;
        } else {
                uint64_t total = 0;
                for (size_t i = l->s; i < e; i++) {
                        total = total * 10 + (l->file->buf[i] - '0');
                }
                tok.v.num.v.u = total;
        }
        return tok;
}

static int read_char_escape(size_t *move, const char *p) {
        (*move)++;
        int c = *p;
        switch (c) {
        case '\'':
        case '\"':
        case '\\':
        case '?':
                return c;
        case '0':
                return 0;
        case 'a':
                return '\a';
        case 'b':
                return '\b';
        case 't':
                return '\t';
        case 'n':
                return '\n';
        case 'v':
                return '\v';
        case 'f':
                return '\f';
        case 'r':
                return '\r';
        }
        printf("invalid escape char '%c'\n", c);
        exit(EXIT_FAILURE);
}

static struct token lex_charlit(struct lexer *l) {
        int64_t c_val;
        SKIPC(l); // skip '
        int c = NEXTC(l);
        if (c == '\\')
                c_val =
                    read_char_escape(&l->e, (const char *)l->file->buf + l->e);
        else
                c_val = c;

        if (PEEKC(l) != '\'') {
                printf("expected \' after char literal\n");
                exit(EXIT_FAILURE);
        }

        struct token tok = make_tok(l, TOKEN_INTLIT);
        tok.v.num.sign = true;
        tok.v.num.t = INTLIT_I;

        tok.v.num.v.s = c_val;
        return tok;
}

static void find_strlit_end(struct lexer *l) {
        int c;
        for (; (c = NEXTC(l)) != '"';) {
                if (c == '\\')
                        SKIPC(l);
        }
}

static struct token lex_str(struct lexer *l) {
        SKIPC(l); // skip "
        const char *start = (const char *)l->file->buf + l->e;
        find_strlit_end(l);
        const char *end = (const char *)l->file->buf + l->e - 1;

        char *buf = calloc(end - start, sizeof(char));
        size_t pos = 0;

        for (; start < end;) {
                if (*start == '\\') {
                        size_t move = 1;
                        buf[pos++] = read_char_escape(&move, start + 1);
                        start += move;
                } else
                        buf[pos++] = *start++;
        }

        struct token tok = make_tok_inplace(l, TOKEN_STR);
        tok.v.str.buf = buf;
        tok.v.str.len = pos;
        return tok;
}

static int lookahead_char(struct lexer *l) {
        SKIPC(l);
        if (IS_EOF(l))
                return -1;
        return PEEKC(l);
}

static void skip_comment_line(struct lexer *l) {
        while (!IS_EOF(l) && PEEKC(l) != '\n')
                SKIPC(l);
}

static struct token get_tok(struct lexer *l) {
        skip_whitespace(l);

        int c = PEEKC(l);
        int ec;

        if (IS_EOF(l)) {
                return make_tok(l, TOKEN_EOF);
        }

        if (c == '\'')
                return lex_charlit(l);

        if (c == '\"')
                return lex_str(l);

        if (isalpha(c) || c == '_') {
                return lex_id(l);
        }

        if (c == '0') {
                SKIPC(l);
                if (!IS_EOF(l) && ((ec = PEEKC(l)) == 'x' || ec == 'X')) {
                        return lex_hexint(l);
                }
                return lex_octint(l);
        }

        if (IS_DECINT(c)) {
                return lex_decint(l);
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
                if (l->e + 2 <= l->file->sz && l->file->buf[l->e + 1] == '.' &&
                    l->file->buf[l->e + 2] == '.') {
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

struct token lex_next(struct lexer *l) {
        if (l->peekf) {
                l->peekf = false;
                return l->peek;
        }
        return get_tok(l);
}

struct token lex_peek(struct lexer *l) {
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

        if (tok.t == TOKEN_INTLIT && tok.v.num.sign) {
                fprintf(f, " : %ld", tok.v.num.v.s);
        } else if (tok.t == TOKEN_INTLIT && !tok.v.num.sign) {
                fprintf(f, " : %lu", tok.v.num.v.u);
        }

        if (tok.t == TOKEN_ID) {
                fprintf(f, " : (%zd, %zd) '", tok.v.id.s1, tok.v.id.s2);
                print_range(f, &tok.v.id);
                fprintf(f, "'");
        }
        if (tok.t == TOKEN_STR) {
                fprintf(f, " : \"%.*s\"", (int)tok.v.str.len, tok.v.str.buf);
        }
        fprintf(f, "\n");
}

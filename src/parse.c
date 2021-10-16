#include <stdlib.h>

#include "parse.h"

#define NEXTT(p) (lex_next(p->lex))
#define SKIPT(p) (lex_skip(p->lex))
#define PEEKT(p) (lex_peek(p->lex))

struct parser {
        struct lexer *lex;
	struct symtab *scope;
};

static enum num_type intlit_to_num[] = {
    [INTLIT_I] = NUM_INT,
    [INTLIT_L] = NUM_LONG,
    [INTLIT_LL] = NUM_LONG_LONG,
};

static void fill_num_expr(struct num *num, struct tok_num *tnum) {
        num->type = intlit_to_num[tnum->t];
        num->sign = tnum->sign ? NUM_SIGNED : NUM_UNSIGNED;

        if (tnum->sign) {
                num->sign = NUM_SIGNED;
                num->v.i = tnum->v.s;
        } else {
                num->sign = NUM_UNSIGNED;
                num->v.u = tnum->v.u;
        }
}

static struct src_range skipr(struct parser *p, enum token_kind t,
                              const char *str) {
        struct token tok = NEXTT(p);
        if (tok.t != t) {
                printf("error: expected token '%s'\n", str);
                exit(EXIT_FAILURE);
        }
        return tok.pos;
}

static struct token skip(struct parser *p, enum token_kind t, const char *str) {
        struct token tok = NEXTT(p);
        if (tok.t != t) {
                printf("error: expected token '%s'\n", str);
                exit(EXIT_FAILURE);
        }
        return tok;
}

static struct expr *parse_expr(struct parser *p);

static struct expr *parse_primary(struct parser *p) {
        struct expr *expr;
        struct token tok = NEXTT(p);
        switch (tok.t) {
        case TOKEN_INTLIT:
                expr = build_expr(EXPR_NUM, tok.pos);
                fill_num_expr(&expr->v.num, &tok.v.num);
                break;
        case TOKEN_ID:
                expr = build_expr(EXPR_VAR, tok.pos);
		expr->v.var = find_var(p->scope, tok.v.id);
		if (expr->v.var == NULL) {
			printf("cannot find variable '");
			print_range(stdout, &tok.v.id);
			printf("' in scope\n");
			exit(EXIT_FAILURE);
		}
                break;
        case TOKEN_STR:
                expr = build_expr(EXPR_STR, tok.pos);
                expr->v.str.buf = (const uint8_t *)tok.v.str.buf;
                expr->v.str.len = tok.v.str.len;
                break;
        case TOKEN_LPAREN:
                expr = parse_expr(p);
                SKIPT(p);
                break;
        default:
                printf("expected expression\n");
                exit(EXIT_FAILURE);
        }
        return expr;
}

static struct expr *parse_assign(struct parser *p);

static struct expr *parse_postfix(struct parser *p) {
        struct expr *ret = parse_primary(p);

        struct token t;
        while ((t = PEEKT(p)).t == TOKEN_INC || t.t == TOKEN_DEC ||
               t.t == TOKEN_LBRACK || t.t == TOKEN_PERIOD ||
               t.t == TOKEN_ARROW || t.t == TOKEN_LPAREN) {
                struct expr *new;
                switch (t.t) {
                case TOKEN_INC:
                        SKIPT(p);
                        new = build_expr(EXPR_SUF_INC,
                                         combine_ranges(ret->pos, t.pos));
                        new->v.unary = ret;
                        ret = new;
                        break;
                case TOKEN_DEC:
                        SKIPT(p);
                        new = build_expr(EXPR_SUF_DEC,
                                         combine_ranges(ret->pos, t.pos));
                        new->v.unary = ret;
                        ret = new;
                        break;
                case TOKEN_LBRACK: {
                        SKIPT(p);
                        struct expr *idx = parse_expr(p);
                        struct src_range end = skipr(p, TOKEN_RBRACK, "]");
                        new = build_expr(EXPR_SUBSCRIPT,
                                         combine_ranges(ret->pos, end));
                        new->v.subscript.arr = ret;
                        new->v.subscript.idx = idx;
                        ret = new;
                        break;
                }
                case TOKEN_PERIOD: {
                        SKIPT(p);
                        struct token member = NEXTT(p);
                        new = build_expr(EXPR_MEMBER,
                                         combine_ranges(ret->pos, member.pos));
                        new->v.member.val = ret;
                        new->v.member.member.name = member.v.id;
                        ret = new;
                        break;
                }
                case TOKEN_ARROW: {
                        SKIPT(p);
                        struct token member = NEXTT(p);
                        new = build_expr(EXPR_PTR_MEMBER,
                                         combine_ranges(ret->pos, member.pos));
                        new->v.member.val = ret;
                        new->v.member.member.name = member.v.id;
                        ret = new;
                        break;
                }
                case TOKEN_LPAREN: {
                        SKIPT(p);
                        struct vec args;
                        vec_init(&args);
                        struct token last_paren;
                        while ((last_paren = PEEKT(p)).t != TOKEN_RPAREN) {
                                struct expr *val = parse_assign(p);
                                VEC_PUSH(&args, &val, struct expr *);
                                if (PEEKT(p).t == TOKEN_COMMA) {
                                        SKIPT(p);
                                }
                        }
                        SKIPT(p);
                        new = build_expr(
                            EXPR_FUNCALL,
                            combine_ranges(ret->pos, last_paren.pos));
                        new->v.funcall.fun = ret;
                        new->v.funcall.args = args;
                        ret = new;
                        break;
                };
                default:
                        break;
                }
        }
        return ret;
}

static enum unop_op token_to_unop[] = {
    [TOKEN_ADD] = UNOP_POS,
    [TOKEN_SUB] = UNOP_NEG,
    [TOKEN_BW_NEG] = UNOP_BW_NOT,
    [TOKEN_BL_NEG] = UNOP_NOT,
};

static struct expr *parse_prefix(struct parser *p) {
        struct expr *ret;
        struct expr *tmp;
        struct token t = PEEKT(p);
        switch (t.t) {
        case TOKEN_INC:
                SKIPT(p);
                tmp = parse_prefix(p);
                ret = build_expr(EXPR_PRE_INC, combine_ranges(t.pos, tmp->pos));
                ret->v.unary = tmp;
                return ret;
        case TOKEN_DEC:
                SKIPT(p);
                tmp = parse_prefix(p);
                ret = build_expr(EXPR_PRE_DEC, combine_ranges(t.pos, tmp->pos));
                ret->v.unary = tmp;
                return ret;
        case TOKEN_STAR:
                SKIPT(p);
                tmp = parse_prefix(p);
                ret = build_expr(EXPR_DEREF, combine_ranges(t.pos, tmp->pos));
                ret->v.unary = tmp;
                return ret;
        case TOKEN_BW_AND:
                SKIPT(p);
                tmp = parse_prefix(p);
                ret = build_expr(EXPR_ADDR, combine_ranges(t.pos, tmp->pos));
                ret->v.unary = tmp;
                return ret;
        case TOKEN_ADD:
        case TOKEN_SUB:
        case TOKEN_BL_NEG:
        case TOKEN_BW_NEG: {
                enum unop_op op = token_to_unop[NEXTT(p).t];
                tmp = parse_prefix(p);
                ret = build_expr(EXPR_UNOP, combine_ranges(t.pos, tmp->pos));
                ret->v.unop.op = op;
                ret->v.unop.val = tmp;
                return ret;
        }
        default:
                return parse_postfix(p);
        }
}

static enum binop_op token_to_binop[] = {
    [TOKEN_BL_AND] = BINOP_AND,    [TOKEN_BL_OR] = BINOP_OR,
    [TOKEN_BW_OR] = BINOP_BW_OR,   [TOKEN_XOR] = BINOP_BW_XOR,
    [TOKEN_BW_AND] = BINOP_BW_AND, [TOKEN_DEQ] = BINOP_EQ,
    [TOKEN_NEQ] = BINOP_NEQ,       [TOKEN_ADD] = BINOP_ADD,
    [TOKEN_SUB] = BINOP_SUB,       [TOKEN_STAR] = BINOP_MUL,
    [TOKEN_DIV] = BINOP_DIV,       [TOKEN_MOD] = BINOP_MOD,
    [TOKEN_LSH] = BINOP_LSH,       [TOKEN_RSH] = BINOP_RSH,
    [TOKEN_LT] = BINOP_LT,         [TOKEN_LTE] = BINOP_LTE,
    [TOKEN_GT] = BINOP_GT,         [TOKEN_GTE] = BINOP_GTE,
};

static struct expr *parse_multiplicative(struct parser *p) {
        struct expr *ret = parse_prefix(p);

        struct token t;
        while ((t = PEEKT(p)).t == TOKEN_STAR || t.t == TOKEN_DIV ||
               t.t == TOKEN_MOD) {
                enum binop_op op = token_to_binop[NEXTT(p).t];
                struct expr *right = parse_prefix(p);
                struct expr *new_expr = build_expr(
                    EXPR_BINOP, combine_ranges(right->pos, right->pos));
                new_expr->v.binop.l = ret;
                new_expr->v.binop.r = right;
                new_expr->v.binop.op = op;
                ret = new_expr;
        }

        return ret;
}

static struct expr *parse_additive(struct parser *p) {
        struct expr *ret = parse_multiplicative(p);

        struct token t;
        while ((t = PEEKT(p)).t == TOKEN_ADD || t.t == TOKEN_SUB) {
                enum binop_op op = token_to_binop[NEXTT(p).t];
                struct expr *right = parse_multiplicative(p);
                struct expr *new_expr = build_expr(
                    EXPR_BINOP, combine_ranges(right->pos, right->pos));
                new_expr->v.binop.l = ret;
                new_expr->v.binop.r = right;
                new_expr->v.binop.op = op;
                ret = new_expr;
        }

        return ret;
}

static struct expr *parse_bitshift(struct parser *p) {
        struct expr *ret = parse_additive(p);

        struct token t;
        while ((t = PEEKT(p)).t == TOKEN_LSH || t.t == TOKEN_RSH) {
                enum binop_op op = token_to_binop[NEXTT(p).t];
                struct expr *right = parse_additive(p);
                struct expr *new_expr = build_expr(
                    EXPR_BINOP, combine_ranges(right->pos, right->pos));
                new_expr->v.binop.l = ret;
                new_expr->v.binop.r = right;
                new_expr->v.binop.op = op;
                ret = new_expr;
        }
        return ret;
}

static struct expr *parse_relational(struct parser *p) {
        struct expr *ret = parse_bitshift(p);

        struct token t;
        while ((t = PEEKT(p)).t == TOKEN_LT || t.t == TOKEN_LTE ||
               t.t == TOKEN_GT || t.t == TOKEN_GTE) {
                enum binop_op op = token_to_binop[NEXTT(p).t];
                struct expr *right = parse_bitshift(p);
                struct expr *new_expr = build_expr(
                    EXPR_BINOP, combine_ranges(right->pos, right->pos));
                new_expr->v.binop.l = ret;
                new_expr->v.binop.r = right;
                new_expr->v.binop.op = op;
                ret = new_expr;
        }

        return ret;
}

static struct expr *parse_equality(struct parser *p) {
        struct expr *ret = parse_relational(p);

        struct token t;
        while ((t = PEEKT(p)).t == TOKEN_DEQ || t.t == TOKEN_NEQ) {
                enum binop_op op = token_to_binop[NEXTT(p).t];
                struct expr *right = parse_relational(p);
                struct expr *new_expr = build_expr(
                    EXPR_BINOP, combine_ranges(right->pos, right->pos));
                new_expr->v.binop.l = ret;
                new_expr->v.binop.r = right;
                new_expr->v.binop.op = op;
                ret = new_expr;
        }

        return ret;
}

static struct expr *parse_bitwise_and(struct parser *p) {
        struct expr *ret = parse_equality(p);

        struct token t;
        while ((t = PEEKT(p)).t == TOKEN_BW_AND) {
                enum binop_op op = token_to_binop[NEXTT(p).t];
                struct expr *right = parse_equality(p);
                struct expr *new_expr = build_expr(
                    EXPR_BINOP, combine_ranges(right->pos, right->pos));
                new_expr->v.binop.l = ret;
                new_expr->v.binop.r = right;
                new_expr->v.binop.op = op;
                ret = new_expr;
        }

        return ret;
}

static struct expr *parse_bitwise_xor(struct parser *p) {
        struct expr *ret = parse_bitwise_and(p);

        struct token t;
        while ((t = PEEKT(p)).t == TOKEN_XOR) {
                enum binop_op op = token_to_binop[NEXTT(p).t];
                struct expr *right = parse_bitwise_and(p);
                struct expr *new_expr = build_expr(
                    EXPR_BINOP, combine_ranges(right->pos, right->pos));
                new_expr->v.binop.l = ret;
                new_expr->v.binop.r = right;
                new_expr->v.binop.op = op;
                ret = new_expr;
        }

        return ret;
}

static struct expr *parse_bitwise_or(struct parser *p) {
        struct expr *ret = parse_bitwise_xor(p);

        struct token t;
        while ((t = PEEKT(p)).t == TOKEN_BW_OR) {
                enum binop_op op = token_to_binop[NEXTT(p).t];
                struct expr *right = parse_bitwise_xor(p);
                struct expr *new_expr = build_expr(
                    EXPR_BINOP, combine_ranges(right->pos, right->pos));
                new_expr->v.binop.l = ret;
                new_expr->v.binop.r = right;
                new_expr->v.binop.op = op;
                ret = new_expr;
        }

        return ret;
}

static struct expr *parse_and(struct parser *p) {
        struct expr *ret = parse_bitwise_or(p);

        struct token t;
        while ((t = PEEKT(p)).t == TOKEN_BL_AND) {
                enum binop_op op = token_to_binop[NEXTT(p).t];
                struct expr *right = parse_bitwise_or(p);
                struct expr *new_expr = build_expr(
                    EXPR_BINOP, combine_ranges(right->pos, right->pos));
                new_expr->v.binop.l = ret;
                new_expr->v.binop.r = right;
                new_expr->v.binop.op = op;
                ret = new_expr;
        }

        return ret;
}

static struct expr *parse_or(struct parser *p) {
        struct expr *ret = parse_and(p);

        struct token t;
        while ((t = PEEKT(p)).t == TOKEN_BL_OR) {
                enum binop_op op = token_to_binop[NEXTT(p).t];
                struct expr *right = parse_and(p);
                struct expr *new_expr = build_expr(
                    EXPR_BINOP, combine_ranges(right->pos, right->pos));
                new_expr->v.binop.l = ret;
                new_expr->v.binop.r = right;
                new_expr->v.binop.op = op;
                ret = new_expr;
        }

        return ret;
}

static struct expr *parse_cond(struct parser *p) {
        struct expr *cond = parse_or(p);
        if (PEEKT(p).t == TOKEN_QMARK) {
                SKIPT(p);
                struct expr *t = parse_expr(p);
                skipr(p, TOKEN_COLON, ":");
                struct expr *f = parse_cond(p);
                struct expr *ret =
                    build_expr(EXPR_COND, combine_ranges(cond->pos, f->pos));
                ret->v.cond.cond = cond;
                ret->v.cond.t = t;
                ret->v.cond.f = f;
                return ret;
        }
        return cond;
}

static enum assn_op token_to_assn[] = {
    [TOKEN_ASSN] = ASSN,         [TOKEN_ADD_ASSN] = ASSN_ADD,
    [TOKEN_SUB_ASSN] = ASSN_SUB, [TOKEN_MUL_ASSN] = ASSN_MUL,
    [TOKEN_DIV_ASSN] = ASSN_DIV, [TOKEN_MOD_ASSN] = ASSN_MOD,
    [TOKEN_LSH_ASSN] = ASSN_LSH, [TOKEN_RSH_ASSN] = ASSN_RSH,
    [TOKEN_AND_ASSN] = ASSN_AND, [TOKEN_OR_ASSN] = ASSN_OR,
    [TOKEN_XOR_ASSN] = ASSN_XOR,
};

static struct expr *parse_assign(struct parser *p) {
        struct expr *lhs = parse_cond(p);

        switch (PEEKT(p).t) {
        case TOKEN_ASSN:
        case TOKEN_ADD_ASSN:
        case TOKEN_SUB_ASSN:
        case TOKEN_MUL_ASSN:
        case TOKEN_DIV_ASSN:
        case TOKEN_MOD_ASSN:
        case TOKEN_LSH_ASSN:
        case TOKEN_RSH_ASSN:
        case TOKEN_AND_ASSN:
        case TOKEN_XOR_ASSN:
        case TOKEN_OR_ASSN: {
                enum assn_op op = token_to_assn[NEXTT(p).t];
                struct expr *rhs = parse_assign(p);
                struct expr *ret =
                    build_expr(EXPR_ASSN, combine_ranges(lhs->pos, rhs->pos));
                ret->v.assn.lhs = lhs;
                ret->v.assn.rhs = rhs;
                ret->v.assn.op = op;
                return ret;
        }
        default:
                return lhs;
        }
}
static struct expr *parse_comma(struct parser *p) {
        struct expr *lhs = parse_assign(p);
        if (PEEKT(p).t == TOKEN_COMMA) {
                SKIPT(p);
                struct expr *rhs = parse_expr(p);
                struct expr *ret =
                    build_expr(EXPR_COMMA, combine_ranges(lhs->pos, rhs->pos));
                ret->v.comma.lhs = lhs;
                ret->v.comma.rhs = rhs;
                return ret;
        }
        return lhs;
}

static struct expr *parse_expr(struct parser *p) { return parse_comma(p); }

static bool is_type_next(struct parser *p) {
        struct token t = PEEKT(p);
        switch (t.t) {
        case TOKEN_VOID:
        case TOKEN_INT:
        case TOKEN_CHAR:
        case TOKEN_SHORT:
        case TOKEN_LONG:
        case TOKEN_FLOAT:
        case TOKEN_DOUBLE:
        case TOKEN_STRUCT:
        case TOKEN_UNION:
        case TOKEN_ENUM:
        case TOKEN_STATIC:
        case TOKEN_SIGNED:
        case TOKEN_UNSIGNED:
        case TOKEN_CONST:
        case TOKEN_AUTO:
                return true;
        default:
                return false;
        }
}

static struct type *parse_type(struct parser *p) {
	enum {
		VOID = 1 << 1,
		CHAR = 1 << 3,
		SHORT = 1 << 4,
		INT = 1 << 5, 
		LONG = 1 << 6,
		UNSIGNED = 1 << 7,
	};

	int builtins = 0;
	
        struct type *ret = type_int;

	while (is_type_next(p)) {
		struct token t = NEXTT(p);
		switch (t.t) {
		case TOKEN_VOID:
			builtins |= VOID;
			break;
		case TOKEN_CHAR:
			builtins |= CHAR;
			break;
		case TOKEN_SHORT:
			builtins |= SHORT;
			break;
		case TOKEN_INT:
			builtins |= INT;
			break;
		case TOKEN_LONG:
			builtins |= LONG;
			break;
		case TOKEN_UNSIGNED:
			builtins |= UNSIGNED;
			break;
		default:
			printf("invalid type name");
			exit(EXIT_FAILURE);
		}

		switch (builtins) {
		case VOID:
			ret = type_void;
			break;
		case CHAR + UNSIGNED:
			ret = type_uchar;
			break;
		case SHORT + UNSIGNED:
			ret = type_ushort;
			break;
		case INT + UNSIGNED:
			ret = type_uint;
			break;
		case LONG + UNSIGNED:
			ret = type_ulong;
			break;
		case CHAR:
			ret = type_char;
			break;
		case SHORT:
			ret = type_short;
			break;
		case INT:
			ret = type_int;
			break;
		case LONG:
			ret = type_long;
			break;
		case LONG + INT:
			ret = type_long;
			break;
		case LONG + INT + UNSIGNED:
			ret = type_ulong;
			break;
		case UNSIGNED:
			ret = type_uint;
			break;
		default:
			printf("invalid builtin type");
			exit(EXIT_FAILURE);
		}
	}
	
        return ret;
}

static void parse_decl(struct vec *stmts, struct parser *p) {
        struct type *base_type = parse_type(p);

	struct token t = PEEKT(p);
	while (t.t != TOKEN_SEMICOLON) {
		struct type *typ = base_type;

		if (PEEKT(p).t == TOKEN_STAR) {
			typ = clone_type(base_type);
		}
		
		while (PEEKT(p).t == TOKEN_STAR) {
			SKIPT(p);
			
			typ = build_ptr(typ);
		};
		struct token sym = skip(p, TOKEN_ID, "variable name");
		struct var_ref *ref = add_var(p->scope, sym.v.id);
		ref->type = typ;

		t = PEEKT(p);
		if (t.t == TOKEN_ASSN) {
			SKIPT(p);

			struct expr *init = parse_assign(p);	
		
			struct expr *var = build_expr(EXPR_VAR, sym.pos);
			var->v.var = ref;
			
			struct expr *real_expr = build_expr(EXPR_ASSN, combine_ranges(sym.pos, init->pos));	
			real_expr->v.assn.op = ASSN;
			real_expr->v.assn.lhs = var;
			real_expr->v.assn.rhs = init;
			struct stmt *new_stmt = build_stmt(STMT_EXPR, combine_ranges(sym.pos, init->pos));
			new_stmt->v.expr = real_expr;
			VEC_PUSH(stmts, &new_stmt, struct stmt*);
			t = PEEKT(p);
		}

		if ((t = PEEKT(p)).t != TOKEN_COMMA && t.t != TOKEN_SEMICOLON) {
			printf("expected ',' or ';' after name\n");
			exit(EXIT_FAILURE);
		}
		if (t.t == TOKEN_COMMA) {
			t = NEXTT(p);
		}
	}
	SKIPT(p);
}

static struct stmt *parse_stmt(struct parser *p) {
        struct token t = PEEKT(p);
        struct stmt *ret;
        switch (t.t) {
        case TOKEN_SEMICOLON:
                ret = build_stmt(STMT_NULL, t.pos);
                break;
        case TOKEN_RETURN: {
                SKIPT(p);
                struct expr *val = parse_expr(p);
                struct src_range last = skipr(p, TOKEN_SEMICOLON, ";");
                ret = build_stmt(STMT_RETURN, combine_ranges(t.pos, last));
                ret->v.ret = val;
                break;
        }
        case TOKEN_IF: {
                SKIPT(p);
                skipr(p, TOKEN_LPAREN, "(");
                struct expr *cond = parse_expr(p);
                skipr(p, TOKEN_RPAREN, ")");
                struct stmt *tru = parse_stmt(p);

                if (PEEKT(p).t == TOKEN_ELSE) {
                        SKIPT(p);
                        struct stmt *fal = parse_stmt(p);
                        ret = build_stmt(STMT_IF,
                                         combine_ranges(t.pos, fal->pos));
                        ret->v._if.f = fal;
                } else {
                        ret = build_stmt(STMT_IF,
                                         combine_ranges(t.pos, tru->pos));
                }
                ret->v._if.t = tru;
                ret->v._if.cond = cond;
                break;
        }
        case TOKEN_LCURLY: {
                SKIPT(p);
                struct vec block;
                vec_init(&block);
		struct symtab new_scope;
		init_symtab(&new_scope, p->scope);
		p->scope = &new_scope;
                while (PEEKT(p).t != TOKEN_RCURLY) {
                        if (is_type_next(p)) {
				parse_decl(&block, p);
                        } else {
                                struct stmt *tmp = parse_stmt(p);
				VEC_PUSH(&block, &tmp, struct stmt*);

                        }
                }
                struct token last = NEXTT(p);
                ret = build_stmt(STMT_BLOCK, combine_ranges(t.pos, last.pos));
                ret->v.block.items = block;
		ret->v.block.vars = new_scope;
		p->scope = p->scope->up;
                break;
        }
	case TOKEN_DEFAULT: {
		SKIPT(p);
		skip(p, TOKEN_COLON, ":");
		struct stmt *next = parse_stmt(p);
		ret = build_stmt(STMT_DEFAULT, combine_ranges(t.pos, next->pos));
		ret->v._default = next;
		break;
		
	}
        default: {
                struct expr *val = parse_expr(p);
                struct token last = NEXTT(p);
                if (last.t != TOKEN_SEMICOLON) {
                        printf("error: expected ';'");
                        exit(EXIT_FAILURE);
                }
                ret = build_stmt(STMT_EXPR, combine_ranges(val->pos, last.pos));
                ret->v.expr = val;
                break;
        }
        }
        return ret;
}

struct trans_unit parse_translation_unit(struct lexer *lex) {
        struct parser p;
	struct symtab global;
	init_symtab(&global, NULL);

        p.lex = lex;
	p.scope = &global;
	struct trans_unit tunit;
        tunit.file = lex_get_file(lex);
        tunit.stmt = parse_stmt(&p);

        return tunit;
}

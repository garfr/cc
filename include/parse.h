#ifndef CC_PARSER_H
#define CC_PARSER_H

#include "ast.h"
#include "lex.h"

struct trans_unit parse_translation_unit(struct lexer *lex);

#endif

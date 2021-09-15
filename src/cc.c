#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>

#include "arg.h"
#include "helpers.h"
#include "lex.h"

int main(int argc, const char **argv) {
    struct arg_flag version_flag = {
	.flag = "v",
	.desc = "prints compiler version",
	.flag_name = "version"
    };
    struct arg_flag input_flag = {
	.flag = "f",
	.desc = "specifies input file to the compiler",
	.flag_name = "input file",
	.arg_t = ARG_STRING,
	.arg_lvl = ARG_REQUIRED,
    };

    struct arg_flag *opts[] = {
	&version_flag, &input_flag, NULL
    };

    struct arg_flag *err;
    if (!arg_parse(argc, argv, opts, &err)) {
	printf("error parsing command line flag\n");
	return EXIT_FAILURE;
    }

    if (version_flag.found) {
	printf("cc - a project c compiler - v0.1\n");
	return EXIT_SUCCESS;
    }

    if (!input_flag.found) {
	printf("no filename specified\n");
	return EXIT_FAILURE;
    }

    struct src_file comp_unit;
    if (!src_file_open(input_flag.arg.str, &comp_unit)) {
	printf("couldn't find file '%s'\n", input_flag.arg.str);
	return EXIT_FAILURE;
    }

    struct lexer *lex = lex_new(&comp_unit);

    struct token tok;
    while ((tok = lex_next(lex)).t != TOKEN_EOF) {
	lex_print(stdout, tok);
    }

    lex_free(lex);

    printf("%s", comp_unit.buf);
    return EXIT_SUCCESS;
}

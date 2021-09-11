#include <stdio.h>
#include <stdlib.h>

#include "arg.h"

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
    }

    if (input_flag.found) {
	printf("opening file %s\n", input_flag.arg.str);
    }
    
    return EXIT_SUCCESS;
}

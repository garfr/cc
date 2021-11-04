#ifndef CC_ARG_H
#define CC_ARG_H

/* arg.h - parses declaratively specified command line arguments
 *
 * expected options should be placed in an array, ending with NULL and passed
 * to arg_parse, the results will be filled into the option structs
 */

#include "stdbool.h"
#include "stddef.h"
#include "stdint.h"

enum arg_lvl {
        ARG_NONE = 0,
        ARG_REQUIRED,
};

enum arg_kind {
        ARG_STRING,
        ARG_NUM,
};

struct arg_flag {
        /* flag  definition, needed to parse the flag */
        const char *flag;
        const char *desc;
        const char *flag_name;
        bool long_flag;
        enum arg_kind arg_t;
        enum arg_lvl arg_lvl;

        /* flag values, filled in by parser */
        bool found;
        union {
                const char *str;
                int64_t num;
        } arg;
};

/* returns false on failure, setting *err to NULL or a pointer to the option
 * struct that caused the problems */
bool arg_parse(int argc, const char **argv, struct arg_flag **flags,
               struct arg_flag **err);

#endif

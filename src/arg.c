#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "arg.h"

#define streq(str1, str2) (strcmp(str1, str2) == 0)

static bool parse_num(const char *arg, int64_t *num_out) {
        errno = 0;
        char *endptr;
        *num_out = strtoll(arg, &endptr, 10);
        if (*endptr != '\0' || errno == ERANGE) {
                return false;
        }
        return true;
}

static int fill_flag(struct arg_flag *flag, const char *arg) {

        if (flag->arg_lvl == ARG_REQUIRED && arg == NULL)
                return -1;
        flag->found = true;
        if (flag->arg_lvl == ARG_NONE) {
                flag->found = true;
                return 1;
        }
        switch (flag->arg_t) {
        case ARG_STRING:
                flag->arg.str = arg;
                break;
        case ARG_NUM:
                if (!parse_num(arg, &flag->arg.num)) {
                        return -1;
                }
                break;
        }
        return 2;
}

static int parse_short(const char *flag, const char *arg,
                       struct arg_flag **flags) {
        for (size_t i = 0; flags[i] != NULL; i++) {
                if (!flags[i]->long_flag && streq(flag, flags[i]->flag)) {
                        return fill_flag(flags[i], arg);
                }
        }
        return -1;
}

bool arg_parse(int argc, const char **argv, struct arg_flag **flags,
               struct arg_flag **err) {
        *err = NULL;
        for (int i = 1; i < argc;) {
                if (streq(argv[i], "-") || strlen(argv[i]) == 0) {
                        return false;
                } else if (argv[i][0] == '-') {
                        int ret = parse_short(
                            argv[i] + 1, argc - i != 1 ? argv[i + 1] : NULL,
                            flags);
                        if (ret == -1)
                                return false;

                        i += ret;
                } else
                        return false;
        }
        return true;
}

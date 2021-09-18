#ifndef CC_HELPERS_H
#define CC_HELPERS_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

struct src_file {
    const uint8_t *buf;
    size_t sz;
    const char *name;
};

bool src_file_open(const char *filename, struct src_file *f);
void src_file_close(struct src_file *f);

struct src_range {
    size_t s1;
    size_t s2;
    struct src_file *f;
};

void print_range(FILE *f, const struct src_range *rng);

#define streq(s1, s2) (strcmp(s1, s2) == 0)
#define strneq(s1, l1, s2, l2) (l1 == l2 ? strncmp(s1, s2, l1) == 0 : false)

#endif

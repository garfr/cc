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
struct src_range combine_ranges(struct src_range rng1, struct src_range rng2);

#define streq(s1, s2) (strcmp(s1, s2) == 0)
#define strneq(s1, l1, s2, l2) (l1 == l2 ? strncmp(s1, s2, l1) == 0 : false)

struct vec {
    uint8_t *buf;
    size_t cap;
    size_t len;
};

void vec_init(struct vec *vec);
void vec_free(struct vec *vec);
void *vec_push(struct vec *vec, void *val, size_t bytes);
void *vec_index(struct vec *vec, size_t idx, size_t bytes);

#define VEC_PUSH(vec, val, type) ((type *)(vec_push(vec, val, sizeof(type))))
#define VEC_INDEX(vec, idx, type) ((type *)(vec_index(vec, idx, sizeof(type))))
#define VEC_SIZE(vec, type) (vec.len / sizeof(type))

#endif

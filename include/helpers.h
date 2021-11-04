#ifndef CC_HELPERS_H
#define CC_HELPERS_H

#include "stdbool.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "string.h"

struct src_file {
        const uint8_t *buf;
        size_t sz;
        const char *name;
};

struct src_file *src_file_open(const char *filename, size_t len);
struct src_file *src_file_open_include(const char *filename, size_t len);
void src_file_close(struct src_file *f);

struct src_range {
        size_t s1;
        size_t s2;
        struct src_file *f;
};

void print_range(FILE *f, const struct src_range *rng);
struct src_range combine_ranges(struct src_range rng1, struct src_range rng2);

#define streq(s1, s2) (strcmp(s1, s2) == 0)
#define strneq(s1, l1, s2, l2)                                                 \
        (l1 == l2 ? strncmp((char *)s1, (char *)s2, l1) == 0 : false)

#define rangeeq(r1, r2)                                                        \
        strneq(r1.f->buf + r1.s1, r1.s2 - r1.s1, r2.f->buf + r2.s1,            \
               r2.s2 - r2.s1)

#define strrange(r1, str)                                                      \
        strneq(r1.f->buf + r1.s1, r1.s2 - r1.s1, str, strlen(str))

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
#define VEC_SIZE(vec, type) ((vec).len / sizeof(type))

struct type;
struct stmt;

struct var_ref {
        struct src_range name;
        struct type *type;
        struct stmt *label_loc;
        struct var_ref *next;
        struct vec tokens;
        size_t token_idx;
        int is_fun_macro;
};

struct symtab {
        struct symtab *up;
        struct var_ref **buckets;
        size_t sz;
};

void init_symtab(struct symtab *tab, struct symtab *up);
struct var_ref *add_var(struct symtab *tab, struct src_range name);
struct var_ref *find_var(struct symtab *tab, struct src_range name);

#endif

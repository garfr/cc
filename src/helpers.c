#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "helpers.h"

bool src_file_open(const char *filename, struct src_file *f) {
        FILE *in_file = fopen(filename, "rb");

        fseek(in_file, 0, SEEK_END);
        size_t fsize = ftell(in_file);
        fseek(in_file, 0, SEEK_SET);

        uint8_t *buf = calloc(fsize + 1, sizeof(uint8_t));
        if (buf == NULL)
                return false;

        fread(buf, sizeof(uint8_t), fsize, in_file);
        buf[fsize] = '\0';

        fclose(in_file);

        f->buf = buf;
        f->sz = fsize;
        f->name = filename;

        return true;
}

void src_file_close(struct src_file *f) { free((char *)f->buf); }

void print_range(FILE *f, const struct src_range *rng) {
        fprintf(f, "%.*s", (int)rng->s2 - rng->s1,
                (char *)rng->f->buf + rng->s1);
}

struct src_range combine_ranges(struct src_range rng1, struct src_range rng2) {
        if (rng1.f != rng2.f) {
                printf("these ranges aren't from the same file\n");
                exit(EXIT_FAILURE);
        }
        struct src_range ret = {
            .f = rng1.f,
            .s1 = rng1.s1,
            .s2 = rng2.s2,
        };
        return ret;
}

void vec_init(struct vec *vec) {
        vec->buf = NULL;
        vec->len = 0;
        vec->cap = 0;
}

void vec_free(struct vec *vec) {
        if (vec->buf) {
                free(vec->buf);
        }
}

static size_t roundup(size_t floor) {
        size_t ret = 1;
        while (ret < floor) {
                ret *= 2;
        }
        return ret;
}

void *vec_push(struct vec *vec, void *val, size_t bytes) {
        if (vec->len + bytes > vec->cap) {
                size_t new_sz = roundup(vec->cap + bytes);
                uint8_t *new_buf = realloc(vec->buf, new_sz);
                vec->cap = new_sz;
                vec->buf = new_buf;
        }
        memcpy(vec->buf + vec->len, val, bytes);
        void *ret = vec->buf + vec->len;
        vec->len += bytes;
        return ret;
}

void *vec_index(struct vec *vec, size_t idx, size_t bytes) {
        return vec->buf + (idx * bytes);
}

#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"

#include "helpers.h"

struct src_file *src_file_open(const char *_filename, size_t len) {
        struct src_file *f = calloc(1, sizeof(struct src_file));
        char *filename;
        if (len != 0) {
                filename = calloc(1, len + 1);
                memcpy(filename, _filename, len);
                filename[len] = '\0';
        } else
                filename = (char *)_filename;

        FILE *in_file = fopen(filename, "rb");
        if (in_file == NULL)
                return NULL;

        fseek(in_file, 0, SEEK_END);
        size_t fsize = ftell(in_file);
        fseek(in_file, 0, SEEK_SET);

        uint8_t *buf = calloc(fsize + 1, sizeof(uint8_t));
        if (buf == NULL)
                return NULL;

        fread(buf, sizeof(uint8_t), fsize, in_file);
        buf[fsize] = '\0';

        fclose(in_file);

        f->buf = buf;
        f->sz = fsize;
        f->name = filename;

        return f;
}

struct src_file *src_file_open_include(const char *_filename, size_t len) {
        struct src_file *f = calloc(1, sizeof(struct src_file));
        char *filename;
        if (len != 0) {
                filename = calloc(1, len + 1);
                memcpy(filename, _filename, len);
                filename[len] = '\0';
        } else
                filename = (char *)_filename;

        FILE *in_file = fopen(filename, "rb");
        const char prefix[] = "\\MinGW\\include\\";
        if (in_file == NULL) {
                filename = calloc(1, len + 1 + sizeof(prefix));
                memcpy(filename, prefix, sizeof(prefix) - 1);
                memcpy(filename + sizeof(prefix) - 1, _filename, len);
                filename[len + sizeof(prefix)] = '\0';
                in_file = fopen(filename, "rb");
                if (in_file == NULL) {
                        return NULL;
                }
        }

        fseek(in_file, 0, SEEK_END);
        size_t fsize = ftell(in_file);
        fseek(in_file, 0, SEEK_SET);

        uint8_t *buf = calloc(fsize + 1, sizeof(uint8_t));
        if (buf == NULL)
                return NULL;

        fread(buf, sizeof(uint8_t), fsize, in_file);
        buf[fsize] = '\0';

        fclose(in_file);

        f->buf = buf;
        f->sz = fsize;
        f->name = filename;

        return f;
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

void init_symtab(struct symtab *out, struct symtab *up) {
        out->up = up;
        out->buckets = calloc(8, sizeof(struct var_ref *));
        out->sz = 8;
}

static size_t hash_name(struct src_range name) {
        size_t total = 0;
        for (size_t i = name.s1; i < name.s2; i++) {
                total += name.f->buf[i];
                total *= 10;
        }
        return total;
}

struct var_ref *add_var(struct symtab *tab, struct src_range name) {
        size_t idx = hash_name(name) % tab->sz;

        struct var_ref *iter = tab->buckets[idx];
        while (iter != NULL) {
                if (rangeeq(iter->name, name)) {
                        printf("var already exists in scope.");
                        exit(EXIT_FAILURE);
                }
                iter = iter->next;
        }
        struct var_ref *new = calloc(1, sizeof(struct var_ref));
        new->next = tab->buckets[idx];
        tab->buckets[idx] = new;
        new->name = name;
        return new;
}

struct var_ref *find_var(struct symtab *tab, struct src_range name) {
        size_t idx = hash_name(name) % tab->sz;

        for (; tab != NULL; tab = tab->up) {
                struct var_ref *iter = tab->buckets[idx];
                while (iter != NULL) {
                        if (rangeeq(iter->name, name)) {
                                return iter;
                        }
                        iter = iter->next;
                }
        }
        return NULL;
}

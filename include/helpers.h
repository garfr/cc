#ifndef CC_HELPERS_H
#define CC_HELPERS_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

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

#endif

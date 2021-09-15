#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "helpers.h"

bool
src_file_open(const char *filename, struct src_file *f) {
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

void
src_file_close(struct src_file *f) {
    free((char*)f->buf);
}

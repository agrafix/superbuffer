#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct sbuf {
  char *contents;
  size_t currentSize;
  size_t maxSize;
};

struct sbuf *new_sbuf(size_t initSize)
{
  char *contents = (char *)malloc(initSize + 1);
  contents[0] = '\0';

  struct sbuf *buf = (struct sbuf *)malloc(sizeof(struct sbuf));
  buf->contents = contents;
  buf->currentSize = 0;
  buf->maxSize = initSize;

  return buf;
}

void append_sbuf(struct sbuf *buf, const char *value, size_t len)
{
  if (len == 0) {
    return;
  }

  while (buf->currentSize + len > buf->maxSize) {
    // reallocate more memory
    buf->maxSize *= 2;
    buf->contents = (char *)realloc(buf->contents, buf->maxSize);
  }
  char *targetLocation = buf->contents + buf->currentSize;
  strcpy(targetLocation, value);
  buf->currentSize += len;
}

char *read_sbuf(struct sbuf *buf, size_t *len)
{
  *len = buf->currentSize;
  return buf->contents;
}

void destroy_sbuf(struct sbuf *buf)
{
  free(buf->contents);
  free(buf);
}

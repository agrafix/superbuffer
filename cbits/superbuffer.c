#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct sbuf {
  char *contents;
  size_t currentSize;
  size_t maxSize;
};

struct sbuf *new_sbuf(const size_t initSize)
{
  char *contents = (char *)malloc(initSize + 1);
  contents[0] = '\0';

  struct sbuf *buf = (struct sbuf *)malloc(sizeof(struct sbuf));
  buf->contents = contents;
  buf->currentSize = 0;
  buf->maxSize = initSize;

  return buf;
}

void append_sbuf(struct sbuf *buf, const char *value, const size_t len)
{
  if (len == 0) {
    return;
  }
  const size_t nextSize = buf->currentSize + len;
  while (nextSize > buf->maxSize) {
    // reallocate more memory
    buf->maxSize *= 2;
    buf->contents = (char *)realloc(buf->contents, buf->maxSize);
  }
  char *targetLocation = buf->contents + buf->currentSize;
  memcpy(targetLocation, value, len);
  buf->currentSize = nextSize;
}

char *read_sbuf(struct sbuf *buf, size_t *len)
{
  *len = buf->currentSize;
  return buf->contents;
}

void destroyContents_sbuf(const struct sbuf *buf)
{
  free(buf->contents);
}

void destroy_sbuf(struct sbuf *buf)
{
  free(buf);
}

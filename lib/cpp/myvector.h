#ifndef MYVECTOR
#define MYVECTOR

#include "myutils.h"

struct _vector {
  int* data;
  size_t size;
};

typedef struct _vector vector_t;

vector_t *init(size_t size) {
  vector_t* n = (vector_t *)malloc(sizeof(vector_t));
  int *data = (int *)malloc(sizeof(int) * size);
  if (n == NULL || data == NULL)
    exit(1);
  n->size = size;
  n->data = data; 
  return n;
}

vector_t *inits(size_t size, ...) {
  vector_t *n = init(size);
  va_list list;
  va_start(list, size);
  int *data = n->data;
  for (int i = 0; i < size; i++) {
    *data++ = va_arg(list, int);
  }
  va_end(list);
  return n;
}

vector_t *from_vector(vector<int> v) {
  vector_t *n = init(v.size());
  for (int i = 0; i < v.size(); i++)
    n->data[i] = v[i];
  return n;
}

vector<int> to_vector(vector_t *v) {
  if (v == NULL)
    return vector<int>();
  vector<int> n(v->size);
  for (int i = 0; i < v->size; i++)
    n[i] = v->data[i];
  return n;
}

void display(vector_t *head) {
  int i;
  printf("<vector> size = %zu: ", head->size);
  for (i = 0; i < head->size - 1; i++)
    printf("%d,", head->data[i]);
  printf("%d\n", head->data[i]);
}

int indexAt(vector_t *head, int index) {
  // O(1)
  // Using a vector, you can access an element of it for a constant time.
  if (0 <= index && index < head->size)
    return head->data[index];
  printf("Out of index");
  exit(1);
}

void insertAt(vector_t *head, int index, int value) {
  // O(n + 1) or O(2n)
  // I think if realloc returns aother pinter, it means memcpy is executed so it needs O(n)
  int *data = (int *)realloc(head->data, sizeof(int) * (head->size + 1));
  if (data == NULL)
    exit(1);
  head->data = data;

  // You need to move an element one by one from index.
  int i = head->size;
  while (i > index) {
    head->data[i] = head->data[i - 1];
    i--;
  }
  head->data[index] = value;
  head->size++;
}

void deleteAt(vector_t *head, int index) {
  // O(n)
  if (index == 0)
    return;

  // You need to move each item from index to the next
  int i = index;
  while (i < head->size - 1) {
    head->data[i] = head->data[i + 1];
    i++;
  }

  // I think because space is decreasing, realloc needs O(1)
  head->data = (int *)realloc(head->data, sizeof(int) * (head->size - 1));
  // WARN: Don't free memories but head->data
  // free(head->data + (head->size - 1));
  head->size--;
}
#endif

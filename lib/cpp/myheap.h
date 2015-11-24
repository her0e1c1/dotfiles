// max heap
// root is the larget number

/*
 * M(1)
 *
 * 0 => 1, 2
 * PARENT(0) <= 0
 */
#define PARENT(n) (((n) - 1) / 2)
#define LEFT(n) (2 * (n) + 1)
#define RIGHT(n) (2 * (n) + 2)
#define VALUE(h, n) ((h)->array[(n)])
#define LAST(h) ((h)->array[(h)->size])

#define data_type int

struct _heap {
  int size;
  data_type *array;
};

typedef struct _heap heap_t;

void swap (heap_t *h, int i, int j) {
  data_type t = h->array[i];
  h->array[i] = h->array[j];
  h->array[j] = t;
}

// a < b => 1
int compare(data_type a, data_type b) {
  return a == b ? 0 : (a < b ? 1 : -1);
}

// return a child index which is more than 0
data_type maxChild(heap_t *h, int parent) {
  int left, right;
  if ((left = LEFT(parent)) >= h->size)
    return 0;  // parent doesn't have both of the children
  else if ((right = RIGHT(parent)) == h->size) {
    return left; // parent doesn't have the child of right
  } else if (compare(VALUE(h, left), VALUE(h, right)) == 1)
    return right;
  else
    return left;
}

// O(log2n)
// move the root to heapify
void downHeap(heap_t *h) {
  int parent = 0;
  while (1) {
    int child = maxChild(h, parent);
    if (child > 0 && compare(VALUE(h, parent), VALUE(h, child)) == 1) {
      swap(h, parent, child);
      parent = child;
    } else
      break;
  }
}

// O(log2n)
// insert at the last and heapify (move the new one at as top as possible)
void upHeap(heap_t *h, data_type data) {
  h->array[h->size] = data;
  int child = h->size;
  while (1) {
    int parent = PARENT(child);
    if (child > 0 && compare(VALUE(h, parent), VALUE(h, child)) == 1) {
      swap(h, parent, child);
      child = parent;
    } else
      break;
  }
  h->size++;
}

void heap_display(heap_t *h) {
  printf("<heap> size = %d: ", h->size);
  for (int i = 0; i < h->size; i++)
    printf("%d, ", h->array[i]);
  printf("\n");
}

heap_t *init(data_type *array, int size) {
  heap_t *h = (heap_t *)malloc(sizeof(heap_t));
  h->size = 0;
  h->array = array;
  for (int i = 0; i < size; i++) {
    upHeap(h, array[i]);
  }
  return h;
}

data_type *heapSort(data_type *array, int size) {
  heap_t *h = init(array, size);
  while (--h->size) {  // maxChild depending on h->size
    swap(h, 0, h->size);
    downHeap(h);
  }
  return array;
}

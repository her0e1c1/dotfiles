// max heap
// root is the larget number

/*
 * M(1) in place
 *
 * 0 => 1, 2
 * WARN: PARENT(0) == 0
 */
#define PARENT(n) (((n) - 1) / 2)
#define LEFT(n) (2 * (n) + 1)
#define RIGHT(n) (2 * (n) + 2)
#define VALUE(h, n) ((h)->array[(n)])

#define data_type int

static inline void swap(data_type *array, int i, int j) {
  data_type t = array[i];
  array[i] = array[j];
  array[j] = t;
}

// return a child index which is more than 0
data_type maxChild(data_type *a, int parent, int size) {
  int left, right;
  if ((left = LEFT(parent)) >= size)
    return 0;  // parent doesn't have both of the children
  else if ((right = RIGHT(parent)) == size) {
    return left; // parent doesn't have the child of right
  } else if (a[left] < a[right])
    return right;
  else
    return left;
}

// O(log2n)
// move the root to heapify
void downHeap(data_type *a, int last) {
  int parent = 0;
  while (1) {
    int child = maxChild(a, parent, last);
    if (child > 0 && a[parent] < a[child]) {
      swap(a, parent, child);
      parent = child;
    } else
      break;
  }
}

// O(log2n)
// insert at the last and heapify (move the new one at as top as possible)
void upHeap(data_type *a, int last) {
  int child = last;
  while (1) {
    int parent = PARENT(child);
    if (child > 0 && a[parent] < a[child]) {
      swap(a, parent, child);
      child = parent;
    } else
      break;
  }
}

// O(nlog2n)
data_type *heapSort(data_type *array, int size) {
  for (int i = 0; i < size; i++)
    upHeap(array, i);
  for (int i = size - 1; i >= 0; i--) {
    swap(array, 0, i);  // sort from last to first
    downHeap(array, i);
  }
  return array;
}

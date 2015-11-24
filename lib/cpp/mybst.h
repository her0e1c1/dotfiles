#ifndef MYBST
#define MYBST

#define data_type int

// left <= parent <= right
// all the children of left are not larger than parent
// all the children of right are not smaller than parent

struct _bst {
  data_type data;
  struct _bst *parent;
  struct _bst *left;
  struct _bst *right;
};

typedef struct _bst bst_t;

bst_t *bst_init(data_type data) {
  bst_t *tree = (bst_t *)malloc(sizeof(bst_t));
  if (tree == NULL) {
    printf("Out of memory\n");
    exit(1);
  }
  tree->data = data;
  tree->parent = NULL;
  tree->left = NULL;
  tree->right = NULL;
  return tree;
}

// O(log2n)
// you don't need the arparent of parent as previous data
// but it makes code more simple
// WARN: it takes O(n) to insert a sorted list
// if you insert range(0, 10), the height is 10
void bst_insert(bst_t **bst, data_type data, bst_t *parent) {
  bst_t *tree = *bst;
  if (tree == NULL) {
    tree = bst_init(data);
    tree->parent = parent;
    *bst = tree;
  } else if (tree->data >= data) {
    bst_insert(&(tree->left), data, tree);
  } else {
    bst_insert(&(tree->right), data, tree);
  }
}

// O(log2n)
bst_t *bst_search(bst_t *tree, data_type data) {
  if (tree == NULL)
    return NULL;
  else if (tree->data == data)
    return tree;
  else if (tree->data >= data)
    return bst_search(tree->left, data);
  else
    return bst_search(tree->right, data);
}

// if tree has 2 children, find the child of the min data at the side of right
// TODO: hot to delete root?
bst_t *bst_delete(bst_t *tree, data_type data) {
  if (tree == NULL) {
    return NULL;
  } else if (tree->data > data) {
    // return the same node which doesn't have data
    tree->left = bst_delete(tree->left, data);
  } else if (tree->data < data) {
    tree->right = bst_delete(tree->right, data);
  } else {
    // data is found
    bst_t *deleted = tree;
    if (tree->left == NULL) {
      tree = tree->right;  // it is ok whether right is NULL or not
    } else if (tree->right == NULL) {
      tree = tree->left;
    } else {
      // have 2 children
      // get the succesor tree, which is the min data
      bst_t *next = tree->right;
      while (next->left)
        next = next->left;
      tree->data = next->data;
      printf("del, min = %d, %d\n", data, next->data);
      
      /* bst_delete(next, next->data); */
      tree->right = bst_delete(tree->right, next->data);
      /* next->parent->left = next->right; */
      /* deleted = next; */
      return tree;
    }
    free(deleted);
  }
  return tree;
}

// TODO: display as tree structure (not liner list)
// because of left <= parent <= right, it is ALWAYS sorted if using in-order
void bst_display(bst_t *tree) {
  if (tree) {
    // in order
    if (tree->left)
      bst_display(tree->left);
    printf("%d, ", tree->data);
    if (tree->right)
      bst_display(tree->right);
  }
}


#endif


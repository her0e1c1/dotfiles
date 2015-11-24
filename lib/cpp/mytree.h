
#define data_type int

typedef struct _tree {
  data_type item;
  struct _tree *parent;
  struct _tree *left;
  struct _tree *right;
};

typedef struct _tree tree_t;


#include <stdio.h>
#include <stdlib.h>

enum _color {
  BLACK = 1,
  RED = 2,
};

#define rbtree_data_type int

struct _rbtree {
  enum _color color;
  rbtree_data_type data;
  struct _rbtree *parent;
  struct _rbtree *left;
  struct _rbtree *right;
};

#define APPEND_L(p, c) {                        \
    (p)->left  = (c);                           \
    if ((c) != NULL) (c)->parent = (p);         \
  }
#define APPEND_R(p, c) {                        \
    (p)->right = (c);                           \
    if ((c) != NULL) (c)->parent = (p);         \
  }
#define APPEND_P(root, tree, c) {                 \
    if ((tree)->parent == NULL) {                \
      (*root) = (c);                              \
      (c)->parent = NULL;                         \
    } else { _APPEND_P(tree, c); }               \
  }
#define _APPEND_P(c1, c2) {                     \
    if ((c1)->parent->left == (c1))             \
      { (c1)->parent->left = (c2); }            \
    else { (c1)->parent->right = (c2); }        \
    (c2)->parent = (c1)->parent;                \
    (c1)->parent = NULL;                        \
  }
#define LEFT_CHILD(c) ((c)->parent->left == (c))
#define RIGHT_CHILD(c) ((c)->parent->right == (c))

typedef struct _rbtree rbtree_t;
typedef enum _color color_e;

// y <= z <= x
// from -> x l-> y r-> z
// to   -> y r-> x l-> z
void rotateR(rbtree_t **root, rbtree_t *tree) {
  // rbtree_t *tree = *root;
  if (tree == NULL || tree->left == NULL) {
    printf("Can't rotate");
    exit(1);
  }
  rbtree_t *l = tree->left;
  rbtree_t *lr = l->right;
  APPEND_P(root, tree, l);
  APPEND_R(l, tree);
  APPEND_L(tree, lr);
}

// -> y r-> x l-> z
// -> x l-> y r-> z
void rotateL(rbtree_t **root, rbtree_t *tree) {
  // rbtree_t *tree = *root;
  if (tree == NULL || tree->right == NULL) {
    printf("Can't rotate");
    exit(1);
  }
  rbtree_t *r = tree->right;
  rbtree_t *rl = r->left;
  APPEND_P(root, tree, r);
  APPEND_L(r, tree);
  APPEND_R(tree, rl);
}

void rbtree_balance(rbtree_t **root, rbtree_t *tree) {
  rbtree_t *p, *u;  // parent, uncle
  while ((p = tree->parent) && p->color == RED && p->parent) {
    if (LEFT_CHILD(p))
      u = p->parent->right;
    else
      u = p->parent->left;
    if (u && u->color == RED) {
      // make a grand parent RED and loop from it recursively
      p->parent->color = RED;
      p->color = u->color = BLACK;
      tree = tree->parent->parent;
    } else {
      // make a parent BLACK and a grand parent RED and rotate
      // (on other way, make a current tree black and loop recursively!)
      // so that the parent is at the top. And finish loop
      // in other words, swap the colors between a parent and a grand parent
      if (LEFT_CHILD(p)) {
        if (RIGHT_CHILD(tree)) {
          // LR
          // after rotation, a tree's pointer is the same as LL
          tree = p;
          rotateL(root, tree);
        }
        // LL
        tree->parent->color = BLACK;
        tree->parent->parent->color = RED;
        rotateR(root, tree->parent->parent);
      } else {
        if (LEFT_CHILD(tree)) {
          // RL
          tree = p;
          rotateR(root, tree);
        }
        // RR
        tree->parent->color = BLACK;
        tree->parent->parent->color = RED;
        rotateL(root, tree->parent->parent);
      }
    }
  }
}

// reduce cases from 6 to 4
// you need to bottom up a tree to root althoug coloring only a current tree BLACK
/* void rbtree_balance(rbtree_t **root, rbtree_t *tree) { */
/*   rbtree_t *p, *u;  // parent, uncle */
/*   while ((p = tree->parent) && p->color == RED && p->parent) { */
/*     if (LEFT_CHILD(p)) { */
/*       if (RIGHT_CHILD(tree)) { */
/*         tree = p; */
/*         rotateL(root, tree); */
/*       } */
/*       tree->color = BLACK; */
/*       rotateR(root, tree->parent->parent); */
/*     } else { */
/*       if (LEFT_CHILD(tree)) { */
/*         tree = p; */
/*         rotateR(root, tree); */
/*       } */
/*       tree->color = BLACK; */
/*       rotateL(root, tree->parent->parent); */
/*     } */
/*     tree = tree->parent; */
/*   } */
/* } */

void rbtree_insert(rbtree_t **root, rbtree_data_type data) {
  rbtree_t *node = (rbtree_t *)malloc(sizeof(rbtree_t));
  if (node == NULL) {
    printf("Out of memory");
    exit(1);
  }

  node->color = RED;  // new data is always RED
  node->data = data;
  node->left = node->right = node->parent = NULL;
  if (*root == NULL) {
    *root = node;
  } else {
    rbtree_t *parent = NULL, *tree = *root;
    while (1) {
      if (tree->data >= data) {
        if (tree->left) {
          parent = tree->parent;
          tree = tree->left;
        } else {
          tree->left = node;
          node->parent = tree;
          break;
        }
      } else {
        if (tree->right) {
          parent = tree->parent;
          tree = tree->right;
        } else {
          tree->right = node;
          node->parent = tree;
          break;
        }
      }
    }
    rbtree_balance(root, node);
  }
  (*root)->color = BLACK;
}

void rbtree_display(rbtree_t *root) {
  if (root) {
    rbtree_display(root->left);
    printf("%d(%s)(%d), ", root->data, root->color == RED ? "R" : "B", root->parent ? root->parent->data : -1);
    rbtree_display(root->right);
  }
}

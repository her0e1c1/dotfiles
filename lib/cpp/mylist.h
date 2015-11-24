#ifndef MYLIST
#define MYLIST

// search/insert/update/delete/next/previous/
// min/max/append/

#include "myutils.h"

// unsorted list
struct _list {
  int data;
  struct _list *next;
};

typedef struct _list list_t;

list_t* init(int data){
  list_t* newNode = (list_t*) malloc(sizeof(list_t));
  if (newNode == NULL) {
    exit(1);
  }
  newNode->data = data;
  newNode->next = NULL;
  return newNode;
}

// O(n)
// add an item to the last of a list
void append(list_t** head, int data){
  list_t* n = init(data);
  if (*head == NULL) {
    *head = n;
  } else {
    list_t* last = *head;
    while(last->next)
      last = last->next;
    last->next = n;
  }
}

// O(1)
// add an item to the last of a list
void append1(list_t** start, list_t** end, int data){
  list_t* n = init(data);
  if (*start == NULL) {
    *start = n;
    *end = n;
  } else {
    (*end)->next = n;
    *end = n;
  }
}

list_t* inits(int argc, ...) {
  // with append1, O(n)
  // if you use append, the complexity is O(n^2)
  if (argc <= 0)
    return NULL;

  va_list list;
  va_start(list, argc);
  
  // WARN: you need NULL, or sometimes it's initilized by the other value
  // NULL needs including stdio.h header
  list_t* head = NULL;
  list_t* tail = NULL;
  for (int i = 0; i < argc; i++)
    append1(&head, &tail, va_arg(list, int));

  va_end(list);
  return head;
}

void display(list_t* head){
  printf("<list> ");
  if (head == NULL)
    return;
  list_t* n;
  for(n = head; n->next; n = n->next)
    printf("%d,", n->data);
  printf("%d\n", n->data);
}

// O(n)
list_t *search(list_t *head, int data) {
  while (head) {
    if (head->data == data)
      return head;
    head = head->next;
  }
  return NULL;
}

// O(n)
// get the nth item from the head of list
list_t* indexAt(list_t* head, int index) {
#define INDEX_ERROR {       \
  printf("Out of index\n"); \
  exit(1); }

  if (index < 0)
    INDEX_ERROR
  for (int i = 0; i < index; i++) {
    if (head)
      head = head->next;
    else
      INDEX_ERROR
  }
  return head;
#undef INDEX_ERROR
}

// O(1)
// Delete the node of the argument
void deleteThis(list_t* node) {
  if (node == NULL)
    return;
  else if (node->next == NULL) {
    // the last item;
    // you need previous node to set null the next node
    // free(prev);
    // prev->next = null;  // I can't do here!
    printf("Can't delete the last item");
    exit(1);
  } else {
    list_t *tmp = node->next;
    node->data = tmp->data;
    node->next = tmp->next;
    free(tmp);  // Don't forget!
  }
}

// O(1) (on the other hand, vector needs O(n))
// insert an item to the next of the node
// but you can't insert to the PREVIOUS one
void insertNext(list_t *node, int data) {
  list_t* n = init(data);
  n->next = node->next;
  node->next = n;
}

// M(n)
list_t *reverse(list_t* node) {
  list_t *head = NULL;
  while (node) {
    list_t *n = init(node->data);
    n->next = head;
    head = n;
    node = node->next;
  }
  return head;
}

// M(1)
list_t *reverseX(list_t *node) {
  list_t *head = NULL;
  while (node) {
    list_t *next = node->next;
    node->next = head;
    head = node;
    node = next;
  }
  return head;
}

// duplicates(X)
// delete(X)
// find loop

#endif

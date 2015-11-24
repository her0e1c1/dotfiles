#ifndef MYHASHTABLE
#define MYHASHTABLE

#include "myutils.h"
#define data_type char*

// also see linear probing
// data size is limited. (if collision occurs, then next pointer is checked (rehash))

/*
 * if there are more than one entries having the same hash value (I mean collision),
 * they become linked list like below. (x is entry, _ is NULL)
 * _ x _ x _ 
 *   _   x
 *       _
 */
struct _entry {
  char *key;
  data_type data;
  struct _entry *next;
};

struct _hash_table {
  int size;
  struct _entry **table;
};

typedef struct _entry entry_t;
typedef struct _hash_table hash_table_t;

// you need rehash for a more number of entries than size
// to reduce load factor n/size
hash_table_t *init(int size) {
  hash_table_t *ht = (hash_table_t *)malloc(sizeof(hash_table_t));
  ht->table = (entry_t **)malloc(sizeof(entry_t *) * size);
  if (ht == NULL || ht->table == NULL) {
    printf("Out of memory");
    exit(1);
  }
  for (int i = 0; i < size; i++)
    ht->table[i] = NULL;
  ht->size = size;
  return ht;
}

void display(hash_table_t *ht) {
  printf("<hash table>\n");
  for (int i = 0; i < ht->size; i++) {
    if (ht->table[i] == NULL)
      continue;
    int first = 1;
    for (entry_t *e = ht->table[i]; e; e = e->next) {
      if (first)
        printf("%s => ", e->key);
      printf("%s, ", e->data);  // change format depending on data_type
      first = 0;
    }
    printf("\n");
  }
}

// hash must unsigned because it is used by an index of an array
int get_hash(hash_table_t *table, char *key) {
  unsigned int v = 0;
  char *p = key;
  while (*p) {
    v += *p++;
  }
  return v % table->size;
}

entry_t *make_entry(char *key, data_type data) {
  entry_t *n = (entry_t *)malloc(sizeof(entry_t));
  char *k = strdup(key);
  if (n == NULL || k == NULL) {
    printf("Out of memory");
    exit(1);
  }
  n->key = k;
  n->data = data;
  n->next = NULL;
  return n;
}

// O(n/size * key) ()
entry_t *ht_search(hash_table_t *ht, char *key) {
  int hashvalue = get_hash(ht, key);
  for (entry_t *e = ht->table[hashvalue]; e; e=e->next) {
    // it is better to take an action here not on the callee side
    // a key can't be hashed because collision should not occur here
    // so if you use a long key, it takes a long time to compare
    if (strcmp(e->key, key) == 0)
      return e;
  }
  return NULL;
}

// O(n/size + key)
void ht_insert(hash_table_t *ht, char *key, data_type data) {
  entry_t *e = ht_search(ht, key);
  if (e == NULL) {
    // create
    int hashvalue = get_hash(ht, key);
    entry_t *n = make_entry(key, data);
    // insert at the head
    n->next = ht->table[hashvalue];
    ht->table[hashvalue] = n;
  } else {
    // update
    e->data = data;
  }
}

// O(n/size + key)
void ht_delete(hash_table_t *ht, char *key) {
  entry_t *e = ht_search(ht, key);
  if (e) {
    int hashvalue = get_hash(ht, key);
    // should move this code into ht_search
    for (entry_t *e = ht->table[hashvalue], *p = NULL; e; p=e, e=e->next) {
      if (strcmp(e->key, key) == 0) {
        if (p == NULL)
          ht->table[hashvalue] = e->next;
        else
          p->next = e->next;
        free(e);
      }
    }
  } else {
    printf("Not found key");
    exit(1);
  }
}
// insert key value
// search

#endif

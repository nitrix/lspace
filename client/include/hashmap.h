#ifndef HASHMAP_H
#define HASHMAP_H

#include "stdio.h"
#include "stdlib.h"
#include "size.h"

typedef struct hashmap_list HASHMAP_BUCKET_LIST;
struct hashmap_list {
    char                 *key;
    void                 *value;
    HASHMAP_BUCKET_LIST  *next;
};

typedef struct hashmap {
    TYPE_SIZE_HASHMAP     size;
    HASHMAP_BUCKET_LIST **map;
} HASHMAP;

HASHMAP       *hm_create(TYPE_SIZE_HASHMAP nb_bucket);
void           hm_destroy(HASHMAP *hm);
unsigned long  hm_hash(char *str);
void           hm_insert(HASHMAP *hm, char *key, void *value);
void           hm_remove(char *str);
void          *hm_search(HASHMAP *hm, char *key);

#endif /* HASHMAP_H */

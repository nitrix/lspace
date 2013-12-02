#include "hashmap.h"

HASHMAP *hm_create(TYPE_SIZE_HASHMAP nb_bucket)
{
    HASHMAP *hm = malloc(sizeof (HASHMAP));
    
    hm->size = nb_bucket ? nb_bucket : 1;
    hm->map  = calloc(nb_bucket, sizeof (HASHMAP_BUCKET_LIST*));

    return hm;
}

void hm_destroy(HASHMAP *hm)
{    
    int i;
    HASHMAP_BUCKET_LIST *node, *tmp;

    /* We delete every nodes in every list */
    for (i=0; i < hm->size; i++) {
        node = hm->map[i];
        while(node) {
            tmp  = node;
            node = node->next;
            free(tmp);
        }
    }

    /* Then delete the buckets */
    free(hm->map);
    
    /* Then delete our hashmap object */
    free(hm);
}

unsigned long hm_hash(char *str)
{
    /* djb2 (k=33) algorythm originally by Dan Bernstein */
    /* Alternative: hash(i) = hash(i - 1) * 33 ^ str[i]; */
    unsigned long hash = 5381;

    while (*str) {
        hash = ((hash << 5) + hash) + *str++;
    }

    return hash;
}

void hm_insert(HASHMAP *hm, char *key, void *value)
{
    TYPE_SIZE_HASHMAP    bucket;
    HASHMAP_BUCKET_LIST *new_node;
        
    /* Prepare our new node to append */
    new_node        = malloc(sizeof (HASHMAP_BUCKET_LIST));
    new_node->key   = key; 
    new_node->value = value;
    new_node->next  = NULL;

    /* Calculate the destination bucket given our key */
    bucket = hm_hash(key) % hm->size;

    /* Place our first node or append our node to an existing one in the list */
    if (hm->map[bucket] == NULL) {
        printf("first\n");
        hm->map[bucket] = new_node;
    } else {
        printf("not first\n");
        new_node->next  = hm->map[bucket];
        hm->map[bucket] = new_node;
    }
}

void hm_remove(char *str)
{
    printf("Rem: %s", str);
}

void *hm_search(HASHMAP *hm, char *key)
{
    TYPE_SIZE_HASHMAP    bucket = hm_hash(key) % hm->size;
    HASHMAP_BUCKET_LIST *node   = hm->map[bucket];

    while(node) {
        if (strcmp(node->key, key) == 0) {
            return node->value;
        }
        node = node->next;
    }

    return NULL;
}

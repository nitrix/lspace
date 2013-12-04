#ifndef LIST_H
#define LIST_H

#include "stdlib.h"

typedef struct list LIST;
struct list {
    void *data;
    LIST *next;
};

void  list_push(LIST *list, void *obj);
void *list_pop(LIST *list);
void  list_remove(LIST *list, void *obj);

#endif /* LIST_H */

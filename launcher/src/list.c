#include "list.h"

// A -> C
// A -> [B] -> C

void list_push(LIST *list, void *obj)
{
    if (obj) {
        LIST *node = malloc(sizeof (LIST));
        node->data = list->data; 
        node->next = list->next;
        list->data = obj;
        list->next = node;
    }
}

void *list_pop(LIST *list)
{
    void *p = list->data;
    if (list->next == NULL) return NULL;
    list_remove(list, list->data);
    return p;
}

void list_remove(LIST *list, void *obj)
{
    LIST *p = list;
    LIST *l = NULL;
    while (p != NULL) {
        if (p->data == obj) {
            if (l) {
                l->next = p->next;
                free(p);
                return;
            } else {
                l = p->next;
                p->data = p->next->data;
                p->next = p->next->next;
                free(l);
            }
        }
        
        l = p;
        p = p->next;
    }
}

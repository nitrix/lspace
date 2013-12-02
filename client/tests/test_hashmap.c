#include "hashmap.h"
#include "stdio.h"

int main(void)
{
    int test_num_a  = 42;
    int test_num_b  = 43;
    int test_num_c  = 44;
    int test_num_d  = 45;
    int *p;

    HASHMAP *hm = hm_create(2);
    
    hm_insert(hm, "f", &test_num_b);
    hm_insert(hm, "g", &test_num_c);
    hm_insert(hm, "h", &test_num_b);
    hm_insert(hm, "i", &test_num_c);
    hm_insert(hm, "j", &test_num_b);
    hm_insert(hm, "a", &test_num_a);

    /* Test #1 - Find a key inserted */
    p = hm_search(hm, "a");
    if (!p) {
        fprintf(stderr, "\nUnable to find key \"a\" after insertion\n");
        return EXIT_FAILURE;
    }

    /* Test #2 - Make sure the value is the good one */
    if (*p != 42) {
        fprintf(stderr, "Did not return the proper value for key \"a\"\n");
        return EXIT_FAILURE;
    }
    
    /* Test #3 - Searching for innexistant key */
    p = hm_search(hm, "z");
    if (p) {
        fprintf(stderr, "Searching \"z\" should not produce a result\n");
        return EXIT_FAILURE;
    }
    
    /* Test #4 - Checking for similar names */
    p = hm_search(hm, "aa");
    if (p) {
        fprintf(stderr, "There should be no results for key \"aa\"\n");
        return EXIT_FAILURE;
    }
    p = hm_search(hm, "b");
    if (p) {
        fprintf(stderr, "There should be no results for key \"b\"\n");
        return EXIT_FAILURE;
    }

    /* Test #5 - Removing a key */
    hm_remove(hm, "a");
    p = hm_search(hm, "a");
    if (p) {
        fprintf(stderr, "Still able to find key \"a\" after deletion\n");
        return EXIT_FAILURE;
    }
    p = hm_search(hm, "g");
    if (!p) {
        fprintf(stderr, "Key affected by the removal of \"a\" and should not\n");
        return EXIT_FAILURE;
    }

    /* Test #6 - Add an exising key */
    hm_insert(hm, "f", &test_num_d);
    p = hm_search(hm, "f");
    if (*p != 45) {
        fprintf(stderr, "Did not manage to insert a most recent value a create duplicated keys like a LIFO stack would\n");
        return EXIT_FAILURE;
    }

    /* Test #7 - Remove a duplicated key */
    hm_remove(hm, "f");
    p = hm_search(hm, "f");
    if (!p) {
        fprintf(stderr, "Removing a duplicated key removed more than a single element\n");
        return EXIT_FAILURE;
    }
    if (*p == 45) {
        fprintf(stderr, "Did not manage to remove the most recent value for duplicated keys like a LIFO stack would\n");
        return EXIT_FAILURE;
    }

    hm_destroy(hm);

    return EXIT_SUCCESS;
}

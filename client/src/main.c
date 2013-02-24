#include "main.h"

int main(void)
{
    struct renderer *rnd = malloc(sizeof(struct renderer));
    renderer_ctor(rnd);
    
    while (true) {
        renderer_update(rnd);
        renderer_render(rnd);
    }

    renderer_dtor();

    return EXIT_SUCCESS;
}

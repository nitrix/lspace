#include "stdlib.h"
#include "renderer.h"


int main(void)
{
    struct renderer *rnd = renderer_init();
    
    while (true) {
        renderer_update(rnd);
        renderer_render(rnd);
    }

    renderer_fini();

    return EXIT_SUCCESS;
}

#include "main.h"

CAMERA *gCamera;

int main(int argc, char *argv[])
{
    world_init();
    renderer_init();
    gCamera = camera_create();

    coord_apply(camera_coord_get(gCamera), 0, 10 - 16, 10 - 16, 0);
    
    while (1) {
        SDL_Event event;
        if (SDL_WaitEventTimeout(&event, 16)) { /* 16ms = 60fps */
            /* handle your event here */
            switch(event.type) {
                case SDL_KEYDOWN:
                    printf("F");
                    switch (event.key.keysym.sym) {
                        case SDLK_ESCAPE:
                            terminate();
                            break;
                        case SDLK_d:
                            coord_apply(camera_coord_get(gCamera), 0, 1, 0, 0); /*right*/
                            break;
                        case SDLK_a:
                            coord_apply(camera_coord_get(gCamera), 0, 0, 0, 1); /*left*/
                            break;
                        case SDLK_w:
                            coord_apply(camera_coord_get(gCamera), 1, 0, 0, 0); /*top*/
                            break;
                        case SDLK_s:
                            coord_apply(camera_coord_get(gCamera), 0, 0, 1, 0); /*bottom*/
                            break;
                    }
                    break;
            }
        }
        /* do some other stuff here */
        renderer_clear();
        renderer_render(gCamera);
    }

    terminate(); /* Good ending */
    return EXIT_FAILURE; /* Bad ending */
} 

void terminate()
{
    camera_destroy(gCamera);
    renderer_fini();
    world_fini();
    exit(EXIT_SUCCESS);
}

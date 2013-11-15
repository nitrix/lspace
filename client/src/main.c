#include "main.h"

CAMERA *gCamera;

int main(int argc, char *argv[])
{
    world_init();
    renderer_init();
    gCamera = camera_create();

    coord_apply(camera_coord_get(gCamera), 0, 10 - 16, 10 - 16, 0);
    main_event_render();
    
    while (1) {
        SDL_Event event;
        if (SDL_WaitEventTimeout(&event, 50)) {
            switch(event.type) {
                case SDL_USEREVENT:
                    if (event.user.code == 0) {
                        renderer_clear();
                        renderer_render(gCamera);
                    }
                    break;
                case SDL_KEYDOWN:
                    switch (event.key.keysym.sym) {
                        case SDLK_ESCAPE:
                            main_terminate();
                            break;
                        case SDLK_d:
                            coord_apply(camera_coord_get(gCamera), 0, 1, 0, 0); /*right*/
                            main_event_render();
                            break;
                        case SDLK_a:
                            coord_apply(camera_coord_get(gCamera), 0, 0, 0, 1); /*left*/
                            main_event_render();
                            break;
                        case SDLK_w:
                            coord_apply(camera_coord_get(gCamera), 1, 0, 0, 0); /*top*/
                            main_event_render();
                            break;
                        case SDLK_s:
                            coord_apply(camera_coord_get(gCamera), 0, 0, 1, 0); /*bottom*/
                            main_event_render();
                            break;
                    }
                    break;
            }
        }
    }

    main_terminate(); /* Good ending */
    return EXIT_FAILURE; /* Bad ending */
} 

void main_event_render()
{
    /*
     * TODO: have a check to block extra calls to this function;
     * too many requests under 100ms should simply be dropped, as the user won't notice any difference
     * but it's eating up precious resources.
     *
     * Plus, we do not want GFX animations to run faster than they should (please consider in the design).
     */
    
    SDL_Event event;
    SDL_UserEvent userevent;
    
    userevent.type = SDL_USEREVENT;
    userevent.code = 0;
    userevent.data1 = NULL;
    userevent.data2 = NULL;

    event.type = SDL_USEREVENT;
    event.user = userevent;

    SDL_PushEvent(&event);
}

void main_terminate()
{
    camera_destroy(gCamera);
    renderer_fini();
    world_fini();

    exit(EXIT_SUCCESS);
}

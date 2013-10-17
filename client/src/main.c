#include "main.h"

int main(int argc, char *argv[])
{
    Camera camera;
    camera.x = 0;
    camera.y = 0;

    renderer_init();
    
    while (1) {
        SDL_Event event;
        if (SDL_WaitEventTimeout(&event, 100)) {
            // handle your event here
            switch(event.type) {
                case SDL_KEYDOWN:
                    switch (event.key.keysym.sym) {
                        case SDLK_ESCAPE:
                            terminate();
                            break;
                        case SDLK_w:
                            camera.x--;
                            break;
                        case SDLK_a:
                            camera.y--;
                            break;
                        case SDLK_s:
                            camera.x++;
                            break;
                        case SDLK_d:
                            camera.y++;
                            break;
                    }
                    break;
            }
            renderer_render(&camera);
        } else {
            // do some other stuff here -- draw your app, etc.
            renderer_render(&camera);
        }
    }

    //terminate(); //Good ending
    return EXIT_FAILURE; //Bad ending
} 

void terminate()
{
    renderer_fini();
    exit(EXIT_SUCCESS);
}

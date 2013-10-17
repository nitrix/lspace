#include "main.h"

int main(int argc, char *argv[])
{
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
                    }
                    break;
            }
        } else {
            // do some other stuff here
            renderer_clear();
            renderer_render();
        }
    }

    terminate(); //Good ending
    return EXIT_FAILURE; //Bad ending
} 

void terminate()
{
    renderer_fini();
    exit(EXIT_SUCCESS);
}

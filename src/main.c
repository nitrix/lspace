#include "toolkit.h"
#include <SDL.h>

int main(int argc, char *argv[]) {
    TK_UNUSED(argc);
    TK_UNUSED(argv);

    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        SDL_Log("Unable to initialize SDL: %s", SDL_GetError());
        return EXIT_FAILURE;
    }

    SDL_Window *window;
    SDL_Renderer *renderer;

    if (SDL_CreateWindowAndRenderer(800, 600, 0, &window, &renderer) != 0) {
        SDL_Log("Unable to create window and renderer: %s", SDL_GetError());
        SDL_Quit();
        return EXIT_FAILURE;
    }

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();

    return EXIT_SUCCESS;
}

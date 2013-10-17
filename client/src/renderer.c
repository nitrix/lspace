#include "renderer.h"

SDL_Window   *gWindow;
SDL_Renderer *gRenderer;

void renderer_init()
{
    // Load SDL
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO | SDL_INIT_TIMER | SDL_INIT_EVENTS) != 0) {
        fprintf(stderr, "Unable to initialize SDL: %s\n", SDL_GetError());
        exit(EXIT_FAILURE);
    }
    
    // Force opengl version 3.0
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION,3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION,0);
    
    // Create a window and renderer for the game
    SDL_CreateWindowAndRenderer(0, 0, SDL_WINDOW_FULLSCREEN_DESKTOP, &gWindow, &gRenderer);

    if (!gWindow || !gRenderer) {
        fprintf(stderr, "Unable to create a window or renderer: %s\n", SDL_GetError());
        exit(EXIT_FAILURE);
    }

    // Get the OpenGL context
    SDL_GLContext context = SDL_GL_CreateContext(gWindow);
    const char *version = glGetString(GL_VERSION);
    if (!version) {
        fprintf(stderr, "Unable to get OpenGL version\n");
        exit(EXIT_FAILURE);
    } else {
        printf("OpenGL version: %s\n", version);
    }
    
    /* We could enforce a logical size and have the system emulate it.
     *
     * SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "linear"); // make the scaled rendering look smoother.
     * SDL_RenderSetLogicalSize(renderer, 640, 480);
     * 
     * Note that 640x480 and 1920x1200 aren't the same aspect ratio: SDL takes care of that, too,
     * scaling as much as possible and letterboxing the difference.
     */
}


void renderer_clear()
{
    // Clear the screen
    SDL_SetRenderDrawColor(gRenderer, 0, 0, 0, 255);
    SDL_RenderClear(gRenderer);
}

void renderer_render()
{
    //TODO: draw elements here
    /*
    SDL_Rect src, dest;
    src.x  = 0;
    src.y  = 0;
    src.w  = 38;
    src.h  = 38;
    
    dest.x = 0;
    dest.y = 0;
    dest.w = 380;
    dest.h = 380;
    SDL_RenderCopy(gRenderer, gTileset, &src, &dest);
    */

    SDL_RenderPresent(gRenderer);
}

void renderer_fini()
{
    SDL_DestroyWindow(gWindow);
    SDL_Quit();
}

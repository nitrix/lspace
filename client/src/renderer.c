#include "renderer.h"

SDL_Window   *gWindow;
SDL_Renderer *gRenderer;
// SDL_Texture  *gTileset;

void renderer_init()
{
    // Load SDL
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO | SDL_INIT_TIMER | SDL_INIT_EVENTS) != 0) {
        fprintf(stderr, "Unable to initialize SDL: %s\n", SDL_GetError());
        exit(EXIT_FAILURE);
    }
    
    // Force opengl version
    //SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION,4);
    //SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION,2);
    
    // Create a window and renderer for the game
    SDL_CreateWindowAndRenderer(0, 0, SDL_WINDOW_FULLSCREEN_DESKTOP, &gWindow, &gRenderer);

    if (!gWindow || !gRenderer) {
        fprintf(stderr, "Unable to create a window or renderer: %s\n", SDL_GetError());
        exit(EXIT_FAILURE);
    }

    /* We could enforce a logical size and have the system emulate it.
     *
     * SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "linear"); // make the scaled rendering look smoother.
     * SDL_RenderSetLogicalSize(renderer, 640, 480);
     * 
     * Note that 640x480 and 1920x1200 aren't the same aspect ratio: SDL takes care of that, too,
     * scaling as much as possible and letterboxing the difference.
     */
    
    // Load SDL_image
    IMG_Init(IMG_INIT_JPG | IMG_INIT_PNG);

    // Load tileset
    // renderer_load_tileset();
}

/*
void renderer_load_tileset()
{
    // Load a test image
    SDL_Surface *tileset = IMG_Load("tileset.png");

    if(!tileset) {
        fprintf(stderr, "Unable to load image `tileset.png`: %s\n", IMG_GetError());
        exit(EXIT_FAILURE);
    }
    
    SDL_Surface *placeholder = SDL_CreateRGBSurfaceFrom(tileset->pixels, 38, 38, 32, tileset->pitch, 0,0,0,0);

    //gTileset = SDL_CreateTextureFromSurface(gRenderer, tileset);
    gTileset = SDL_CreateTextureFromSurface(gRenderer, placeholder);
    
    SDL_RenderCopyEx(gRenderer, gTileset, NULL, NULL, 45, NULL, SDL_FLIP_VERTICAL);

    SDL_FreeSurface(tileset);
    SDL_FreeSurface(placeholder);
}
*/

void renderer_clear()
{
    // Clear the screen
    SDL_SetRenderDrawColor(gRenderer, 0, 0, 0, 255);
    SDL_RenderClear(gRenderer);
}

void renderer_render()
{
    renderer_clear();

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

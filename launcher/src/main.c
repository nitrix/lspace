#include "main.h"

int main(void)
{
    /*
    LIST test;
    list_push(&test, (void*)11);
    list_push(&test, (void*)12);
    list_push(&test, (void*)13);
    list_push(&test, (void*)14);
    list_push(&test, (void*)15);
    list_push(&test, (void*)16);
    list_remove(&test, (void*)16);
    list_remove(&test, (void*)15);
    list_remove(&test, (void*)13);
    // 11,12,14

    list_remove(&test, (void*)11);

    LIST *p;
    while (p = list_pop(&test)) {
        printf("%u\n", p);
    } 
    exit(0);
    */

    SDL_Window      *window;
    SDL_Renderer    *renderer;
    SDL_DisplayMode  displayMode;
    int              check, i;
    LIST            *layer[3];

    /* Load SDL */
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO | SDL_INIT_TIMER | SDL_INIT_EVENTS) != 0) {
        fprintf(stderr, "Unable to initialize SDL: %s\n", SDL_GetError());
        exit(EXIT_FAILURE);
    }
    
    /* Create a window and renderer for the launcher */
    SDL_CreateWindowAndRenderer(0, 0, SDL_WINDOW_FULLSCREEN_DESKTOP, &window, &renderer);
    if (!window || !renderer) {
        fprintf(stderr, "Unable to create a window or renderer: %s\n", SDL_GetError());
        exit(EXIT_FAILURE);
    }

    /* Save the display mode for later uses */
    check = SDL_GetCurrentDisplayMode(0, &displayMode);
    if (check != 0) {
        fprintf(stderr, "Unable to get current display mode: %s\n", SDL_GetError());
    }

    /* Set alpha blending mode */
    SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);

    /* Clear the entire screen */
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);

    srand(time(NULL));

    /* Generate stars */
    for (i = 0; i < 2000; i++) { //FIXME: proper ratio given the screen size please
        STAR *star  = malloc(sizeof (STAR));
        star->x     = rand() % displayMode.w;
        star->y     = rand() % displayMode.h;
        star->alpha = rand() % 255;
        list_add(layer[rand() % 3], star);
    }
    
    //SDL_SetRenderDrawColor(renderer, 255, 255, 255, rand() % 255);
    //SDL_RenderDrawPoint(renderer, rand() % displayMode.w, rand() % displayMode.h);

    // Up until now everything was drawn behind the scenes.
    // This will show the new, red contents of the window.
    SDL_RenderPresent(renderer);

    SDL_Delay(5000);

    /*
     * Load SDL_image
     * IMG_Init(IMG_INIT_PNG);
     *
     * Load the tileset using a temporary surface
     * tmp = IMG_Load("tileset.png");
     * if(!tmp) {
     *     fprintf(stderr, "Unable to load the tileset (tileset.png): %s\n", IMG_GetError());
     *     exit(EXIT_FAILURE);
     * }
     * gTileset = SDL_CreateTextureFromSurface(gRenderer, tmp);
     * SDL_FreeSurface(tmp);
     */

    /* Cleanup SDL */
    SDL_DestroyWindow(window);
    SDL_Quit();

    /*
     * Cleanup SDL_image 
     * IMG_Quit();
     */

    return 0;
}

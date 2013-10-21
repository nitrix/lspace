#include "renderer.h"

SDL_Window      *gWindow;
SDL_Renderer    *gRenderer;
SDL_Texture     *gTileset;
SDL_DisplayMode  gDisplayMode;

void renderer_init()
{
    int check;
    SDL_Surface *tmp = NULL;

    /* Load SDL */
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO | SDL_INIT_TIMER | SDL_INIT_EVENTS) != 0) {
        fprintf(stderr, "Unable to initialize SDL: %s\n", SDL_GetError());
        exit(EXIT_FAILURE);
    }
    
    /* Create a window and renderer for the game */
    SDL_CreateWindowAndRenderer(0, 0, SDL_WINDOW_FULLSCREEN_DESKTOP, &gWindow, &gRenderer);

    if (!gWindow || !gRenderer) {
        fprintf(stderr, "Unable to create a window or renderer: %s\n", SDL_GetError());
        exit(EXIT_FAILURE);
    }

    /* Save the display mode for later uses */
    check = SDL_GetCurrentDisplayMode(0, &gDisplayMode);
    if (check != 0) {
        fprintf(stderr, "Unable to get current display mode: %s\n", SDL_GetError());
    }

    /* Load SDL_image */
    IMG_Init(IMG_INIT_PNG);

    /* Load the tileset using a temporary surface */
    tmp = IMG_Load("tileset.png");
    if(!tmp) {
        fprintf(stderr, "Unable to load the tileset (tileset.png): %s\n", IMG_GetError());
        exit(EXIT_FAILURE);
    }

    gTileset = SDL_CreateTextureFromSurface(gRenderer, tmp);
    SDL_FreeSurface(tmp);
}

void renderer_fini()
{
    /* Cleanup SDL */
    SDL_DestroyWindow(gWindow);
    SDL_Quit();

    /* Cleanup SDL_image */
    IMG_Quit();
}

void renderer_render(CAMERA *camera)
{
    COORD *camera_coord, coord;
    CELL  *cell;
    SDL_Rect src, dest;
    unsigned int i,j;
    unsigned int screen_width_gfx, screen_height_gfx;
    unsigned int screen_width_chunk, screen_height_chunk;

    src.w  = GFX_WIDTH_PX;
    src.h  = GFX_HEIGHT_PX;
    dest.w = GFX_WIDTH_PX;
    dest.h = GFX_HEIGHT_PX;

    screen_width_gfx    = gDisplayMode.w / GFX_WIDTH_PX  + 1;
    screen_height_gfx   = gDisplayMode.h / GFX_HEIGHT_PX + 1;
    screen_width_chunk  = screen_width_gfx / SIZE_CHUNK  + 1; 
    screen_height_chunk = screen_height_gfx / SIZE_CHUNK + 1;

    /* copy the camery coords locally to work with it */
    camera_coord = camera_get_coord(camera);
    coord = *camera_coord;
    
    printf("Gfx: %d, %d\n", screen_width_gfx, screen_height_gfx);
    printf("Chunk: %d, %d\n", screen_width_chunk, screen_height_chunk);
    printf("World coord: %d, %d\n", camera_coord->world_position_chunk_x, camera_coord->world_position_chunk_y);
    printf("Chunk coord: %d, %d\n", camera_coord->chunk_position_cell_x, camera_coord->chunk_position_cell_y);
    printf("------\n");

        
    /* the rendering - only one chunk */
    for (i=camera_coord->chunk_position_cell_x; i<SIZE_CHUNK; i++) {
        dest.x = ((i - camera_coord->chunk_position_cell_x) * GFX_WIDTH_PX);
        for (j=camera_coord->chunk_position_cell_y; j<SIZE_CHUNK; j++) {
            dest.y = ((j - camera_coord->chunk_position_cell_y) * GFX_HEIGHT_PX);
           
            coord_apply(&coord, 0, i, j, 0);
            cell  = world_get_cell(&coord);
            src.x = GFX_WIDTH_PX  * cell->gfx.tileset_x; 
            src.y = GFX_HEIGHT_PX * cell->gfx.tileset_y;
            
            SDL_RenderCopy(gRenderer, gTileset, &src, &dest);
        }
    }

    SDL_RenderPresent(gRenderer);
}

void renderer_clear()
{
    /* Clear the SDL screen */
    SDL_SetRenderDrawColor(gRenderer, 0, 0, 0, 255);
    SDL_RenderClear(gRenderer);
}

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
    TYPE_SIZE_CHUNK i,j; /* nested loops x,y cells */
    TYPE_SIZE_WORLD k,l; /* nested loops x,y chunks */
    unsigned int screen_width_gfx, screen_height_gfx;
    unsigned int screen_width_chunk, screen_height_chunk;

    /* set source and destination widht/height to our gfx dimensions  */
    src.w  = GFX_WIDTH_PX;
    src.h  = GFX_HEIGHT_PX;
    dest.w = GFX_WIDTH_PX;
    dest.h = GFX_HEIGHT_PX;

    /* calculate how many chunks can fit our screen */
    screen_width_gfx    = gDisplayMode.w / GFX_WIDTH_PX  + 1; /* +1 to accomodate rounding error */
    screen_height_gfx   = gDisplayMode.h / GFX_HEIGHT_PX + 1; /* +1 to accomodate rounding error */
    screen_width_chunk  = screen_width_gfx / SIZE_CHUNK  + 2; /* +2 to accomodate rounding error & safety extra chunk */
    screen_height_chunk = screen_height_gfx / SIZE_CHUNK + 2; /* +2 to accomodate rounding error & safety extra chunk */

    /* local pointer to make it simpler */
    camera_coord = camera_coord_get(camera);
    
    /* the rendering of all visible chunks on screen */
    for (k=0; k < screen_width_chunk; k++) {
        for (l=0; l < screen_height_chunk; l++) {

            /* the rendering of all cells inside each chunk */
            for (i=0; i<SIZE_CHUNK; i++) {
                for (j=0; j<SIZE_CHUNK; j++) {

                    /* compute the destination of this cell (read position) on the screen */
                    dest.x = ((i - camera_coord->chunk_position_cell_x) * GFX_WIDTH_PX) + (k * SIZE_CHUNK * GFX_WIDTH_PX);
                    dest.y = ((j - camera_coord->chunk_position_cell_y) * GFX_HEIGHT_PX) + (l * SIZE_CHUNK * GFX_HEIGHT_PX);
                    
                    /* compute this cell coordinates to query the world system some information about it */
                    coord.world_position_chunk_x = k + camera_coord->world_position_chunk_x;
                    coord.world_position_chunk_y = l + camera_coord->world_position_chunk_y;
                    coord.chunk_position_cell_x = i;
                    coord.chunk_position_cell_y = j;

                    /* query the world system about our cell */
                    cell  = world_cell_get(&coord);

                    /* use available cell information on which gfx to draw */
                    src.x = GFX_WIDTH_PX  * cell->gfx.tileset_x; 
                    src.y = GFX_HEIGHT_PX * cell->gfx.tileset_y;
                
                    /* render the source gfx computed to the proper destination computed */
                    SDL_RenderCopy(gRenderer, gTileset, &src, &dest);
                
                }
            }

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

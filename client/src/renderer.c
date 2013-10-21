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

    src.w  = GFX_WIDTH_PX;
    src.h  = GFX_HEIGHT_PX;
    dest.w = GFX_WIDTH_PX;
    dest.h = GFX_HEIGHT_PX;

    screen_width_gfx    = gDisplayMode.w / GFX_WIDTH_PX  + 1;
    screen_height_gfx   = gDisplayMode.h / GFX_HEIGHT_PX + 1;
    screen_width_chunk  = screen_width_gfx / SIZE_CHUNK  + 2; 
    screen_height_chunk = screen_height_gfx / SIZE_CHUNK + 2;

    camera_coord = camera_get_coord(camera);
    
    printf("Gfx: %d, %d\n", screen_width_gfx, screen_height_gfx);
    printf("Screen: %d, %d\n", screen_width_chunk, screen_height_chunk);
    printf("World coord: %d, %d\n", camera_coord->world_position_chunk_x, camera_coord->world_position_chunk_y);
    printf("Chunk coord: %d, %d\n", camera_coord->chunk_position_cell_x, camera_coord->chunk_position_cell_y);
    printf("------\n");

    /* the rendering of all chunks */
    for (k=0; k < screen_width_chunk; k++) {
        for (l=0; l < screen_height_chunk; l++) {
            /*
             * current chunk x : camera_coord->world_position_chunk_x + k
             * current chunk y : camera_coord->world_position_chunk_y + l
             */ 
            printf("Rendering chunk (%d,%d)\n", camera_coord->world_position_chunk_x + k, camera_coord->world_position_chunk_y + l);

            /* the rendering of all cells */

            for (i=0; i<SIZE_CHUNK; i++) {
                dest.x = ((i - camera_coord->chunk_position_cell_x) * GFX_WIDTH_PX) + (k * SIZE_CHUNK * GFX_WIDTH_PX);
                for (j=0; j<SIZE_CHUNK; j++) {
                    dest.y = ((j - camera_coord->chunk_position_cell_y) * GFX_HEIGHT_PX) + (l * SIZE_CHUNK * GFX_HEIGHT_PX);
                    
                    coord = *camera_coord;
                    /* coord_apply(&coord, 0, i + (k * SIZE_CHUNK), j + (l * SIZE_CHUNK), 0); */
                    cell  = world_get_cell(&coord); /* will show same cell everywhere */
                    src.x = GFX_WIDTH_PX  * cell->gfx.tileset_x; 
                    src.y = GFX_HEIGHT_PX * cell->gfx.tileset_y;
                    
                    SDL_RenderCopy(gRenderer, gTileset, &src, &dest);
                }
            }

            /*
            for (i=0; i<SIZE_CHUNK; i++) {
                dest.x = (i * GFX_WIDTH_PX) + (k * SIZE_CHUNK * GFX_WIDTH_PX) - (camera_coord->chunk_position_cell_x * GFX_WIDTH_PX);
                for (j=0; j<SIZE_CHUNK; j++) {
                    dest.y = (j * GFX_HEIGHT_PX) + (l * SIZE_CHUNK * GFX_HEIGHT_PX) - (camera_coord->chunk_position_cell_y * GFX_HEIGHT_PX);
                    
                    coord = *camera_coord;
                    coord_apply(&coord, 0, i + (k * SIZE_CHUNK), j + (l * SIZE_CHUNK), 0);
                    cell  = world_get_cell(&coord);
                    src.x = GFX_WIDTH_PX  * cell->gfx.tileset_x; 
                    src.y = GFX_HEIGHT_PX * cell->gfx.tileset_y;
                    
                    SDL_RenderCopy(gRenderer, gTileset, &src, &dest);
                }
            }
            */
            /*
            for (i=camera_coord->chunk_position_cell_x; i<SIZE_CHUNK; i++) {
                dest.x = ((i - camera_coord->chunk_position_cell_x) * GFX_WIDTH_PX) + (l * SIZE_CHUNK * GFX_WIDTH_PX);
                for (j=camera_coord->chunk_position_cell_y; j<SIZE_CHUNK; j++) {
                    dest.y = (j * GFX_HEIGHT_PX) + (k * SIZE_CHUNK * GFX_HEIGHT_PX);
                    if (k == 0) {
                        dest.y -= camera_coord->chunk_position_cell_y * GFX_HEIGHT_PX;
                    }
                   
                    coord = *camera_coord;
                    coord_apply(&coord, 0, i + (l * SIZE_CHUNK), j + (k + SIZE_CHUNK), 0);
                    cell  = world_get_cell(&coord);
                    src.x = GFX_WIDTH_PX  * cell->gfx.tileset_x; 
                    src.y = GFX_HEIGHT_PX * cell->gfx.tileset_y;
                    
                    SDL_RenderCopy(gRenderer, gTileset, &src, &dest);
                }
            }
            */
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

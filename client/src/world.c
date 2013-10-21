#include "world.h"

CELL *gTestCell;

void world_init()
{
    gTestCell = cell_create(TILE_FLOOR, LAYER_GROUND);
}

void world_fini()
{
    cell_destroy(gTestCell);
}

void world_set_cell(COORD *coord, CELL *cell)
{
    /* coord->world_chunk_x  */
    /* coord->world_chunk_y */
    /* coord->chunk_inner_x */
    /* coord->chunk_inner_y */
    /* coord->layer */
}

CELL *world_get_cell(COORD *coord)
{
    /* position->x */
    /* position->y */

    return gTestCell;
}

void world_load_chunk(CHUNK *chunk)
{

}

void world_unload_chunk(CHUNK *chunk)
{
}

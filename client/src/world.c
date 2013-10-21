#include "world.h"

CELL *gTestCell;
CELL *gMagicCell;
CELL *gAnotherMagicCell;

void world_init()
{
    gTestCell        = cell_create(TILE_FLOOR, LAYER_GROUND);
    gMagicCell       = cell_create(TILE_DOOR, LAYER_OBJECTS);
    gAnotherMagicCell = cell_create(TILE_WALL, LAYER_OBJECTS);
}

void world_fini()
{
    cell_destroy(gTestCell);
    cell_destroy(gMagicCell);
    cell_destroy(gAnotherMagicCell);
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

    /* hack */
    if (coord->world_position_chunk_x == 0 && coord->world_position_chunk_y == 0) {
        return gMagicCell;
    }
    if (coord->world_position_chunk_x == 1 && coord->world_position_chunk_y == 1) {
        return gAnotherMagicCell;
    }

    return gTestCell;
}

void world_load_chunk(CHUNK *chunk)
{

}

void world_unload_chunk(CHUNK *chunk)
{
}

#include "world.h"

CELL *gTestCell;
CELL *gMagicCell;
CELL *gAnotherMagicCell;

void world_init(void)
{
    gTestCell         = cell_create(TILE_FLOOR, LAYER_GROUND);
    gMagicCell        = cell_create(TILE_DOOR, LAYER_OBJECTS);
    gAnotherMagicCell = cell_create(TILE_WALL, LAYER_OBJECTS);
}

void world_fini(void)
{
    cell_destroy(gTestCell);
    cell_destroy(gMagicCell);
    cell_destroy(gAnotherMagicCell);
}

void world_cell_set(COORD *coord, CELL *cell)
{
    /* coord->world_chunk_x  */
    /* coord->world_chunk_y */
    /* coord->chunk_inner_x */
    /* coord->chunk_inner_y */
    /* coord->layer */
}

CELL *world_cell_get(COORD *coord)
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

void world_chunk_load(CHUNK *chunk)
{

}

void world_chunk_unload(CHUNK *chunk)
{
}

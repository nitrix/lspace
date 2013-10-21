#include "cell.h"

CELL *cell_create(TILE tile, LAYER layer)
{
    CELL *cell  = calloc(1, sizeof (CELL));
    cell->tile  = tile;
    cell->layer = layer;
    cell->gfx   = gfx_find(tile, 0);
    return cell;
}

void cell_destroy(CELL *cell)
{
    free(cell);
}

#ifndef CELL_H
#define CELL_H

#include <stdlib.h>

#include "layer.h"
#include "tile.h"
#include "gfx.h"

typedef struct cell CELL;
struct cell {
    TILE   tile;
    GFX    gfx;
    LAYER  layer;
    CELL  *next;
};

/* TODO: implement linked list here  */

CELL *cell_create(TILE tile, LAYER layer);
void  cell_destroy(CELL *cell);

#endif /* CELL_H */

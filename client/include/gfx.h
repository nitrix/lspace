#ifndef GFX_H
#define GFX_H

#include "tile.h"

/* Dimensions of our gfx elements to draw in pixels */
#define GFX_WIDTH_PX  32
#define GFX_HEIGHT_PX 32

typedef struct gfx {
    unsigned int step;
    unsigned int tileset_x;
    unsigned int tileset_y;
} GFX;

GFX gfx_find(TILE tile, unsigned int step);

#endif /* GFX_H */

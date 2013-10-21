#include "gfx.h"

GFX gfx_find(TILE tile, unsigned int step) {
    GFX gfx;
    gfx.step = step;

    /* Default gfx in case of issues */
    gfx.tileset_x = 0;
    gfx.tileset_y = 0;

    /* TODO: better gfx system here, probably a table or something */
    if (tile == TILE_FLOOR && step == 0) {
        gfx.tileset_x = 0;
        gfx.tileset_y = 5;
        return gfx;
    }
}

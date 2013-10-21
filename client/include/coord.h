#ifndef COORD_H
#define COORD_H

#include "size.h"
#include "layer.h"

typedef struct coord {
    /* chunk position in the world */
    TYPE_SIZE_WORLD  world_position_chunk_x;
    TYPE_SIZE_WORLD  world_position_chunk_y;
    /* cell position in that chunk */
    TYPE_SIZE_CHUNK  chunk_position_cell_x;
    TYPE_SIZE_CHUNK  chunk_position_cell_y;
    /* layer position in that cell */
    LAYER            cell_position_layer;
} COORD;

void coord_translate(COORD *coord, COORD *offset);
void coord_apply(COORD *coord, int8_t off_top, int8_t off_right, int8_t off_bottom, int8_t off_left);

#endif /* COORD_H */

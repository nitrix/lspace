#include "coord.h"

void coord_translate(COORD *coord, COORD *offset)
{
    /* chunk position in the world */
    coord->world_position_chunk_x += offset->world_position_chunk_x;
    coord->world_position_chunk_y += offset->world_position_chunk_y;
    /* cell position in that chunk */
    coord->chunk_position_cell_x += offset->chunk_position_cell_x;
    coord->chunk_position_cell_y += offset->chunk_position_cell_y;
}

void coord_apply(COORD *coord, TYPE_SIZE_CHUNK off_top, TYPE_SIZE_CHUNK off_right, TYPE_SIZE_CHUNK off_bottom, TYPE_SIZE_CHUNK off_left)
{
    /* We're playing with integer promotions here, so do not deal with types > int8_t please,
     * unless you happen to know what you are doing. */

    /* use a signed integer to temporarily compute the position, as it allows negative values */
    int pos_x = coord->chunk_position_cell_x + off_right  - off_left;
    int pos_y = coord->chunk_position_cell_y + off_bottom - off_top;

    /* x axis */
    if (pos_x > SIZE_CHUNK - 1) {
        /* we reached chunk boundary to the right. substract 255 from calculated pos for the new chunk position,
         * and increment the world position by one chunk to the right. */
        pos_x -= SIZE_CHUNK;
        coord->world_position_chunk_x++;
    }
    else if (pos_x < -(SIZE_CHUNK - 1)) {
        /* we reached chunk boundary to the left. add 255 from calculated pos for the new chunk position,
         * and decrement the world position by one chunk to the left. */
        pos_x += SIZE_CHUNK;
        coord->world_position_chunk_x--;
    }
    else {
        /* everything is it order, we are still in the same chunk */
    }
    coord->chunk_position_cell_x = pos_x;
    
    /* y axis */
    if (pos_y > SIZE_CHUNK - 1) {
        /* we reached chunk boundary to the right. substract 255 from calculated pos for the new chunk position,
         * and increment the world position by one chunk to the right. */
        pos_y -= SIZE_CHUNK;
        coord->world_position_chunk_y++;
    }
    else if (pos_y < -(SIZE_CHUNK - 1)) {
        /* we reached chunk boundary to the left. add 255 from calculated pos for the new chunk position,
         * and decrement the world position by one chunk to the left. */
        pos_y += SIZE_CHUNK;
        coord->world_position_chunk_y--;
    }
    else {
        /* everything is it order, we are still in the same chunk */
    }
    coord->chunk_position_cell_y = pos_y;
}

/* COORD *coord_offset(COORD *a, COORD *b) {} */

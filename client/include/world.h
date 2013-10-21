#ifndef WORLD_H
#define WORLD_H

/*******************************************************************************
 *                             DOCUMENTATION !
 *******************************************************************************
 *
 * SINGLETON:
 *
 *  This module is meant to be initialized only once per application,
 *  then finished properly when you are done with it.
 *
 *  Use _init() and _fini() respectively.
 *
 ******************************************************************************/

#include "coord.h"
#include "cell.h"
#include "chunk.h"
#include "layer.h"

void world_init();
void world_fini();

void world_set_cell(COORD *coord, CELL *cell);
CELL *world_get_cell(COORD *coord);
void world_load_chunk(CHUNK *chunk);
void world_unload_chunk(CHUNK *chunk);

#endif /* WORLD_H */

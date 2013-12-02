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
 *******************************************************************************
 *
 * MECHANICS:
 *  
 *  _cell_() let you acces any cell in the world and do operations on them.
 *      _get() to obtain a cell at the specified coordinate.
 *      _set() to update a cell at the specified coordinate.
 *
 *  _chunk_() let you manage the chunks the game is dealing with.
 *      _load() to work with a chunk or simply prepare it ahead of time.
 *      _unload() to get rid of a chunk not needed anymore.
 *
 ******************************************************************************/

#include "coord.h"
#include "cell.h"
#include "chunk.h"
#include "layer.h"

/* Singleton */
void  world_init(void);
void  world_fini(void);

/* Operations on cells */
void  world_cell_set(COORD *coord, CELL *cell);
CELL *world_cell_get(COORD *coord);

/* Operatiosn on chunks */
void  world_chunk_load(CHUNK *chunk);
void  world_chunk_unload(CHUNK *chunk);

#endif /* WORLD_H */

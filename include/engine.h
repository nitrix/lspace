#ifndef ENGINE_H
#define ENGINE_H

#include "stdlib.h"
#include "ncurses.h"
#include "stage.h"

struct engine {
    WINDOW       *window;
    struct stage *stage;
};

void engine_init(struct engine *this);
void engine_fini(struct engine *this);
void engine_update(struct engine *this);
void engine_render(struct engine *this);
void engine_stage(struct engine *this, struct stage *stage);

#endif /* ENGINE_H */

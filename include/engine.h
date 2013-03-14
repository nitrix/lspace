#ifndef ENGINE_H
#define ENGINE_H

#include "stdlib.h"
#include "stdbool.h"
#include "ncurses.h"
#include "stage.h"

struct engine {
    WINDOW       *window;
    struct stage *stage;
    bool          running;
};

void engine_init(struct engine *this);
void engine_fini(struct engine *this);
void engine_update(struct engine *this);
void engine_render(struct engine *this);
void engine_run(struct engine *this);
void engine_stage(struct engine *this, struct stage *stage);

#endif /* ENGINE_H */

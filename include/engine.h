#ifndef ENGINE_H
#define ENGINE_H

#include "stage.h"
#include "stdbool.h"

struct engine {
    struct stage *stage;
    bool          running;
};

void engine_init(struct engine *this);
void engine_fini(struct engine *this);
void engine_run(struct engine *this);
void engine_stop(struct engine *this);
void engine_stage(struct engine *this, struct stage *stage);

#endif /* ENGINE_H */

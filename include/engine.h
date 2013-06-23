#ifndef ENGINE_H
#define ENGINE_H

#include "stdbool.h"
#include "renderer.h"

struct engine {
	bool running;
};

bool engine_init(struct engine* this);
bool engine_run(struct engine* this);
void engine_stop(struct engine* this);
void engine_inputs(void);

#endif /* ENGINE_H */

#ifndef RENDERER_H
#define RENDERER_H

#include "stdlib.h"
#include "ncurses.h"
#include "stage.h"

struct renderer {
    WINDOW       *window;
    struct stage *stage;
};

void renderer_init(struct renderer *this);
void renderer_fini(struct renderer *this);
void renderer_update(struct renderer *this);
void renderer_render(struct renderer *this);
void renderer_stage(struct renderer *this, struct stage *stage);

#endif /* RENDERER_H */

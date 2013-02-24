#ifndef RENDERER_H
#define RENDERER_H

#include "stdio.h"
#include "ncurses.h"

struct renderer {
    WINDOW* window;
    int     number;
};

void renderer_ctor(struct renderer *this);
void renderer_dtor(void);
void renderer_update(struct renderer *this);
void renderer_render(struct renderer *this);

#endif /* RENDERER_H */


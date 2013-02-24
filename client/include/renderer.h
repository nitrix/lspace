#ifndef RENDERER_H
#define RENDERER_H

#include "stdlib.h"
#include "ncurses.h"

struct renderer;

struct renderer *renderer_init(void);
void             renderer_fini(void);
void             renderer_update(struct renderer *this);
void             renderer_render(struct renderer *this);

#endif /* RENDERER_H */


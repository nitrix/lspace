#ifndef RENDERER_H
#define RENDERER_H

#include "stdio.h"
#include "stdbool.h"
#include "GL/freeglut.h"
#include "GL/gl.h"
#include "GL/glu.h"

bool renderer_init(void);
void renderer_update(void);
void renderer_render(void);

#endif /* RENDERER_H */


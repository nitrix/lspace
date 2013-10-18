#ifndef RENDERER_H
#define RENDERER_H

#include <stdlib.h>
#include <stdio.h>

#include "SDL.h"
#include "SDL_image.h"

void renderer_init();
void renderer_fini();
void renderer_clear();
void renderer_render();

#endif //RENDERER_H

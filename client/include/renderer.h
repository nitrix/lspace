#ifndef RENDERER_H
#define RENDERER_H

#include <stdlib.h>

#include "SDL2/SDL.h"
#include "SDL2/SDL_opengl.h"
#include "SDL2/SDL_image.h"

#include "camera.h"

void renderer_init();
void renderer_fini();
void renderer_load_tileset();
void renderer_clear();
void renderer_render(Camera *camera);

#endif //RENDERER_H

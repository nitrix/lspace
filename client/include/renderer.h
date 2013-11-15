#ifndef RENDERER_H
#define RENDERER_H

/*******************************************************************************
 *                             DOCUMENTATION !
 *******************************************************************************
 *
 * SINGLETON:
 *
 *  This module is meant to be initialized only once per application,
 *  then finished properly when you are done with it.
 *
 *  Use _init() and _fini() respectively.
 *
 *******************************************************************************
 *
 * MECHANICS:
 *  
 *  _render() renders every visible cells by the player with the help of
 *  the camera, coord and world module. It has to be called periodically to
 *  provide good visual feebacks to the user. Such operation is often referred
 *  to as one frame.
 *
 *  _clear() clears the screen (usually called in-between frames) to prevent
 *  visual glitchs that would happen with transparent objects moved or removed.
 *
 ******************************************************************************/

#include <stdlib.h>
#include <stdio.h>

#include "SDL.h"
#include "SDL_image.h"

#include "camera.h"
#include "cell.h"
#include "world.h"

void renderer_init();
void renderer_fini();
void renderer_render(CAMERA *camera);
void renderer_clear(); 

#endif /* RENDERER_H */

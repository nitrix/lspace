#ifndef MAIN_H
#define MAIN_H

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#include "SDL.h"
#include "renderer.h"
#include "camera.h"

int    main(int argc, char *argv[]);
void   main_event_render();
void   main_terminate();

#endif /* MAIN_H */

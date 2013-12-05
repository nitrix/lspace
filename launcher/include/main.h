#ifndef MAIN_H
#define MAIN_H

#include "stdio.h"
#include "SDL2/SDL.h"
#include "SDL/SDL_ttf.h"
#include "list.h"

typedef struct star {
    unsigned char r, g, b;
    int           x, y;
    unsigned char alpha;
} STAR;

int main(int argc, const char *argv[]);

#endif /* MAIN_H */

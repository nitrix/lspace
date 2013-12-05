#ifndef MAIN_H
#define MAIN_H

#include "stdio.h"
#include "SDL2/SDL.h"
#include "list.h"

typedef struct star {
    unsigned char r, g, b;
    int           x, y;
    unsigned char alpha;
} STAR;

int main(void);

#endif /* MAIN_H */

#ifndef CAMERA_H
#define CAMERA_H

#include <stdlib.h>

#include "coord.h"

typedef struct camera {
    COORD coord; /* coordinates of the top-left corner */
} CAMERA;

CAMERA  *camera_create();
void    camera_destroy(CAMERA *camera);
void    camera_move(CAMERA *camera, COORD *new_coord);
COORD   *camera_coord_get();

#endif /* CAMERA_H */

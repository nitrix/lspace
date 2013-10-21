#include "camera.h"

CAMERA *camera_create()
{
    return calloc(1, sizeof (CAMERA));
}

void camera_destroy(CAMERA *camera)
{
    free(camera);
}

void camera_move(CAMERA *camera, COORD *new_coord)
{
    camera->coord = *new_coord;
}

COORD *camera_get_coord(CAMERA *camera)
{
    return &camera->coord;
}

#ifndef LAYER_H
#define LAYER_H

typedef enum layer {
    LAYER_VOID,
    LAYER_GROUND,
    LAYER_OBJECTS,
    LAYER_ENTITIES,
    LAYER_FLOATING,
    /* used for counts */
    LAYER_END
} LAYER;

#endif /* LAYER_H */

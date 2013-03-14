#ifndef STAGE_H
#define STAGE_H

#include "stdlib.h"
#include "utils.h"

struct stage {
    void (*update)(struct stage *);
    void (*render)(struct stage *);
};

void stage_init(struct stage *this);
void stage_fini(struct stage *this);
void stage_noop(struct stage *this);

#endif /* STAGE_H */

#include "stage.h"

void stage_init(struct stage *this)
{
    /* Initialize virtual member functions with placeholders */
    this->update = stage_noop;
    this->render = stage_noop;
}

void stage_fini(struct stage *this)
{
    UNUSED(this);
}

void stage_noop(struct stage *this)
{
    /* Placeholder for implementations */
    UNUSED(this);
}

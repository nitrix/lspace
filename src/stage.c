#include "stage.h"

void stage_init(struct stage *this)
{
    /* Initialize virtual member functions with placeholders */
    this->update = stage_noop;
}

void stage_fini(struct stage *this)
{
    UNUSED_PARAM(this);
}

void stage_noop(struct stage *this)
{
    /* Placeholder for implementations */
    UNUSED_PARAM(this);
}

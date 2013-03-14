#include "engine.h"

void engine_init(struct engine *this)
{
    /* Initialize member variables */
    this->stage = NULL;
}

void engine_fini(struct engine *this)
{
    UNUSED(this);
}

void engine_stage(struct engine *this, struct stage *stage)
{
    this->stage = stage;
}

void engine_run(struct engine *this)
{
    /* Run as long as there is a stage */
    while (this->stage) {
        this->stage->update(this->stage);
        this->stage->render(this->stage);
    }
}

void engine_stop(struct engine *this)
{
    this->stage = NULL;
}

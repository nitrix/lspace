#include "engine.h"

void engine_init(struct engine *this)
{
    /* Initialize member variables */
    this->stage   = NULL;
}

void engine_fini(struct engine *this)
{
    UNUSED(this);
}

void engine_stage(struct engine *this, struct stage *stage)
{
    this->stage   = stage;
    stage->engine = this;
}

void engine_run(struct engine *this)
{
    /* Run only if it has a stage */
    if (this->stage) {
        this->running = true;

        /* As long as engine_stop() isn't called */
        while (this->running) {
            this->stage->update(this->stage);
            this->stage->render(this->stage);
        }
    }
}

void engine_stop(struct engine *this)
{
    this->running = false;
}

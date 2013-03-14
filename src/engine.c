#include "engine.h"

void engine_init(struct engine *this)
{
    /* Allocate member objects */
    WINDOW       *window = initscr();

    /* Initialize member variables */
    this->stage   = NULL;
    this->window  = window;
    this->running = true;
}

void engine_fini(struct engine *this)
{
    UNUSED(this);

    /* Deallocate member objects */
    endwin();
}

void engine_stage(struct engine *this, struct stage *stage)
{
    this->stage = stage;
}

void engine_update(struct engine *this)
{
    /* We call update() on whatever is our current stage implementation */
    if (this->stage)
        this->stage->update(this->stage);
}

void engine_render(struct engine *this)
{
    wrefresh(this->window);
}

void engine_run(struct engine *this)
{
    while (this->running) {
        engine_update(this);
        engine_render(this);
    }
}

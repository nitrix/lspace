#include "renderer.h"

void renderer_init(struct renderer *this)
{
    /* Allocate member objects */
    WINDOW       *window = initscr();

    /* Initialize member variables */
    this->stage  = NULL;
    this->window = window;
}

void renderer_fini(struct renderer *this)
{
    UNUSED_PARAM(this);

    /* Deallocate member objects */
    endwin();
}

void renderer_stage(struct renderer *this, struct stage *stage)
{
    this->stage = stage;
}

void renderer_update(struct renderer *this)
{
    /* We call update() on whatever is our current stage implementation */
    if (this->stage)
        this->stage->update(this->stage);
}

void renderer_render(struct renderer *this)
{
    wrefresh(this->window);
}

#include "stage_game.h"

void stage_game_init(struct stage_game *this)
{
    /* Initialize virtual member functions */
    this->stage.update = stage_game_update;
    this->stage.render = stage_game_render;

    /* Initialize member variables */
    this->test = 0;
}

void stage_game_fini(struct stage_game *this)
{
    UNUSED(this);
}

void stage_game_update(struct stage *this)
{
    /* TEMPORARY */
    UNUSED(this);
}

void stage_game_render(struct stage *this)
{
    /* TEMPORARY */
    UNUSED(this);
}

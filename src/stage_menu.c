#include "stage_menu.h"

void stage_menu_init(struct stage_menu *this)
{
    /* Initialize virtual member functions */
    this->stage.update = stage_menu_update;
    this->stage.render = stage_menu_render;

    /* Initialize member variables */
    this->test = 0;
}

void stage_menu_fini(struct stage_menu *this)
{
    UNUSED(this);
}

void stage_menu_update(struct stage *this)
{
    /* TEMPORARY */
    UNUSED(this);
}

void stage_menu_render(struct stage *this)
{
    /* TEMPORARY */
    UNUSED(this);
}

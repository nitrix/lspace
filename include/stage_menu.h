#ifndef STAGE_MENU_H
#define STAGE_MENU_H

#include "stage.h"
#include "stdio.h"

struct stage_menu {
    struct stage stage;
    int          test;
};

void stage_menu_init(struct stage_menu *this);
void stage_menu_fini(struct stage_menu *this);
void stage_menu_update(struct stage *this);
void stage_menu_render(struct stage *this);

#endif /* STAGE_MENU_H */

#ifndef STAGE_MENU_H
#define STAGE_MENU_H

#include "stage.h"
#include "stdio.h"
#include "ncurses.h"

/* Our options */
#define OPTION_NEW      0
#define OPTION_CONTINUE 1
#define OPTION_SETTINGS 2
#define OPTION_QUIT     3

struct stage_menu {
    struct stage  stage;
    WINDOW       *window;
    int           current_option;
};

void stage_menu_init(struct stage_menu *this);
void stage_menu_fini(struct stage_menu *this);
void stage_menu_update(struct stage *this);
void stage_menu_render(struct stage *this);

#endif /* STAGE_MENU_H */

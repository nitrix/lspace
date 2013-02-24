#include "renderer.h"

void renderer_ctor(struct renderer *this)
{
    this->window = initscr();
    this->number = 0;
}

void renderer_dtor(void)
{
    endwin();
}

void renderer_update(struct renderer *this)
{
    //Some game logic that change stuff on screen
    mvwprintw(this->window, 0, 0, "Hello World #%d", this->number);
    this->number++;
}

void renderer_render(struct renderer *this)
{
    wrefresh(this->window);
}

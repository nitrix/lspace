#include "renderer.h"

struct renderer {
    WINDOW* window;
    int     number;
};

struct renderer *renderer_init(void)
{
    struct renderer *this = malloc(sizeof(struct renderer));
    
    this->window = initscr();
    this->number = 0;

    return this;
}

void renderer_fini(void)
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

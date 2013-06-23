#include "renderer.h"

void renderer_init(void)
{
	initscr();
}

void renderer_render(void)
{
	/* TODO: print stuff */
	printw("Hello World!\n");

	refresh();
}

void renderer_fini(void)
{
	endwin();
}

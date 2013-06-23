#include "engine.h"

bool engine_init(struct engine* this) {
    	/* TODO: parse command line parameters */
	/* TODO: load configuration file */
	
	this->running = false;
	return true;
}

bool engine_run(struct engine* this) {
	this->running = true;
	renderer_init();

	/* main loop */
	while (this->running) {
		renderer_render();
		engine_inputs();
	}

	renderer_fini();
	return true; /* finished gracefully */
}

void engine_stop(struct engine* this) {
	this->running = false;
}

void engine_inputs(void) {
	noecho(); /* do not print the keys pressed */
	cbreak(); /* do not wait for Enter on getch */

	/* TODO: handle user inputs */
	getch();
}

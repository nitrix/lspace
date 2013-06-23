#include "engine.h"

bool engine_init(struct engine* this) {
    	/* TODO: parse command line parameters */
	/* TODO: load configuration file */
	
	renderer_init();
	
	this->running = false;
	return true;
}

bool engine_run(struct engine* this) {
	this->running = true;

	/* main loop */
	while (this->running) {
		renderer_render();
		engine_handle_inputs();
	}

	return true; /* finished gracefully */
}

void engine_stop(struct engine* this) {
	this->running = false;
}

void engine_fini(struct engine* this) {
	if (this->running) {
		/* TODO: the engine was running when it was forcefully stopped */
	}

	renderer_fini();
}

void engine_handle_inputs(void) {
	noecho(); /* do not print the keys pressed */
	cbreak(); /* do not wait for Enter on getch */

	/* TODO: handle user inputs */
	getch();
}

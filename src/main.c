#include "main.h"

struct engine g_engine;

int main(void) {
	signal(SIGTERM, terminate); /* termination request sent to the program  */
	signal(SIGABRT, terminate); /* abnormal termination */
	signal(SIGSEGV, terminate); /* invalid memory access */
	/* TODO: signal(SIGINT,  terminate); interactive attention request sent to the program */
	/* Careful, this handles CTRL-C */

	if (engine_init(&g_engine)) {
		if (engine_run(&g_engine)) {
			return EXIT_SUCCESS;
		}
	}
	return EXIT_FAILURE;
}


void terminate(int signal) {
	UNUSED(signal); /* TODO: something with this */
	engine_fini(&g_engine);
}

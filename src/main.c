#include "main.h"

int main(void) {
	struct engine engine;

	if (engine_init(&engine)) {
		if (engine_run(&engine)) {
			return EXIT_SUCCESS;
		}
	}
	return EXIT_FAILURE;
}

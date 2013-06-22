#include "main.h"

int main(void) {
	if (engine_init()) {
		if (engine_run()) {
			return EXIT_SUCCESS;
		}
	}
	return EXIT_FAILURE;
}

#include "main.h"

int main(int argc, char *argv[])
{
    renderer_init();
    
    renderer_render();
    
    sleep(5);
    renderer_fini();

    return EXIT_SUCCESS;
} 

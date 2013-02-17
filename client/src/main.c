#include "main.h"

int main(int argc, char *argv[])
{
    //Initialize freeGLUT
    glutInit(&argc, argv);

    //Create OpenGL 2.1 context for old-style fixed functions pipeline (temporary)
    glutInitContextVersion(2, 1);

    //Create Double Buffered Window
    glutInitDisplayMode(GLUT_DOUBLE);
    glutInitWindowSize(800, 600);
    glutCreateWindow("Lonesome Space");

    //Do post window/context creation initialization
    if(!renderer_init()) {
        printf("Unable to initialize graphics library!\n");
        return EXIT_FAILURE;
    }

    //Set rendering function
    glutDisplayFunc(renderer_render);

    //Set main loop
    glutTimerFunc(1000 / 60, runMainLoop, 0);

    //Start GLUT main loop
    glutMainLoop();

    printf("It works; client here\n");
    return EXIT_SUCCESS;
}

void runMainLoop(int val)
{
    //Frame logic
    renderer_update();
    renderer_render();

    //Run frame one more time
    glutTimerFunc(1000/60, runMainLoop, val);
}

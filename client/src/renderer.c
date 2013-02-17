#include "renderer.h"

bool renderer_init(void)
{
    //Initialize projection matrix
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0.0, 800, 600, 0.0, 1.0, -1.0);

    //Initialize modelview matrix
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    //Set clear color to black
    glClearColor(0.f, 0.f, 0.f, 1.f);

    //Check for any errors
    GLenum error = glGetError();
    if (error != GL_NO_ERROR) {
        printf("Error initializing OpenGL: %s\n", gluErrorString(error));
        return false;
    } else {
        return true;
    }
}

void renderer_update(void)
{
    //There's no real game logic to update yet.
}

void renderer_render(void)
{
    //Clear color buffer
    glClear(GL_COLOR_BUFFER_BIT);

    //Reset modelview matrix
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    //Move to center of the screen
    glTranslatef(800/2.f, 600/2.f, 0.f);
    
    //Render a quad
    glBegin(GL_QUADS);
        glColor3f(0.f, 1.f, 1.f);
        glVertex2f(-0.50f, -0.50f);
        glVertex2f( 0.50f, -0.50f);
        glVertex2f( 0.50f,  0.50f);
        glVertex2f(-0.50f,  0.50f);
    glEnd();

    //Update screen
    glutSwapBuffers();
}

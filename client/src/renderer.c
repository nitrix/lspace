#include "renderer.h"

SDL_Window   *gWindow;
SDL_Renderer *gRenderer;
// SDL_Texture  *gTileset;

GLfloat light_diffuse[] = {1.0, 0.0, 0.0, 1.0};  /* Red diffuse light. */
GLfloat light_position[] = {1.0, 1.0, 1.0, 0.0};  /* Infinite light location. */
GLfloat n[6][3] = {  /* Normals for the 6 faces of a cube. */
  {-1.0, 0.0, 0.0}, {0.0, 1.0, 0.0}, {1.0, 0.0, 0.0},
  {0.0, -1.0, 0.0}, {0.0, 0.0, 1.0}, {0.0, 0.0, -1.0} };
GLint faces[6][4] = {  /* Vertex indices for the 6 faces of a cube. */
  {0, 1, 2, 3}, {3, 2, 6, 7}, {7, 6, 5, 4},
  {4, 5, 1, 0}, {5, 6, 2, 1}, {7, 4, 0, 3} };
GLfloat v[8][3];  /* Will be filled in with X,Y,Z vertexes. */

void renderer_init()
{
    // Load SDL
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO | SDL_INIT_TIMER | SDL_INIT_EVENTS) != 0) {
        fprintf(stderr, "Unable to initialize SDL: %s\n", SDL_GetError());
        exit(EXIT_FAILURE);
    }
    
    // Force opengl version
    //SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION,4);
    //SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION,2);
    
    // Create a window and renderer for the game
    SDL_CreateWindowAndRenderer(0, 0, SDL_WINDOW_FULLSCREEN_DESKTOP, &gWindow, &gRenderer);

    if (!gWindow || !gRenderer) {
        fprintf(stderr, "Unable to create a window or renderer: %s\n", SDL_GetError());
        exit(EXIT_FAILURE);
    }

    SDL_GLContext context = SDL_GL_CreateContext(gWindow);
    const char *version = glGetString(GL_VERSION);
    if (!version) {
        fprintf(stderr, "Unable to get OpenGL version\n");
        exit(EXIT_FAILURE);
    } else {
        printf("OpenGL version: %s\n", version);
    }
    
    /* We could enforce a logical size and have the system emulate it.
     *
     * SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "linear"); // make the scaled rendering look smoother.
     * SDL_RenderSetLogicalSize(renderer, 640, 480);
     * 
     * Note that 640x480 and 1920x1200 aren't the same aspect ratio: SDL takes care of that, too,
     * scaling as much as possible and letterboxing the difference.
     */
    
    // Load SDL_image
    IMG_Init(IMG_INIT_JPG | IMG_INIT_PNG);

    // Setup cube vertex data
    v[0][0] = v[1][0] = v[2][0] = v[3][0] = -1;
    v[4][0] = v[5][0] = v[6][0] = v[7][0] = 1;
    v[0][1] = v[1][1] = v[4][1] = v[5][1] = -1;
    v[2][1] = v[3][1] = v[6][1] = v[7][1] = 1;
    v[0][2] = v[3][2] = v[4][2] = v[7][2] = 1;
    v[1][2] = v[2][2] = v[5][2] = v[6][2] = -1;

    // Enable a single OpenGL light.
    glLightfv(GL_LIGHT0, GL_DIFFUSE, light_diffuse);
    glLightfv(GL_LIGHT0, GL_POSITION, light_position);
    glEnable(GL_LIGHT0);
    glEnable(GL_LIGHTING);

    // Use depth buffering for hidden surface elimination.
    glEnable(GL_DEPTH_TEST);

    // Setup the view of the cube.
    glMatrixMode(GL_PROJECTION);
    gluPerspective(40.0, // field of view in degree
    1.0, // aspect ratio 
    1.0, // Z near
    10.0); // Z far
    glMatrixMode(GL_MODELVIEW);

    // Prepare the camera
    gluLookAt(0.0, 0.0, 5.0, // eye is at (0,0,5)
    0.0, 0.0, 0.0, // center is at (0,0,0)
    0.0, 1.0, 0.0); // up is in positive Y direction

    // TODO: Adjust cube position to be asthetic angle.
    glTranslatef(0.0, 0.0, -1.0);
    glRotatef(60, 1.0, 0.0, 0.0);
    glRotatef(-20, 0.0, 0.0, 1.0);
}


void renderer_clear()
{
    // Clear the screen
    SDL_SetRenderDrawColor(gRenderer, 0, 0, 0, 255);
    SDL_RenderClear(gRenderer);
}

void renderer_render(Camera *camera)
{
    renderer_clear();

    

    //TODO: draw elements here
    /*
    SDL_Rect src, dest;
    src.x  = 0;
    src.y  = 0;
    src.w  = 38;
    src.h  = 38;
    
    dest.x = 0;
    dest.y = 0;
    dest.w = 380;
    dest.h = 380;
    SDL_RenderCopy(gRenderer, gTileset, &src, &dest);
    */
    int i;

    for (i = 0; i < 6; i++) {
        glBegin(GL_QUADS);
        glNormal3fv(&n[i][0]);
        glVertex3fv(&v[faces[i][0]][0]);
        glVertex3fv(&v[faces[i][1]][0]);
        glVertex3fv(&v[faces[i][2]][0]);
        glVertex3fv(&v[faces[i][3]][0]);
        glEnd();
    }

    SDL_RenderPresent(gRenderer);
}

void renderer_fini()
{
    SDL_DestroyWindow(gWindow);
    SDL_Quit();
}

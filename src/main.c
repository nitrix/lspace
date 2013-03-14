#include "main.h"

/* Object guidelines
 *
 * - We use objects.
 * - You will manage your objects yourself using new() and delete().
 * - You will never forget to call *_init() and *_fini() on objects.
 * - You will never initialize or finalize -twice- the same objects.
 * 
 */

int main(void)
{
    /* Allocate objects */
    struct renderer   *renderer   = malloc(sizeof(struct renderer));
    struct stage_menu *stage_menu = malloc(sizeof(struct stage_menu));
    struct stage_game *stage_game = malloc(sizeof(struct stage_game)); 

    /* Initialize objects */
    renderer_init(renderer);
    stage_menu_init(stage_menu);
    stage_game_init(stage_game);

    /* Set the correct stage */
    renderer_stage(renderer, (struct stage*)stage_menu);
    
    /* Main loop */
    while (true) {
        renderer_update(renderer);
        renderer_render(renderer);
    }

    /* Finalize objects */
    renderer_fini(renderer);
    stage_menu_fini(stage_menu);
    stage_game_fini(stage_game);

    /* Deallocate objects */
    free(renderer);
    free(stage_menu);
    free(stage_game);

    return EXIT_SUCCESS;
}

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
    struct engine     *engine     = malloc(sizeof(struct engine));
    struct stage_menu *stage_menu = malloc(sizeof(struct stage_menu));
    struct stage_game *stage_game = malloc(sizeof(struct stage_game)); 

    /* Initialize objects */
    engine_init(engine);
    stage_menu_init(stage_menu);
    stage_game_init(stage_game);

    /* Set the correct stage */
    engine_stage(engine, (struct stage*)stage_menu);
    
    /* Main loop */
    while (true) {
        engine_update(engine);
        engine_render(engine);
    }

    /* Finalize objects */
    engine_fini(engine);
    stage_menu_fini(stage_menu);
    stage_game_fini(stage_game);

    /* Deallocate objects */
    free(engine);
    free(stage_menu);
    free(stage_game);

    return EXIT_SUCCESS;
}

#ifndef STAGE_GAME_H
#define STAGE_GAME_H

#include "stdio.h"
#include "stage.h"

struct stage_game {
    struct stage stage;
    int          test;
};

void stage_game_init(struct stage_game *this);
void stage_game_fini(struct stage_game *this);
void stage_game_update(struct stage *this);
void stage_game_render(struct stage *this);

#endif /* STAGE_GAME_H */

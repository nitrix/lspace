#include "stage_menu.h"

void stage_menu_init(struct stage_menu *this)
{
    /* Initialize virtual member functions */
    this->stage.update = stage_menu_update;
    this->stage.render = stage_menu_render;

    /* Initialize member variables */
    this->current_option = 0;
    this->window = initscr();

    nonl(); /* no line break */
    noecho(); /* no output */
    curs_set(0); /* invisible */
    keypad(this->window, true);
}

void stage_menu_fini(struct stage_menu *this)
{
    UNUSED(this);
    endwin();
}

void stage_menu_update(struct stage *this)
{
    /* Initializing variables */
    struct stage_menu *local = (struct stage_menu *)this;
    int logo_x, i, s;
    char *options[4];
    
    /* Prepare the labels for the menu */
    options[0] = "New game";
    options[1] = "Continue game";
    options[2] = "Settings";
    options[3] = "Quit";

    /* Detect the size of the shell running our game */
    logo_x = getmaxx(local->window);
    logo_x = (logo_x/ 2) - (65 / 2); /* magic number */
    
    /* Draw the logo */
    mvwprintw(local->window,  1,logo_x, "             ,-,--.     _ __    ,---.       _,.----.       ,----. ");
    mvwprintw(local->window,  2,logo_x, "   _.-.    ,-.'-  _\\ .-`.' ,`..--.'  \\    .' .' -   \\   ,-.--` , \\");
    mvwprintw(local->window,  3,logo_x, " .-,.'|   /==/_ ,_.'/==/, -   \\==\\-/\\ \\  /==/  ,  ,-'  |==|-  _.-`");
    mvwprintw(local->window,  4,logo_x, "|==|, |   \\==\\  \\  |==| _ .=. /==/-|_\\ | |==|-   |  .  |==|   `.-.");
    mvwprintw(local->window,  5,logo_x, "|==|- |    \\==\\ -\\ |==| , '=',\\==\\,   - \\|==|_   `-' \\/==/_ ,    /");
    mvwprintw(local->window,  6,logo_x, "|==|, |    _\\==\\ ,\\|==|-  '..'/==/ -   ,||==|   _  , ||==|    .-' ");
    mvwprintw(local->window,  7,logo_x, "|==|- `-._/==/\\/ _ |==|,  |  /==/-  /\\ - \\==\\.       /|==|_  ,`-._");
    mvwprintw(local->window,  8,logo_x, "/==/ - , ,|==\\ - , /==/ - |  \\==\\ _.\\=\\.-'`-.`.___.-' /==/ ,     /");
    mvwprintw(local->window,  9,logo_x, "`--`-----' `--`---'`--`---'   `--`                    `--`-----`` ");

    mvwprintw(local->window, 11,logo_x+25, "LONESOME SPACE,");
    mvwprintw(local->window, 13,logo_x+23, "version 0.0.1 alpha");

    /* Show the menu options based on current selection */
    s = sizeof(options) / sizeof(options[0]);
    for(i=0; i<s; i++) {
        if (i == local->current_option) {
            mvwaddch(local->window, 16+(i*2),logo_x-2, ACS_DIAMOND);
            attron(A_STANDOUT);
        } else {
            mvwaddch(local->window, 16+(i*2),logo_x-2, ' ');
        }
        mvwprintw(local->window, 16+(i*2),logo_x, " %s ", options[i]);
        attron(A_BOLD);
        mvwprintw(local->window, 16+(i*2),logo_x+1, "%c", options[i][0]);
        attroff(A_BOLD);
        if (i == local->current_option) {
            attroff(A_STANDOUT);
        }
    }

    /* Wait for inputs and process them as they arrive */
    int ch  = wgetch(local->window);
    int old = local->current_option;
    handle_inputs:
    switch (ch) {
        /* -------------------------------- */
        case 'n': local->current_option = OPTION_NEW;      break;
        case 'c': local->current_option = OPTION_CONTINUE; break;
        case 's': local->current_option = OPTION_SETTINGS; break;
        case 'q': local->current_option = OPTION_QUIT;     break;
        /* -------------------------------- */
        case KEY_RESIZE:
            wclear(local->window);
            break;
        /* -------------------------------- */
        case 'j':
        case KEY_DOWN:
            if (s - local->current_option - 1) {
                local->current_option++;
            } else {
                local->current_option = OPTION_NEW;
            }
            break;
        /* -------------------------------- */
        case 'k':
        case KEY_UP:
            if (local->current_option) {
                local->current_option--;
            } else {
                local->current_option = OPTION_QUIT;
            }
            break;
        /* -------------------------------- */
        case '\r':
            switch (local->current_option) {
                /* ----------------- */
                case OPTION_NEW:
                case OPTION_CONTINUE:
                case OPTION_SETTINGS:
                    break;
                /* ----------------- */
                case OPTION_QUIT:
                    engine_stop(this->engine);
                    break;
                /* ----------------- */
            }
            break;
        /* -------------------------------- */
        default:
            old = -1;
    }
    
    if (old == local->current_option && ch != '\r') {
        ch = '\r';
        goto handle_inputs;
    }
}

void stage_menu_render(struct stage *this)
{
    struct stage_menu *local = (struct stage_menu *)this;
    wrefresh(local->window);
}

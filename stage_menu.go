package main

import "github.com/nsf/termbox-go"

type StageMenu struct {
	currentSelection int
}

func (stage StageMenu) Update() {
	//fmt.Println("StageMenu Update()")
}

func (stage StageMenu) Render() {
	//fmt.Println("StageMenu Render()")

	stage.DrawLogo()

	/*
	options := []string {
		"Continue game",
		"New game",
		"Settings",
		"Quit",
	}
	*/
}

func (stage *StageMenu) HandleEvents(e *Engine) {
	ev := termbox.PollEvent()
	switch ev.Type {
		case termbox.EventKey:

			//fmt.Printf("%d - %d!\n", ev.Ch, ev.Key)

			switch ev.Key {
				case termbox.KeyCtrlZ, termbox.KeyCtrlX, termbox.KeyCtrlC:
					e.Stop()
			}
			
			//termbox.Clear(termbox.ColorDefault, termbox.ColorDefault)
			//draw_keyboard()
			//dispatch_press(&ev)
			//pretty_print_press(&ev)
			//termbox.Flush()
		case termbox.EventResize:
			termbox.Clear(termbox.ColorDefault, termbox.ColorDefault)
			stage.Render()
			termbox.Flush()
		case termbox.EventError:
			panic(ev.Err)
	}
}

func (stage StageMenu) DrawLogo() {
	width, _ := termbox.Size()
        print_tb(width/2-33, 1, termbox.ColorWhite, termbox.ColorDefault, "             ,-,--.     _ __    ,---.       _,.----.       ,----. \n");
        print_tb(width/2-33, 2, termbox.ColorWhite, termbox.ColorDefault, "   _.-.    ,-.'-  _\\ .-`.' ,`..--.'  \\    .' .' -   \\   ,-.--` , \\\n");
        print_tb(width/2-33, 3, termbox.ColorWhite, termbox.ColorDefault, " .-,.'|   /==/_ ,_.'/==/, -   \\==\\-/\\ \\  /==/  ,  ,-'  |==|-  _.-`\n");
        print_tb(width/2-33, 4, termbox.ColorWhite, termbox.ColorDefault, "|==|, |   \\==\\  \\  |==| _ .=. /==/-|_\\ | |==|-   |  .  |==|   `.-.\n");
        print_tb(width/2-33, 5, termbox.ColorWhite, termbox.ColorDefault, "|==|- |    \\==\\ -\\ |==| , '=',\\==\\,   - \\|==|_   `-' \\/==/_ ,    /\n");
        print_tb(width/2-33, 6, termbox.ColorWhite, termbox.ColorDefault, "|==|, |    _\\==\\ ,\\|==|-  '..'/==/ -   ,||==|   _  , ||==|    .-' \n");
        print_tb(width/2-33, 7, termbox.ColorWhite, termbox.ColorDefault, "|==|- `-._/==/\\/ _ |==|,  |  /==/-  /\\ - \\==\\.       /|==|_  ,`-._\n");
        print_tb(width/2-33, 8, termbox.ColorWhite, termbox.ColorDefault, "/==/ - , ,|==\\ - , /==/ - |  \\==\\ _.\\=\\.-'`-.`.___.-' /==/ ,     /\n");
        print_tb(width/2-33, 9, termbox.ColorWhite, termbox.ColorDefault, "`--`-----' `--`---'`--`---'   `--`                    `--`-----`` \n");
	termbox.Flush()
}

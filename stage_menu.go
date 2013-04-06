package main

import "github.com/nsf/termbox-go"

type StageMenu struct {
	CurrentSelection int
}

func (stage *StageMenu) Update() {
	//fmt.Println("StageMenu Update()")
}

func (stage *StageMenu) Render() {
	termbox.Clear(termbox.ColorDefault, termbox.ColorDefault)
	stage.DrawLogo()
	stage.DrawMenu()
	termbox.Flush()
}

func (stage *StageMenu) HandleEvents(e *Engine) {
	ev := termbox.PollEvent()
	switch ev.Type {
		case termbox.EventKey:
			switch ev.Key {
				case termbox.KeyCtrlZ, termbox.KeyCtrlX, termbox.KeyCtrlC:
					e.Stop()
				case termbox.KeyArrowDown:
					stage.CurrentSelection++

				case termbox.KeyArrowUp:
					stage.CurrentSelection--
					//termbox.Clear(termbox.ColorDefault, termbox.ColorDefault)
					//stage.Render()
			}

		case termbox.EventResize:
			stage.Render()

		case termbox.EventError:
			panic(ev.Err)
	}
}

func (stage StageMenu) DrawLogo() {
	width, height := termbox.Size()
        print_tb(width/2-33, 1, termbox.ColorWhite, termbox.ColorDefault, "             ,-,--.     _ __    ,---.       _,.----.       ,----. \n")
        print_tb(width/2-33, 2, termbox.ColorWhite, termbox.ColorDefault, "   _.-.    ,-.'-  _\\ .-`.' ,`..--.'  \\    .' .' -   \\   ,-.--` , \\\n")
        print_tb(width/2-33, 3, termbox.ColorWhite, termbox.ColorDefault, " .-,.'|   /==/_ ,_.'/==/, -   \\==\\-/\\ \\  /==/  ,  ,-'  |==|-  _.-`\n")
        print_tb(width/2-33, 4, termbox.ColorWhite, termbox.ColorDefault, "|==|, |   \\==\\  \\  |==| _ .=. /==/-|_\\ | |==|-   |  .  |==|   `.-.\n")
        print_tb(width/2-33, 5, termbox.ColorWhite, termbox.ColorDefault, "|==|- |    \\==\\ -\\ |==| , '=',\\==\\,   - \\|==|_   `-' \\/==/_ ,    /\n")
        print_tb(width/2-33, 6, termbox.ColorWhite, termbox.ColorDefault, "|==|, |    _\\==\\ ,\\|==|-  '..'/==/ -   ,||==|   _  , ||==|    .-' \n")
        print_tb(width/2-33, 7, termbox.ColorWhite, termbox.ColorDefault, "|==|- `-._/==/\\/ _ |==|,  |  /==/-  /\\ - \\==\\.       /|==|_  ,`-._\n")
        print_tb(width/2-33, 8, termbox.ColorWhite, termbox.ColorDefault, "/==/ - , ,|==\\ - , /==/ - |  \\==\\ _.\\=\\.-'`-.`.___.-' /==/ ,     /\n")
        print_tb(width/2-33, 9, termbox.ColorWhite, termbox.ColorDefault, "`--`-----' `--`---'`--`---'   `--`                    `--`-----`` \n")

        print_tb(width/2-8, 11, termbox.ColorWhite, termbox.ColorDefault, "LONESPACE SPACE\n")
        print_tb(width/2-26, 12, termbox.ColorWhite, termbox.ColorDefault, "Presented by Alex Belanger <i.caught.air@gmail.com>\n")
       
	//Footer 
	print_tb(width-60, height-1, termbox.ColorWhite, termbox.ColorDefault, "Visit lonesomespace.com for more information on the project")
}

func (stage *StageMenu) DrawMenu() {
	options := []string {
		"Continue game",
		"New game",
		"Settings",
		"Quit",
	}

	width,_ := termbox.Size()
	for key,val := range options {
		termbox.SetCell(width/2-31, 15+key*2, 0x2022, termbox.ColorWhite, termbox.ColorDefault)
		if key == stage.CurrentSelection {
			printf_tb(width/2-29, 15+key*2, termbox.ColorBlack, termbox.ColorWhite, " %s ", val)
		} else {
			print_tb(width/2-28, 15+key*2, termbox.ColorWhite, termbox.ColorDefault, val)
		}
	}
}

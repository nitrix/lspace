package main

import "github.com/nsf/termbox-go"

type Engine struct {
	//player
	//stage
	//inputs
	//renderer
	Running bool
	Stage   Stage
}

func (e *Engine) Run() {
	termbox.Init()
	defer termbox.Close()
	//termbox.SetCursor(0)

	e.Running = true

	for e.Running {
		e.HandleEvents();
		//e.Stage.Update(e)
		//e.Stage.Render(e)
	}
}

func (e *Engine) Stop() {
	e.Running = false
}

func (e *Engine) HandleEvents() {
	ev := termbox.PollEvent()
	switch ev.Type {
		case termbox.EventKey:

			//fmt.Printf("%d - %d!\n", ev.Ch, ev.Key)

			switch ev.Key {
				case termbox.KeyCtrlB:
					termbox.SetCell(0, 0, 27, termbox.ColorWhite, termbox.ColorRed)
					termbox.Flush()
				case termbox.KeyCtrlZ, termbox.KeyCtrlX, termbox.KeyCtrlC:
					e.Stop()
			}
			
			termbox.Clear(termbox.ColorDefault, termbox.ColorDefault)
			//draw_keyboard()
			//dispatch_press(&ev)
			//pretty_print_press(&ev)
			termbox.Flush()
		case termbox.EventResize:
			termbox.Clear(termbox.ColorDefault, termbox.ColorDefault)
			//draw_keyboard()
			//pretty_print_resize(&ev)
			termbox.Flush()
		case termbox.EventError:
			panic(ev.Err)
	}
}

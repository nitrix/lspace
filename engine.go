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
	//Prepare termbox, very clean, I love it
	termbox.Init()
	termbox.HideCursor()
	defer termbox.Close()

	e.Running = true
	for e.Running {
		e.Stage.Update()
		e.Stage.Render()
		e.Stage.HandleEvents(e);
	}
}

func (e *Engine) Stop() {
	e.Running = false
}


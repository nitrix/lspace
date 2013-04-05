package main

type Engine struct {
	//player
	//stage
	//inputs
	//renderer
	running bool
	stage   Stage
}

func (e *Engine) Run() {
	e.running = true
	//Main program loop
	for e.running {
		e.stage.Update(e)
		e.stage.Render(e)
	}
}

func (e *Engine) Stop() {
	e.running = false
}

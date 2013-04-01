package main

import "fmt"

type Engine struct {
    //player
    //stage
    //inputs
    //renderer int
    //stage
    running bool
}

func (e *Engine) Run() {
    e.running = true

    //Main program loop
    for e.running {
        fmt.Println("seems to work")
        //stage.Update()
        //stage.Render()
    }
}

func (e *Engine) Stop() {
    e.running = false
}

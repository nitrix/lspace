package main

import "fmt"

type StageGame struct {
	test int
}

func (stage StageGame) Update() {
	fmt.Println("StageGame Update()")
}

func (stage StageGame) Render() {
	fmt.Println("StageGame Render()")
}

func (stage StageGame) HandleEvents(e *Engine) {
}

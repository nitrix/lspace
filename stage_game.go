package main

import "fmt"

type StageGame struct {
	test int
}

func (stage StageGame) Update(e *Engine) {
	fmt.Println("StageGame Update()")
}

func (stage StageGame) Render(e *Engine) {
	fmt.Println("StageGame Render()")
}

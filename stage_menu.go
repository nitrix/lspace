package main

import "fmt"

type StageMenu struct {
	test int
}

func (stage StageMenu) Update(e *Engine) {
	fmt.Println("StageMenu Update()")
}

func (stage StageMenu) Render(e *Engine) {
	fmt.Println("StageMenu Render()")
}

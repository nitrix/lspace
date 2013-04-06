package main

import "fmt"

type StageMenu struct {
	currentSelection int
}

func (stage StageMenu) Update(e *Engine) {
	fmt.Println("StageMenu Update()")
}

func (stage StageMenu) Render(e *Engine) {
	fmt.Println("StageMenu Render()")

	options := []string {
		"Continue game",
		"New game",
		"Settings",
		"Quit",
	}

	for k, v := range options {
		fmt.Printf("%s - %s\n",k,v)
	}
}

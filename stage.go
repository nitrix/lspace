package main

type Stage interface {
	Update()
	Render()
	HandleEvents(e *Engine)
}

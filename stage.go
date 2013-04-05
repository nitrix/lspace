package main

type Stage interface {
	Update(e *Engine)
	Render(e *Engine)
}

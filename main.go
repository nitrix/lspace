package main

func main() {
	engine := Engine{
		Stage: &StageMenu{},
	}
	engine.Run()
}

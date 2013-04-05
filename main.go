package main

func main() {
	engine := Engine{
		stage: &StageMenu{},
	}
	engine.Run()
}

package main

func main() {
	type boolean bool
	var b boolean
	_ = (true && b)
}

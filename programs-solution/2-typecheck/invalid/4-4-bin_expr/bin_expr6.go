package main

func main() {
	var a struct {
		x int
	}
	var b struct {
		y int
	}
	_ = (a == b)
}

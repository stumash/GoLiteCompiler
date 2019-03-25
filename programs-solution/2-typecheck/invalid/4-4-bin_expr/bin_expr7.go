package main

func main() {
	var a struct {
		x int
		y int
	}
	var b struct {
		y int
		x int
	}
	_ = (a == b)
}

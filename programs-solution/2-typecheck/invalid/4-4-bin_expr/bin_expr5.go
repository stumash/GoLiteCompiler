package main

func main() {
	var a, b struct {
		x []int
	}
	_ = (a == b)
}

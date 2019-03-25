package main;

type p1 struct {
	x int
}

type p2 struct {
	x int
}

func main() {
	var x p1
	var y p2
	x = y
}

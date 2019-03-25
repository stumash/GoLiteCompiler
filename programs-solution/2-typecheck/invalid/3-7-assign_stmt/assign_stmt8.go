package main

func main() {
	type num int
	var p struct {
		x num
	}
	p.x = 5
}

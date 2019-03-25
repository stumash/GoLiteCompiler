package main

type num int

func f() num {
	var x num
	return x
}

func main() {
	var x int = f()
}

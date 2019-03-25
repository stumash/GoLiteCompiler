package main

func f() [5]int {
	var a [5]int
	return a
}

func main() {
	f()[0]++
}

package main

func main() {
	type sl []int

	var a []int
	var b sl
	b = sl(a)
}

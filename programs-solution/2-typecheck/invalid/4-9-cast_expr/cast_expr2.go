package main

func main() {
	type arr [5]int
	var a [5]int
	var b arr

	b = arr(a)
}

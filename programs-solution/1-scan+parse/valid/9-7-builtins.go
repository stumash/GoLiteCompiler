package main

func main() {
	var a string = "string"

	var b []int
	b = append(b, 0)

	var c [5]int

	println(len(a))

	println(len(b))
	println(cap(b))

	println(len(c))
	println(cap(c))
}

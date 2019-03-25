package main

func main() {
	var a string = "string"
	var b []int
	b = append(b, 0)
	var c [5]int

	var r int
	r = len(a)

	r = len(b)
	r = cap(b)

	r = len(c)
	r = cap(c)
}


package main

func main() {
	a := 1
	println(a)

	b, c := 2, 3
	println(b, c)

	b, d := 4, 5
	println(b, d)

	{
		a := 0
	}

	var e int
	var f float64

	e, f, g := 1, 2.0, 3

	h, _ := 0, 0.0
	i, _ := 0.0, 0
}

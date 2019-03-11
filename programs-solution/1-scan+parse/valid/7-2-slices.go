package main

func main() {
	// single dimension slice declaration
	var x []int
	var m, n int

	// appending
	x = append(x, 1)
	x = append(x, 2)
	x = append(x, 3)

	// slice access rhs
	var y1, y2, y3, y4, y5 int
	y1 = x[0]
	y2 = x[1]
	y3 = x[2]
	y4 = x[m]
	y5 = x[m + 1]


	// multi-dimensional slice
	var a [][]float64
	a[0] = append(a[0], 1.0)
	a[0] = append(a[0], 2.0)
	a[0] = append(a[m], 2.0)

	// multi-dimensional slice access rhs
	var b1, b2, b3 float64
	b1 = a[0][0]
	b2 = a[0][1]
	b3 = a[m][n]
} 

package main

func main() {
	// single dimension array declaration
	var x [3]int
	var m, n int

	// array access lhs
	x[0] = 1
	x[1] = 2
	x[2] = 3
	x[m] = 4
	x[m + 1] = 5

	// array access rhs
	var y1, y2, y3, y4, y5 int
	y1 = x[0]
	y2 = x[1]
	y3 = x[2]
	y4 = x[m]
	y5 = x[m + 1]

	// multi-dimensional array
	var a [1][2]float64
	a[0][0] = 0.0
	a[0][1] = 0.0
	a[m][n] = 0.0
	

	// multi-dimensional array access rhs
	var b1, b2, b3 float64
	b1 = a[0][0]
	b2 = a[0][1]
	b3 = a[m][n]
} 

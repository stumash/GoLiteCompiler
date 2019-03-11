package main

func unary_exprs() {
	var x int
	var y int
	var b bool
	var c bool
	// single
	y = -x
	y = +x
	c = !b
	y = ^x

	// multiple
	y = - -x
	y = + +x
	c = !!c
	y = ^^x
} 

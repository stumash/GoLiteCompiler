package main

func bin_exprs() {
	var x int
	var y int
	var r int

	var b bool
	var c bool

	r = x + 1
	r = x - 1
	r = x * 1
	r = x / 1
	r = x % 1
	r = x & 1
	r = x | 1
	r = x ^ 1
	r = x &^ 1
	r = x << 1
	r = x >> 1
	c = b && b
	c = b || b
	c = x < y
	c = x <= y
	c = x == y
	c = x != y
	c = x >= y
	c = x > y
	r = x + y
} 

package main

func assign_int() {
	var a, b int
	a = b

	type num int
	var c, d num
	c = d
}

func assign_rune() {
	var a, b rune
	a = b

	type r rune
	var c, d r
	c = d
}

func assign_float64() {
	var a, b float64
	a = b

	type num float64
	var c, d num
	c = d
}

func assign_bool() {
	var a, b bool
	a = b

	type boolean bool
	var c, d boolean
	c = d
}

func assign_string() {
	var a, b string
	a = b

	type s string
	var c, d s
	c = d
}

func assign_slice() {
	var a, b []int
	a = b

	type num int
	var c, d []num
	c = d

	type numi []int
	var e, f numi
	e = f
}

func assign_array() {
	var a, b [5]int
	a = b

	type num int
	var c, d [5]num
	c = d

	type numi [5]int
	var e, f numi
	e = f

	var g [5][2]int
	var h [5][2]int
	h = g
}

func assign_struct() {
	var a, b struct {
		x int
	}
	a = b

	var e struct {
		x int
		y int
	}
	var f struct {
		x, y int
	}
	f = e

	type num struct {
		x int
	}
	var c, d num
	c = d
}

func assign_multiple() {
	var a, b int
	a, b = b, a
}

func slice() []int {
	var a []int
	a = append(a, 0)
	return a
}

func assign_lvalue() {
	slice()[0] = 5
}

func assign_blank() {
	_ = 0
	_ = 0.0
	var a int = 0
	_, a = "string", 4
}

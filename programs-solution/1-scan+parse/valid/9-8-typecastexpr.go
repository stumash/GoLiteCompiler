package main

func typecast_expr() {
	var a int
	var b int

	b = int(a)
	b = (int)(a)

	type num int
	var c num

	c = num(a)
	c = (num)(a)
}

package main

type num int
var x num

func main() {
	type num float64
	var y num
	x = y
}

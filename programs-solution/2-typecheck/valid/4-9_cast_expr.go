package main

func cast_int() {
	type num int
	var x int = 5
	var y num = num(x)
	x = int(y)
}

func cast_float64() {
	type num float64
	var x float64 = 5.0
	var y num = num(x)
	x = float64(y)
}

func cast_rune() {
	type num rune
	var x rune = 'z'
	var y num = num(x)
	x = rune(y)
}

func cast_bool() {
	type num bool
	var x bool = true
	var y num = num(x)
	x = bool(y)
}

func cast_string() {
	type str string
	var x string = ""
	var y str = str(x)
	x = string(y)
}

func cast_underlying() {
	type num int
	type num2 int

	var a num
	var b num2

	a = num(b)
	b = num2(a)
}

func cast_int_string() {
	var a int
	var b string
	var c rune

	b = string(a)
	b = string(c)
}

func cast_int_float() {
	var a int
	var b float64
	var c rune

	a = int(b)
	a = int(c)
	b = float64(a)
	b = float64(c)
	c = rune(a)
	c = rune(b)

	type num int
	type flt float64
	type run rune

	var d num
	var e flt
	var f run

	d = num(e)
	d = num(f)
	e = flt(d)
	e = flt(f)
	f = run(d)
	f = run(e)
}

func main() {

}

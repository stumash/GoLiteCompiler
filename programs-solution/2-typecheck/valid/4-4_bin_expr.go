package main

func arith() {
	var (
		i int = 42
		f float64 = 3.141592
		r rune = 'V'
		s string = "golite"
	)

	// Addition
	println(i + i)
	println(f + f)
	println(r + r)
	println(s + s)

	i = i + i

	// Subtraction
	println(i - i)
	println(f - f)
	println(r - r)

	i = i - i

	// Multiplication
	println(i * i)
	println(f * f)
	println(r * r)

	i = i * i

	// Division
	println(i / i)
	println(f / f)
	println(r / r)

	i = i / i

	// Modulo
	println(i % i)
	println(r % r)

	i = i % i

	// Bit-and
	println(i & i)
	println(r & r)

	i = i & i

	// Bit-or
	println(i | i)
	println(r | r)

	i = i | i

	// Bit-xor
	println(i ^ i)
	println(r ^ r)

	i = i ^ i

	// Bit-clear
	println(i &^ i)
	println(r &^ r)

	i = i &^ i

	// Left shift
	println(i << i)
	println(r << r)

	i = i << i

	// Right shift
	println(i >> i)
	println(r >> r)

	i = i >> i
}

func logical() {
	var b bool = true

	// Logical and/or
	println(b && b)
	println(b || b)

	b = b && b
	b = b || b

	type boolean bool

	var c boolean = boolean(true)

	println(c && c)
	println(c || c)

	c = c && c
	c = c || c
}

func comparison() {
	var (
		b bool = true
		i int = 42
		f float64 = 3.141592
		r rune = 'V'
		s string = "golite"
	)

	var result bool

	// Equality
	println(b == b)
	println(i == i)
	println(f == f)
	println(r == r)
	println(s == s)

	result = s == s

	// Inequality
	println(b != b)
	println(i != i)
	println(f != f)
	println(r != r)
	println(s != s)

	result = s != s

	// Less than
	println(i < i)
	println(f < f)
	println(r < r)
	println(s < s)

	result = s < s

	// Less/equal than
	println(i <= i)
	println(f <= f)
	println(r <= r)
	println(s <= s)

	result = s <= s

	// Greater than
	println(i > i)
	println(f > f)
	println(r > r)
	println(s > s)

	result = s > s

	// Greater/equal than
	println(i >= i)
	println(f >= f)
	println(r >= r)
	println(s >= s)

	result = s >= s
}

func type_aliases() {
	type t int
	var x, y t

	// Arithmetic
	println(x + y)
	println(x - y)
	println(x * y)
	println(x / y)
	println(x % y)

	x = x + y
	x = x - y
	x = x * y
	x = x / y
	x = x % y

	// Bitwise ops
	println(x & y)
	println(x | y)
	println(x ^ y)
	println(x &^ y)
	println(x << y)
	println(x >> y)

	x = x & y
	x = x | y
	x = x ^ y
	x = x &^ y
	x = x << y
	x = x >> y

	// Comparison ops
	println(x == y)
	println(x != y)
	println(x < y)
	println(x <= y)
	println(x >= y)
	println(x > y)

	var result bool
	result = x == y
	result = x != y
	result = x < y
	result = x <= y
	result = x >= y
	result = x > y
}

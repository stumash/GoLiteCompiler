package main

func assign_stmts() {
	var a, b int
	var c [5]int
	var d struct {
		a int
	}

	// basic assignment
	a = 0
	// multiple assignment
	a, b = 0, 1

	// blank assignment
	a, _ = 0, 1

	// parenthesized assignment
	(a) = 0
	(a), (b) = 0, 1

	// op assign
	a += 1
	a -= 1
	a *= 1
	a /= 1
	a %= 1
	a &= 1
	a |= 1
	a ^= 1
	a &^= 1
	a <<= 1
	a >>= 1

	// array checks
	c[0] = 1
	c[0] += 1
	d.a = 1
	d.a += 1
}

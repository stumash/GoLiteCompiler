package main

func op_string() {
	
	var s1 string
	var s2 string

	s1 += s2

	type s string
	var s3 s
	var s4 s

	s3 += s4
}

func op_float64() {
	var f1 float64
	var f2 float64

	f1 += f2
	f1 -= f2
	f1 *= f2
	f1 /= f2

	type f float64
	var f3 f
	var f4 f

	f3 += f4
	f3 -= f4
	f3 *= f4
	f3 /= f4
}

func op_int() {
	var i1 int
	var i2 int

	i1 += i2
	i1 -= i2
	i1 *= i2
	i1 /= i2

	i1 %= i2
	i1 |= i2
	i1 &= i2
	i1 <<= i2
	i1 >>= i2
	i1 &^= i2
	i1 ^= i2

	type i int
	var i3 i
	var i4 i

	i3 += i4
	i3 -= i4
	i3 *= i4
	i3 /= i4

	i3 %= i4
	i3 |= i4
	i3 &= i4
	i3 <<= i4
	i3 >>= i4
	i3 &^= i4
	i3 ^= i4

}

func op_rune() {
	var r1 rune
	var r2 rune

	r1 += r2
	r1 -= r2
	r1 *= r2
	r1 /= r2

	r1 %= r2
	r1 |= r2
	r1 &= r2
	r1 <<= r2
	r1 >>= r2
	r1 &^= r2
	r1 ^= r2

	type r rune
	var r3 r
	var r4 r

	r3 += r4
	r3 -= r4
	r3 *= r4
	r3 /= r4

	r3 %= r4
	r3 |= r4
	r3 &= r4
	r3 <<= r4
	r3 >>= r4
	r3 &^= r4
	r3 ^= r4

}



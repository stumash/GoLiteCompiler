// Try to access fields of a variable when:
// - it is declared as a struct (s)
// - it is declared as an alias to a struct (p)
// - it is declared as an alias to an alias to a struct (q)
//
// Also try to put aliased fields inside a struct

package main

type point struct {
	x, y, z float64
}

type indirect_point point

type vector struct {
	i indirect_point
	j indirect_point
}

func main() {
	var a bool
	var b int
	var c string

	var s struct {
		a bool
		b int
		c string
	}
	s.a = true
	s.b = 42
	s.c = "struct"

	a = s.a
	b = s.b
	c = s.c

	var x, y, z float64

	var p point
	p.x = 0.0
	p.y = -1.0
	p.z = 1.0

	x = p.x
	y = p.y
	x = p.z

	var q indirect_point
	q.x = 0.0
	q.y = -1.0
	q.z = 1.0

	x = q.x
	y = q.y
	z = q.z

	var v vector
	v.i = q
	v.j = q

	v.i.x = 2.0
	v.i.y = 3.0
	v.i.z = 4.0

	x = v.i.x
	y = v.i.y
	z = v.i.z
}

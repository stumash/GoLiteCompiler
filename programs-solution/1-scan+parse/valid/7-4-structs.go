package main

func main() {
	// simple struct
	var p struct {
		x, y, z int
	}

	// struct access lhs
	p.x = 1
	p.y = 2
	p.z = 3

	// struct access rhs
	var x1, y1, z1 int
	x1 = p.x
	y1 = p.y
	z1 = p.z

	// nested stuct
	var q struct {
		n struct {
			x, y, z int
		}
	}

	// nested struct access lhs
	q.n.x = 1
	q.n.y = 2
	q.n.z = 3

	// nested struct access rhs
	var x2, y2, z2 int
	x2 = q.n.x
	y2 = q.n.y
	z2 = q.n.z

	var t [3]struct {
		x, y, z int
	}

	t[0].x = 1
	t[0].y = 2
	t[0].z = 3
} 

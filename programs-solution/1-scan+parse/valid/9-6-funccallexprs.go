package main

func f() {
}

func g(l, a, b, c int) {

}

func h() []int {
	var a []int
	a = append(a, 0)
	return a
}

func funccall_exprs() {
	var a, b, c int
	var d [1]int

	f()
	g(0, a, b+c, d[0])

	(f)()
	(g)(0, a, b+c, d[0])

	h()[0] = 0
	a = h()[0]
}

package main

func main() {
	type slice []int
	type indirect_slice slice

	type array [5]int
	type indirect_array array

	type elem int
	type index int

	var ws indirect_slice
	var xs slice
	var ys []int
	var zs []elem

	var wa indirect_array
	var xa array
	var ya [5]int
	var za [5]elem

	var a int
	var e elem
	var i index

	a = ws[0]
	a = xs[0]
	a = ys[0]
	e = zs[0]

	a = ws[i]
	a = xs[i]
	a = ys[i]
	e = zs[i]

	a = wa[0]
	a = xa[0]
	a = ya[0]
	e = za[0]

	a = wa[i]
	a = xa[i]
	a = ya[i]
	e = za[i]


	var wsm []indirect_slice
	var xsm []slice
	var ysm [][]int
	var zsm [][]elem

	var wam []indirect_array
	var xam []array
	var yam [5][5]int
	var zam [5][5]elem

	var ism indirect_slice
	var dsm slice
	var asm []int
	var esm []elem

	var iam indirect_array
	var dam array
	var aam [5]int
	var eam [5]elem

	ism = wsm[0]
	dsm = xsm[0]
	asm = ysm[0]
	esm = zsm[0]

	a = wsm[0][0]
	a = xsm[0][0]
	a = ysm[0][0]
	e = zsm[0][0]

	iam = wam[0]
	dam = xam[0]
	aam = yam[0]
	eam = zam[0]

	a = wam[0][0]
	a = xam[0][0]
	a = yam[0][0]
	e = zam[0][0]
}

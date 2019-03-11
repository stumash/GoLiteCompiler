package main

func dec_stmts() {
	// simple variable declarations
	var a1 int
	var a2 float64 = 0.0
	var a3 = 0.0

	// multiple variable declarations
	var a4, a5 string
	var a6, a7 string = "a", `b`
	var a8, a9 = 'a', 0

	// blank identifier declaration
	var _ int

	// distributive variable declarations
	var (
		b1 int
		b2 float64 = 0.0
		b3 = 0.0

		b4, b5 string
		b6, b7 string = "a", `b`
		b8, b9 = 'a', 0

		_ int
	)

	// empty variable declaration
	var ( )

	// type alias
	type t1 int

	// distributive type aliases
	type (
		t2 rune
		t3 bool
	)

	// empty type aliases
	type ( )

	// struct type
	type t4 struct {
		x float64
		y, z float64
	}

	// nested struct
	type t5 struct {
		k float64
		n struct {
			a int
		}
	}

	// empty struct
	type t6 struct {
	}

	// slices
	type t7 []int			// base type
	type t8 []t1			// type id
	type t9 []struct {		// composite type
		a int
	}
	type t10 [][]float64		// matrix

	// arrays
	type t11 [4]int			// base type
	type t12 [4]t1			// type id
	type t13 [8]struct {		// composite type
		a int
	}
	type t14 [3][3]float64	// matrix
} 

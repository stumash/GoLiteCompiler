package main

// type alias
type t1 int

// distributive type aliases
type (
	t2 rune
	t3 bool
)

// parenthesized types
type t4 (int)

// empty type aliases
type ( )

// struct type
type t5 struct {
	x float64
	y, z float64
}

// nested struct
type t6 struct {
	k float64
	n struct {
		a int
	}
}

// empty struct
type t7 struct {
}

// slices
type t8 []int			// base type
type t9 []t1			// type id
type t10 []struct {		// composite type
	a int
}
type t11 [][]float64		// matrix

// arrays
type t12 [4]int			// base type
type t13 [4]t1			// type id
type t14 [8]struct {		// composite type
	a int
}
type t15 [3][3]float64	// matrix 

package main

func switch_stmts() {
	var x int

	// no expression
	switch {
		case x < 0:
		case x > 0:
		default:
	}

	// expression, no default
	switch x {
		case 0:
		case 1, 3, 5, 7, 9:
	}

	// statement, no expression,
	switch x++; {
		case x < 0:
		case x > 0:
		default:
	}

	// default in the middle
	switch x++; x {
		case 0:
		default:
		case 1, 3, 5, 7, 9:
	}

	// empty
	switch {

	}
} 

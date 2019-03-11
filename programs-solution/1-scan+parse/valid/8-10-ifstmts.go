
package main

func if_stmts() {
	var x int

	// if/then
	if true {
	}

	// if/then with empty init statement
	if ; true {
	}

	// if/then with init statement
	if x := 0; x == 0 {
	}

	// if/then/else
	if false {
	} else {
	}

	// if/then/else with empty init statement
	if ; true {
	} else {
	}

	// if/then/else with init statement
	if x++; true {
	} else {
	}

	// if/else-if/else
	if false {
	} else if false {
	} else {
	}

	// if/else-if/else with init statement
	if false {
	} else if x++; false {
	} else {
	}
} 

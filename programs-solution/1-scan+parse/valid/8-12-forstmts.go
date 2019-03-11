package main

func for_stmts() {
	var a int

	// Infinite loop
	for {
	}

	// "while" loop
	for a < 10 {
	}

	// three-part loop, all parts missing
	for ;; {
	}

	// three-part loop, init statement only
	for a := 0; ; {
	}

	// three-part loop, expression only
	for ; a < 10; {
	}

	// three-part loop, update statement only
	for ; ; a++ {
	}

	// three-part loop, init statement and expression
	for a := 0; a < 10; {
	}

	// three-part loop, init and update statements
	for a := 10; ; a++ {
	}

	// three-part loop, expr and update statements
	for ; a < 10; a++ {
	}

	// three-part loop, all parts
	for a := 0; a < 10; a++ {
	}
}
 

package main

func return_stmts_lit() int {
	// return literal
	return 17
}

func return_stmts_empty() {
	// empty return
	return
}

func return_stmts_expr() int {
	// return expr
	return return_stmts_lit()
} 

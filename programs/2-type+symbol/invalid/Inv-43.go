package main

//Invalid 
//Checks if the expressions of the LHS are all lvalues


func f () {
	var a1, a2 int;

	a1, a2, a1 + a2 = 3, 4, 5
}
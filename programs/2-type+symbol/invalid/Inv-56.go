package main 

//InValid
//The for expression is checked if its component expressions are of same type

func f () {
	type b1 bool

	type b2 b1

	var a b2
	var b b1

	for a || b {
		
	}
}
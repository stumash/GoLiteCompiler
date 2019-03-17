package main 

//Valid
//The for expression is checked if its resolved type is bool 

func f () {
	type b1 bool

	type b2 b1

	var a b2

	for a {
		
	}
}
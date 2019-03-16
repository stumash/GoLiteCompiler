package main 

//Invalid
//The bool 

func f () {
	type b1 bool
	type b2 bool

	var a1, a2 b1
	var c1, c2 b2

	for (a1 && a2) || (c1 && c2) {

	}
}
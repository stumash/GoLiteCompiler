package main 

//InValid
//The for expression is checked if its component expressions are of same type
//Followed by checking if the final exp has resolved type bool 

func f () {
	type b1 bool

	type b2 b1

	var a1, a2 b2
	var c1, c2 b2

	for (a1 != a2) || (c1 == c2) {
		
	}
}
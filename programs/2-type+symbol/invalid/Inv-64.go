package main 

//Invalid
//Types of case exp are not resolved to base type to check if it is bool type when swithc exp does not exist

func f () {
	
	type bo bool
	var b bo 
	
	switch { 
	case true : { }
	case b : { }
	}
}
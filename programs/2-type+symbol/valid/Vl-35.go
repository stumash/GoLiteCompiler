package main 

//SUPER WEIRD

func f () {
	type ns string 
	type str struct {
		a, b ns 
	}
	var x str
	var y ns 
	switch x.a {
	case y : { }
		default : 
	}
}
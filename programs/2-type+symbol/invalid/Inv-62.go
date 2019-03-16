package main 

//Invalid
//Checks for invalidity of case exp with that of switch exp type
//Note that it only checks eqaulity of types not condition the swith exp type to only certain types

func f () {
	type x struct {
		a,b int 
	}

	var c x

	switch c {
	case 1 : {}
		default : {}
	}
}
package main 

//Invalid
//Checks if the cases are on the same type as that of the switch expression 

func f () {
	switch a:=0; a {
	case 0 : { }
	case 'x': { }
	default :
	}
}
package main 

//Invalid 
// Chekcs if every case inside switch has a return of type func f return type as f itself does not have a return statement

func f () int {
	var a int = 10;

	switch a {
	case 1 : { }
	default : { }
	}
}
package main 

//Invalid
//Checks if the typecheker does resolve types to base and checks or not ? (it does not )

func f () {
	type num int 
	var a num 

	switch a { 
	case 1: {}
	case 2 : {}
	default : {}
	}
}
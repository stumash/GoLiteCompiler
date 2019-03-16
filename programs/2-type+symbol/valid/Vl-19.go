package main 

//Valid 
//Does a return check on for, if else clauses and switch 

func f () int {
	for 3 > 5 {
		return 1
	}

	if true {
		return 2
	} else {
		return 3
	} 

	switch 4   {
	case 4 : return 4
	default : return 5
	}
}
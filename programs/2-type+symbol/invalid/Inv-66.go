package main 

//Invalid
//Case exp are not resolved and checked but checked as is

func f () {
	type ns string  
	type n ns 
	var y n
	var x ns
	switch x {
	case n : 
		default :
	}
}
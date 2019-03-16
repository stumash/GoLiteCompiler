package main 

//Invalid 
//The case expressions have to match the defined type of the switch expression

func f () {
	type num int 
	var x num
	switch x  {
	case 10 : { }
	}
}
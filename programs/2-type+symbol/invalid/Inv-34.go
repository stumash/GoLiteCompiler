package main 

//Invalid 
//Check if the return type is of same type and not of any resolved type 

type num int 
func f ( ) int {
	var a num = num(1)

	return a 
}
package main 

//Invalid
//Check if the identifier is already used in the current scope,  the type declaration should raise an error

func main () {
	var num int 
	type num int
}
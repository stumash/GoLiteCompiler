package main 

//valid 
// Declaring a varibale of same name in anohter scope shadows the previous declaration in that same scope itself 
var a int 

func main () {
	var a int 
	{
		var a int 
	}
}
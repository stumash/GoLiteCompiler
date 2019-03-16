package main 

//Invalid 
//Recursive call would not work as the function name is shadowed in the scope inside the function declaration itself 

func f ( a int ) {
	var f int 
	f ( a)
}
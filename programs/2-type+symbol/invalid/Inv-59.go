package main 

//Invalid 
//The for which shadows previous a is in a new scope entirely 
//Checks the assigment later with the declaration withing the function scope and not that of for 


func f() {
	var a string 
	
	for a := 0; true ; a ++ {

	}

	a = 100

}
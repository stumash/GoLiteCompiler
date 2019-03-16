package main

//Valid 
//Check if typecast of defined type to base type for function call passes arg to func successfully 

func f(x int ) {
}


func main() {
	type num int
	var a num 
	f((int)(a))
}
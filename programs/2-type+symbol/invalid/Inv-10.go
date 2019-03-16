package main

//Invaliud 
//Checks if assigment calls for implicit type cast to base type when passed that of defined type resolvable to that base type

func main() {
	type num int 
	var a int  = (num)(3)
}
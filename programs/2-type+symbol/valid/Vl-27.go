package main 

//Valid
//Trries to resolve function return type to base type 
type num int 
type num1 num

func f1 ( ) num1 { 
	return num1(1)
}

func f () {
	print (f1())
}
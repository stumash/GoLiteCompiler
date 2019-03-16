package main

//Invalid
//Checks the validity of assigment of one defined type var to anohter whcih are resolvable to same base type 

func main() {
	type num int 
	type num1 num
	var a num
	var b num1 
	a = b
}

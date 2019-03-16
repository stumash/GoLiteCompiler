package main 

//Valid 
//Checks the Resolved type of expression to valiadte increament and decreamenet operations 

func f () {
	type num int 
	type num2 num

	var a0 int 
	var a1 num
	var a2 num2

	a0++
	a1++
	a2++

}
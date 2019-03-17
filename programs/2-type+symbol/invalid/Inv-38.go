package main 

//Invalid
//Checks for incompatible defined type assigment even though its resolved type is base type

func f () {
	type num int 

	var a1, a2 num
	var a3, a4 int 

	a1, a2, a3, a4, a5 := 1, 2, 3, 4, 5;
}
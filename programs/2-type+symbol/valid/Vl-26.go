package main

//Valid
//Tries to resolve all types of exxpressions within print to base type 

func f () {
	type num int 
	type fl float64
	type s string 
	type b bool

	var a1 num
	var a2 fl
	var a3 s
	var a4 b

	print (a1, a2, a3, a4)
}
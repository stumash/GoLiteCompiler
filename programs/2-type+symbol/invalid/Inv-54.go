package main

//Invalid
//Checks if expressions for print resolve to base type (checks slice)

func f () {
	var a1 int 
	var a2 []int 

	print (a1, a2)
}
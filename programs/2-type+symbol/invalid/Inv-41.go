package main 

//Invalid
//Checks whther there is implicit type case of length return to defined type nnum for this to work

func f () {
	type num int 
	var a1 num;
	 
	var arr [10]int

	a1, a2 := len(arr), len(arr)
}
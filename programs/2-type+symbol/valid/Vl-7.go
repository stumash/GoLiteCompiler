package main

//Valid 
//Checks if the  defined type num is not checked when shadowing it in anoohter scope aas a child of main scope

type num int 

func main() {

	
	var num num
}
package main

//Invalud 
//Checks if the scope of an identifier is checked properly when it is being redefined in same scope



func main() {
   type num int 
	var a num
	var num num
}
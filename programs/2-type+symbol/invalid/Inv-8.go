package main

//Invalid
// Checks if redefinition of type num is allowed or not 


func main() {
   type num int 
	var a num
	type num float64
	var b num
}
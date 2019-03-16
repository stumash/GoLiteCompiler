package main

//Valid 
//Checks to see if binary/unary expressions resolve to defined type if its base type is valid for binary/ unary op

// Weird case 
func main() {
		
	type num int
	
	var b, c num;
	var g num
	g = +(b * c)
	g = -g
}

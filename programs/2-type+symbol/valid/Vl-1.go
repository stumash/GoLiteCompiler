package main

//Valid
//Case to check if explicit typecasting values of base type to defined type is allowed 

// Weird case 
func main() {
		
	type num int
	
	var b, c num;
	var g num
	b = num(1)
	c = num(2)
}

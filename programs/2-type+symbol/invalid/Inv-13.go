package main

//invalid
//Cheks if 'int' is actually the shadowed variable name 'int' while checking the validity of 'int' as a type in func main  

var int float64

func main() {
	var a int 	
}


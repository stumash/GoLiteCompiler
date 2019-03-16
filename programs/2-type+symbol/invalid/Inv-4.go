package main


//Invalid 
//A check to see whehter scope is resolved for defined types when passing arguments to func  

type num int
func f(x num ) {
}


func main() {
	
	var a int 
	f(a)
}
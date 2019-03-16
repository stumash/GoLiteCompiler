package main

//Valid 
//Check to see if lieteral struct types are compatible in function calls 

func s (p struct {
		x, y int 
		} ) {

}


func main() {
    var a struct {
		x, y int
	}
	s(a)
}
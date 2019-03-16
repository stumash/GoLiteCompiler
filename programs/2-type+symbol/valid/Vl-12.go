package main

//Valid 
//Another complicated check of equality of struct literal and a variable  of struct literal

type gg struct {
			a, b int 
			}
func s (p struct {
		x, y gg
		} ) {

}


func main() {
    var a struct {
		x, y gg
	}
	s(a)
}
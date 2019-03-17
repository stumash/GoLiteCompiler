package main 

//Invalid
//Checks if fucntion returns are lvalues (they are not )

func f1 () [5]int {
	var a [5]int
	return a
}

func f () {
	var a1, a2, a3 int 
	a1, a2, f1()[3] = 2, 3, a3
}
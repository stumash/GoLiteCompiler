package main 

//Valid
//Function returns an int type value

func f1 () int {
	return 2
}

func f () {
	var a1, a2 int 
	a1, a2 = 2, f1()
}
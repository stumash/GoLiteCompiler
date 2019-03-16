package main 

//Valid
//Checks the validity of op= operations 

func f () {
	var a1 int 
	var a2 float64
	var a3 rune
	a1 += 10
	a2 += 5.5
	a3 += 'a'
	a3 -= 'a'
	a2 -= 4.5
	a1 -= 5
}
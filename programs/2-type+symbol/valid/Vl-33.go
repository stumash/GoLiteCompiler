package main 

//Valid
//The block of if can shadow the init variables 

func f () {
	if a:=0; a < 10 {
		a := "gg"
	} else {

	}
}
package main 

//Valid Weird case
//Allows init defined variables to b shadowed insdie new scope

func f () {
	for a := 0; a < 10; a ++ {
		a := 1000
		print(a)
	}
}
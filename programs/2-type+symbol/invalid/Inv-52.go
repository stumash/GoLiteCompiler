package main

//Invalid
//CHecks if expressions inside expression list of print resolve to base type (here strcut is put )

func f () {
	var a1 int 

	type x struct {
		a, b int 
	}

	var a2 x;

	print(a1, a2)
}
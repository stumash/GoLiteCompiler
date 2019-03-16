package main 

//valid 
//Checks if every case expression is of type bool if the swithc has no expresison

func f () {
	var a bool
	type bo bool
	var a1, a2 bo
	switch {
	case true: { }
	case false : { }
	case a1 == a2 : { }
	case a1 != a2 : { }
	}
}
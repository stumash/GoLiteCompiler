package main 

//Invalid special
//Checks if Blank identifiers can take in void return types (it does not )

func f1 (a int ) int {
	return 2
}

func f2 () {

}

func f () {
	a1, a2, _ := f1(1), 2, f2()
}
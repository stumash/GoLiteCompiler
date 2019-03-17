package main

//Invalid SPecial case check
//Checks for any void types in RHS, if so it is incompatible

func f1 () {

}

func f2 () {
	a1, a2 := f1(), f1()
}
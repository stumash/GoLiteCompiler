package main

//Invalid
//Checks if func return type resolves to base type (here it is void type)

func f1 () {

}

func f () {
	print (f1())
}
package main 

//Valid
//Checking if the for statement init, exp and post type check
// init and post are the collection of simple statements (statement_ in our parser)

func f1 () int {
	return 1
} 

func f () {
	for f1(); true ; f1() {

	}
}
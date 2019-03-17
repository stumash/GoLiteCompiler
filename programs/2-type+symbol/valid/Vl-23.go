package main 

//Valid
//Checks how append instruction returns slice type is given the samee type to LHS variables in Short Declarations 

func f () {
	var a1 []int

	a1 = append(a1, 2)

	a2, _ := append(a1, 1), append (a1, 3)
}
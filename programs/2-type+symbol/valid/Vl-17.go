package main 

//Valid 
//Interesting case where we know it will pass but does it check the retunr inside the if and else clauses??

func f () int {
	for {
		if true {
			return 0
		}else {
			return 1
		}
		return 2;

	}
}
package main

//Valid
//Checks if return is checked outside for within func f. (it is not cause it is assumed for would loop and retunr ) 

// return invalid 
func main() {
		
	
}

func f () int {
	for {
		return 3
	}
}
 

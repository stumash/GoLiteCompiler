package main

//Invalid 
//To check if a variable of defined type can be assigned to another variable of anohter defined type which are boht resolvalble to same struct 


func main() {
	type simple struct {
		a, b int 
		}
	type simple_alt simple
	var x simple 
	var y simple_alt
	
	x = y
}

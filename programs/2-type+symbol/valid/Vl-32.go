package main 

//Valid
//The inner scope of for can access the init variables that aslo can be shawdoed inside 

func f () {
	for a:= 0; a < 10; a ++ {
		print(a)
	}
}
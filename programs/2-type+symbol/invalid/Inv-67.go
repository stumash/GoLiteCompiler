package main 

//InValid 
//Each case clause opens a new scope 

func f () {
	var a int ;

	switch  a {
	case 2: {
		var b int; 
		var c int ;
	}
	case 3 : {
		b = 10000;
	}
	default : 
	}
}
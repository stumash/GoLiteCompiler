//To check all types 
package main 

func main () {
	var a int ;
	
	type num int 
	var b num

	var d []int 
	var e [100]int 

	e[99] = 100;
	d = append(d, 1)

	type sample  struct  {
		a, b int ;
		x, y rune;
	}

	var x1 sample
	var x2 sample 

	x1 = x2;

	var f struct {a, b int;}
	var g struct {a, b int ;}

	f = g;

	var h struct {_, b int ;}
	var i struct {_, b int ;}

	h = i;


}
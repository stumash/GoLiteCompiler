//To check werid cases for functions 

package main 


func fs(a struct {a, b int ; }) {

}

func fa(a [10]int ) {
	a[5] = 1000; 
}

func fsl(a []int) {
	a[2] = 100004;
}

func main () {
	var x1 struct {a, b int ;}
	fs(x1);

	var x2 [10]int;
	x2[5] = 10;
	println(x2[5]); //~10
	fa(x2);
	println(x2[5]); //~10

	var x3 []int;
	x3 = append(x3, 1);
	x3 = append(x3, 2);
	x3 = append(x3, 3);
	println(x3[2]); //~3

	fsl(x3);
	println(x3[2]); //~100004
}

package main 

//invalid
//The post part of for expression does not typecheck

func f () {
	var a1 bool
	var a2 bool

	for  i := 0; a1 && a2; a1 ++ {

	}
}
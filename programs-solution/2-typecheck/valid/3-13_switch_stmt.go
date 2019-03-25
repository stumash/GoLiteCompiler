package main

func main() {
	switch x := 0; x {
		case 1:
			x++
		case 2,3:
			x++
		default:
			x++
	}

	switch x := 0; x {
		case 1:
			var x int
		case 2,3:
			var x int
		default:
			var x int
	}

	switch {
		case true:
		case false:
	}

	type num int
	switch x := num(0); x {
		case num(1):
		case num(2),num(3):
	}
}

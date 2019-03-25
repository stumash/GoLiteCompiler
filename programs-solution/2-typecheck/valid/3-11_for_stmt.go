package main

func main() {
	type t bool
	var my_bool bool
	var my_t t

	for {
		break
	}

	for my_bool {
		break
	}

	for my_t {
		break
	}

	for ; my_bool; {
		break
	}

	for ; my_t; {
		break
	}

	for x := 0; ; {
		x++
	}

	for x := 0; ; {
		var x int
	}
}

package main

func main() {
	type s struct {
		a int
	}

	var a struct {
		a int
	}
	var b s

	b = s(a)
}

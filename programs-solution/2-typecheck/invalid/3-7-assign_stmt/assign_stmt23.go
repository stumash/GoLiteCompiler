package main

func f() [10]int {
	var a [10]int
	return a
}

func main() {
    f()[0] = 1
} 

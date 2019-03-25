package main

// Recursive function
func fib(n int) int {
	if n < 2 {
		return n
	} else {
		return fib(n-1) + fib(n-2)
	}
}

func id(x int) int {
	return x
}

type num int

func f(a num) {

}

func main() {
	println((id)(3))
	f(num(5))
}

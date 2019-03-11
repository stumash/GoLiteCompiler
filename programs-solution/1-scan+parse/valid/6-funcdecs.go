package main

// no params, no return value
func f1() {

}

// no params, one return value
func f2() int {
	return 0
}

// one param, no return value
func f3(a int) {

}

// two params (long form), no return value
func f4(a int, b int) {

}

// two params (short form), no return value
func f5(a, b int) {

}

// two sets of short form params
func f6(a, b int, c, d bool) {

}

// parenthesized types
func f7(a (int)) {

}

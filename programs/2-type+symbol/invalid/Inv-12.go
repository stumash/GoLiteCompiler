package main

//Invalid
//Checks if defined types which are different but resolved to same base type can be compatible to arithmetic operations 

func main() {
    type num int
    var a num
    type natural num
    var b natural

    a = a + b
}
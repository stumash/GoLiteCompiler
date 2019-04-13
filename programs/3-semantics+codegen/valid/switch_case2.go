package main

func main() {
    var n int = 5

    switch n++; {
    case n == 8:
        println("yes")
    case n == 7:
        println("no")
    default:
        println("default") //~default
    }
}

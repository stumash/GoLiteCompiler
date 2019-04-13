package main

func main() {
    var n int = 5

    switch n++; {
    case n == 6:
        println("yes") //~yes
    case n == 7:
        println("no")
    default:
        println("default")
    }
}

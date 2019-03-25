package main

func main() {
    type p1 struct {
        x int
    }
    type p2 struct {
        x int
    }
    var x p1
    var y p2 = x
}

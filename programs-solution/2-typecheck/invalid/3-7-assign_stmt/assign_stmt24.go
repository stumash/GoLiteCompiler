package main

func f() struct { a int; } {
    var a struct { a int; }
    return a
}

func main() {
    f().a = 1
}

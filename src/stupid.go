package main

var a struct{
    x, y int
    a struct{
        x, y int
    }
}

var b = a.x
var c = a.a.x

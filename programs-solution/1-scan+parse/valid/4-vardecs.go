package main

// simple variable declarations
var a1 int
var a2 float64 = 0.0
var a3 = 0.0

// multiple variable declarations
var a4, a5 string
var a6, a7 string = "a", `b`
var a8, a9 = 'a', 0

// parenthesized type
var a10 (string)
var a11 (string) = "a"

// blank identifier declaration
var _ int

// distributive variable declarations
var (
	b1 int
	b2 float64 = 0.0
	b3 = 0.0

	b4, b5 string
	b6, b7 string = "a", `b`
	b8, b9 = 'a', 0

	_ int
)

// empty variable declaration
var ( ) 

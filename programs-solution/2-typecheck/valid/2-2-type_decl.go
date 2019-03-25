package main

type a struct {
	a int
	int int
	b int
}

type rec1 struct {
	field []rec1
}

type rec2 [][3]rec2

type rec3 [3][]rec3

type rec4 struct{
	field struct {
		field []rec4
	}
}

type _ int

type _ struct {
	_ int
}

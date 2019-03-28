#include<stdio.h>

void main () {
    struct {int a; int b; } X;
    struct {int a; int b; } Y;

    X = Y;
}
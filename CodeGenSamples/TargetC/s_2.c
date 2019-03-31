#include<stdio.h>

struct { int x; int y; } func1 (struct { int x; int y; } a) {
    a.x = 10;
    a.y = 100;

    return a;
}

void main () {
    struct { int x; int y; }a, b;    

    a.x = 10;
    a.y= 10;

    b.x = 20;
    b.y = 30;
    printf("%d %d \n", b.x, b.y);

}
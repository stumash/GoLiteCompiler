#include<stdio.h>

int func(int a[6]) {
    a[0] = 2;
    return a[0];
}

void main () {
    int a[6] = {1, 2, 3, 4, 5, 6};

    printf("%d\n", a[0]);
    func(a);
    printf("%d\n", a[0]);
}

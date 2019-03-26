#include<stdio.h> 

typedef struct X {
    int a;
    int b;
}X;

void func(struct {int a; int b;} g) {

}

int main() {
    X a;

    func(a);
}
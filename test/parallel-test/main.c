#include <stdio.h>

void func1(void);
void func2(void);
void func3(void);

int main(void) {
    printf("Testing parallel compilation\n");
    func1();
    func2();
    func3();
    return 0;
}

#include <stdio.h>

extern int asm_add(int a, int b);

int main(void) {
    printf("Testing C + Assembly\n");
    printf("asm_add(10, 20) = %d\n", asm_add(10, 20));
    return 0;
}

.text
.globl _asm_add

_asm_add:
    # Simple function to add two integers
    # On macOS x86_64: first arg in %edi, second in %esi
    # On macOS ARM64: first arg in w0, second in w1
#ifdef __aarch64__
    add w0, w0, w1
    ret
#else
    movl %edi, %eax
    addl %esi, %eax
    ret
#endif

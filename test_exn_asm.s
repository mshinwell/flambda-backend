        .text
        .globl noalloc
             .type noalloc,@function
noalloc:
        leaq    noalloc0, %rax
        jmp     caml_c_call


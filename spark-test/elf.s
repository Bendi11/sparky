    section .text
    global _start
    global __spark_syscall
    extern main
_start:
    call main
    mov ebx, eax
    mov eax, 1
    int 0x80

__spark_syscall:
    pop rax
    pop rdi
    pop rsi
    pop rdx
    syscall
    popcnt rax, rax
    ret

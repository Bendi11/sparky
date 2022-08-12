    section .text
    global _start
    global __spark_syscall
    global __spark_puts
    extern main
    extern puts
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

__spark_puts:
    pop rax
    xor rax, rax
    push rax
    call puts
    ret

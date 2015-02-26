
%macro	movmem 2
mov    r14, %2
mov    %1, r14
%endmacro

%macro	movshl 2
mov    %1, %2
shl    %1, 1
%endmacro

%macro	movshr 2
mov    %1, %2
shr    %1, 1
%endmacro

%macro	savetup 1
mov    %1, r15
shl    %1, 1
add    %1, 1
%endmacro

section .text

global	main
extern	disp, make_tuple, dispi, heap_init, gc_check, equiv

main:
mov	rbp, rsp
call	heap_init
mov	rdi, 12
call	gc_check
sub	rsp, 40
mov	rdi, 6
mov	rsi, 2
mov	rdx, 4
mov	rcx, 6
xor	rax, rax
call	make_tuple
mov	[rbp - 16], rax
mov	rdi, 6
mov	rsi, 2
mov	rdx, 4
mov	rcx, 6
xor	rax, rax
call	make_tuple
mov	[rbp - 24], rax
mov	rcx, [rbp - 16]
mov	rbx, [rbp - 24]
push	rbx
push	rcx
mov	rdi, rbx
mov	rsi, rcx
call	equiv
pop	rcx
pop	rbx
push	rax
push	rbx
push	rcx
mov	rdi, rax
call	dispi
mov	rdx, rax
pop	rcx
pop	rbx
pop	rax
jmp	_ml_term

_ml_term:
mov	rsp, rbp
mov	rax, 0
ret

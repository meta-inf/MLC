
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
mov	rdi, 10
call	gc_check
sub	rsp, 40
mov	rdi, 2
mov	rsi, 0
xor	rax, rax
call	make_tuple
mov	[rbp - 16], rax
mov	rdi, 2
mov	rsi, 0
xor	rax, rax
call	make_tuple
mov	[rbp - 24], rax
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 24]
movshl	rcx, fn24
xor	rax, rax
call	make_tuple
mov	[rbp - 32], rax
movmem	[rbp - 8], [rbp - 16]
mov	QWORD [rbp - 16], 20
movmem	[rbp - 24], [rbp - 32]
mov	rsp, rbp
jmp	fn16

_ml_term:
mov	rsp, rbp
mov	rax, 0
ret

fn24:
sub	rsp, 24
movshr	r15, QWORD [rbp - 8]
sub	rsp, 0
mov	rax, [rbp - 16]
push	rax
mov	rdi, rax
call	dispi
mov	rbx, rax
pop	rax
jmp	_ml_term

fn16:
sub	rsp, 24
mov	rdi, 12
call	gc_check
movshr	r15, QWORD [rbp - 8]
sub	rsp, 32
mov	rbx, [rbp - 16]
push	rbx
mov	rdi, rbx
mov	rsi, 0
call	equiv
pop	rbx
cmp	rax, 0
jz	cf1
mov	rcx, [rbp - 24]
mov	QWORD [rbp - 16], 2
shr	rcx, 1
movmem	[rbp - 8], [rcx + 8]
movshr	r14, [rcx + 16]
mov	rsp, rbp
jmp	r14
cf1:
mov	[rbp - 16], rbx
mov	[rbp - 32], rax
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 16]
mov	rcx, [rbp - 24]
xor	rax, rax
call	make_tuple
mov	[rbp - 40], rax
mov	rbx, [rbp - 16]
mov	rax, rbx
sub	rax, 2
mov	[rbp - 16], rbx
mov	[rbp - 48], rax
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 40]
movshl	rcx, fn32
xor	rax, rax
call	make_tuple
mov	[rbp - 56], rax
movmem	[rbp - 16], [rbp - 48]
movmem	[rbp - 24], [rbp - 56]
savetup	QWORD [rbp - 8]
mov	rsp, rbp
jmp	fn16

fn32:
sub	rsp, 24
movshr	r15, QWORD [rbp - 8]
sub	rsp, 0
mov	rcx, [r15 + 8]
mov	rbx, [rbp - 16]
mov	rax, rbx
imul	rax, rcx
shr	rax, 1
mov	rdx, [r15 + 16]
mov	[rbp - 16], rax
shr	rdx, 1
movmem	[rbp - 8], [rdx + 8]
movshr	r14, [rdx + 16]
mov	rsp, rbp
jmp	r14

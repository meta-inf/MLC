
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
mov	rdi, 20
call	gc_check
sub	rsp, 56
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
movshl	rcx, fn23
xor	rax, rax
call	make_tuple
mov	[rbp - 40], rax
mov	rdi, 4
mov	rsi, 0
mov	rdx, [rbp - 40]
xor	rax, rax
call	make_tuple
mov	[rbp - 32], rax
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 32]
movshl	rcx, fn26
xor	rax, rax
call	make_tuple
mov	[rbp - 48], rax
movmem	[rbp - 8], [rbp - 16]
mov	QWORD [rbp - 16], 20000000
movmem	[rbp - 24], [rbp - 48]
mov	rsp, rbp
jmp	fn15

_ml_term:
mov	rsp, rbp
mov	rax, 0
ret

fn26:
sub	rsp, 24
movshr	r15, QWORD [rbp - 8]
sub	rsp, 0
mov	rax, [rbp - 16]
mov	QWORD [rbp - 16], 2
movmem	[rbp - 24], [r15 + 8]
shr	rax, 1
movmem	[rbp - 8], [rax + 8]
movshr	r14, [rax + 16]
mov	rsp, rbp
jmp	r14

fn23:
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

fn15:
sub	rsp, 24
mov	rdi, 18
call	gc_check
movshr	r15, QWORD [rbp - 8]
sub	rsp, 32
mov	rdi, 6
mov	rsi, 0
savetup	rdx
movshl	rcx, fn15
xor	rax, rax
call	make_tuple
mov	[rbp - 40], rax
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 16]
mov	rcx, [rbp - 40]
xor	rax, rax
call	make_tuple
mov	[rbp - 32], rax
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 32]
movshl	rcx, fn31
xor	rax, rax
call	make_tuple
mov	[rbp - 48], rax
mov	rax, [rbp - 24]
movmem	[rbp - 16], [rbp - 48]
shr	rax, 1
movmem	[rbp - 8], [rax + 8]
movshr	r14, [rax + 16]
mov	rsp, rbp
jmp	r14

fn31:
sub	rsp, 24
mov	rdi, 24
call	gc_check
movshr	r15, QWORD [rbp - 8]
sub	rsp, 48
mov	rbx, [r15 + 8]
cmp	rbx, 0
setz	al
movzx	rax, al
cmp	rax, 0
jz	cf1
mov	rcx, [rbp - 24]
shr	rcx, 1
movmem	[rbp - 8], [rcx + 8]
movshr	r14, [rcx + 16]
mov	rsp, rbp
jmp	r14
cf1:
mov	[r15 + 8], rbx
mov	[rbp - 32], rax
mov	rdi, 4
mov	rsi, 0
mov	rdx, [rbp - 24]
xor	rax, rax
call	make_tuple
mov	[rbp - 40], rax
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 40]
movshl	rcx, fn35
xor	rax, rax
call	make_tuple
mov	[rbp - 56], rax
mov	rdi, 8
mov	rsi, 0
mov	rdx, [r15 + 8]
mov	rcx, [rbp - 16]
mov	r8, [rbp - 56]
xor	rax, rax
call	make_tuple
mov	[rbp - 48], rax
mov	rbx, [r15 + 8]
mov	rax, rbx
sub	rax, 2
mov	[rbp - 64], rax
mov	[r15 + 8], rbx
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 48]
movshl	rcx, fn40
xor	rax, rax
call	make_tuple
mov	[rbp - 72], rax
mov	rax, [r15 + 16]
movmem	[rbp - 16], [rbp - 64]
movmem	[rbp - 24], [rbp - 72]
shr	rax, 1
movmem	[rbp - 8], [rax + 8]
movshr	r14, [rax + 16]
mov	rsp, rbp
jmp	r14

fn40:
sub	rsp, 24
movshr	r15, QWORD [rbp - 8]
sub	rsp, 0
mov	rcx, [r15 + 8]
mov	rbx, [r15 + 16]
mov	rax, rbx
imul	rax, rcx
shr	rax, 1
mov	rdx, [rbp - 16]
mov	[rbp - 16], rax
movmem	[rbp - 24], [r15 + 24]
shr	rdx, 1
movmem	[rbp - 8], [rdx + 8]
movshr	r14, [rdx + 16]
mov	rsp, rbp
jmp	r14

fn35:
sub	rsp, 24
movshr	r15, QWORD [rbp - 8]
sub	rsp, 0
mov	rax, [r15 + 8]
shr	rax, 1
movmem	[rbp - 8], [rax + 8]
movshr	r14, [rax + 16]
mov	rsp, rbp
jmp	r14

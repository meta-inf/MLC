
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
mov	rdi, 18
call	gc_check
sub	rsp, 40
mov	rdi, 2
mov	rsi, 0
xor	rax, rax
call	make_tuple
mov	[rbp - 16], rax
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 16]
movshl	rcx, fn55
xor	rax, rax
call	make_tuple
mov	[rbp - 32], rax
mov	rdi, 4
mov	rsi, 0
mov	rdx, [rbp - 32]
xor	rax, rax
call	make_tuple
mov	[rbp - 24], rax
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 24]
movshl	rcx, fn50
xor	rax, rax
call	make_tuple
mov	[rbp - 40], rax
movmem	[rbp - 8], [rbp - 16]
mov	QWORD [rbp - 16], 0
movmem	[rbp - 24], [rbp - 40]
mov	rsp, rbp
jmp	fn55

_ml_term:
mov	rsp, rbp
mov	rax, 0
ret

fn50:
sub	rsp, 24
mov	QWORD [rbp - 40], 0
mov	rdi, 20
call	gc_check
movshr	r15, QWORD [rbp - 8]
sub	rsp, 32
mov	rax, [rbp - 16]
push	rax
mov	rdi, rax
call	dispi
mov	rbx, rax
pop	rax
mov	[rbp - 16], rax
mov	[rbp - 24], rbx
mov	rdi, 4
mov	rsi, 0
mov	rdx, [r15 + 8]
xor	rax, rax
call	make_tuple
mov	[rbp - 32], rax
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 32]
movshl	rcx, fn34
xor	rax, rax
call	make_tuple
mov	[rbp - 48], rax
mov	rdi, 4
mov	rsi, 0
mov	rdx, [rbp - 48]
xor	rax, rax
call	make_tuple
mov	[rbp - 40], rax
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 40]
movshl	rcx, fn31
xor	rax, rax
call	make_tuple
mov	[rbp - 56], rax
movmem	[rbp - 8], [rbp - 32]
mov	QWORD [rbp - 16], 6
movmem	[rbp - 24], [rbp - 56]
mov	rsp, rbp
jmp	fn34

fn31:
sub	rsp, 24
mov	QWORD [rbp - 40], 0
mov	rdi, 8
call	gc_check
movshr	r15, QWORD [rbp - 8]
sub	rsp, 16
mov	rdi, 2
mov	rsi, 0
xor	rax, rax
call	make_tuple
mov	[rbp - 24], rax
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 24]
movshl	rcx, fn28
xor	rax, rax
call	make_tuple
mov	[rbp - 32], rax
mov	rax, [r15 + 8]
mov	QWORD [rbp - 16], 4
movmem	[rbp - 24], [rbp - 32]
shr	rax, 1
movmem	[rbp - 8], [rax + 8]
movshr	r14, [rax + 16]
mov	rsp, rbp
jmp	r14

fn28:
sub	rsp, 24
movshr	r15, QWORD [rbp - 8]
sub	rsp, 0
jmp	_ml_term

fn34:
sub	rsp, 24
mov	rdi, 14
call	gc_check
movshr	r15, QWORD [rbp - 8]
sub	rsp, 32
mov	rdi, 8
mov	rsi, 0
mov	rdx, [rbp - 24]
mov	rcx, [rbp - 16]
mov	r8, [r15 + 8]
xor	rax, rax
call	make_tuple
mov	[rbp - 32], rax
mov	rbx, [rbp - 16]
cmp	4, rbx
setz	al
movzx	rax, al
mov	[rbp - 40], rax
mov	[rbp - 16], rbx
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 32]
movshl	rcx, fn44
xor	rax, rax
call	make_tuple
mov	[rbp - 48], rax
mov	rax, [r15 + 8]
movmem	[rbp - 16], [rbp - 40]
movmem	[rbp - 24], [rbp - 48]
shr	rax, 1
movmem	[rbp - 8], [rax + 8]
movshr	r14, [rax + 16]
mov	rsp, rbp
jmp	r14

fn44:
sub	rsp, 24
mov	QWORD [rbp - 40], 0
mov	rdi, 10
call	gc_check
movshr	r15, QWORD [rbp - 8]
sub	rsp, 32
mov	rax, [rbp - 16]
push	rax
mov	rdi, rax
call	dispi
mov	rbx, rax
pop	rax
mov	[rbp - 16], rax
mov	[rbp - 24], rbx
mov	rdi, 4
mov	rsi, 0
mov	rdx, [r15 + 8]
xor	rax, rax
call	make_tuple
mov	[rbp - 32], rax
mov	rbx, [r15 + 16]
cmp	rbx, 4
setz	al
movzx	rax, al
mov	[rbp - 40], rax
mov	[r15 + 16], rbx
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 32]
movshl	rcx, fn38
xor	rax, rax
call	make_tuple
mov	[rbp - 48], rax
mov	rax, [r15 + 24]
movmem	[rbp - 16], [rbp - 40]
movmem	[rbp - 24], [rbp - 48]
shr	rax, 1
movmem	[rbp - 8], [rax + 8]
movshr	r14, [rax + 16]
mov	rsp, rbp
jmp	r14

fn38:
sub	rsp, 24
movshr	r15, QWORD [rbp - 8]
sub	rsp, 0
mov	rax, [rbp - 16]
push	rax
mov	rdi, rax
call	dispi
mov	rbx, rax
pop	rax
mov	rcx, [r15 + 8]
mov	[rbp - 16], rbx
shr	rcx, 1
movmem	[rbp - 8], [rcx + 8]
movshr	r14, [rcx + 16]
mov	rsp, rbp
jmp	r14

fn55:
sub	rsp, 24
movshr	r15, QWORD [rbp - 8]
sub	rsp, 0
mov	rax, [rbp - 16]
cmp	rax, 0
jz	cf1
mov	rbx, [rbp - 24]
mov	QWORD [rbp - 16], 2
shr	rbx, 1
movmem	[rbp - 8], [rbx + 8]
movshr	r14, [rbx + 16]
mov	rsp, rbp
jmp	r14
cf1:
mov	rbx, [rbp - 24]
mov	QWORD [rbp - 16], 0
shr	rbx, 1
movmem	[rbp - 8], [rbx + 8]
movshr	r14, [rbx + 16]
mov	rsp, rbp
jmp	r14

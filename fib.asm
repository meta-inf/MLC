
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
movshl	rcx, fn30
xor	rax, rax
call	make_tuple
mov	[rbp - 32], rax
movmem	[rbp - 8], [rbp - 16]
mov	QWORD [rbp - 16], 40
movmem	[rbp - 24], [rbp - 32]
mov	rsp, rbp
jmp	fn16

_ml_term:
mov	rsp, rbp
mov	rax, 0
ret

fn30:
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
mov	rdi, 24
call	gc_check
movshr	r15, QWORD [rbp - 8]
sub	rsp, 128
mov	rdi, 2
mov	rsi, 0
xor	rax, rax
call	make_tuple
mov	[rbp - 32], rax
mov	rdi, 2
mov	rsi, 0
xor	rax, rax
call	make_tuple
mov	[rbp - 40], rax
mov	rdi, 6
mov	rsi, 0
savetup	rdx
movshl	rcx, fn16
xor	rax, rax
call	make_tuple
mov	[rbp - 56], rax
mov	rdi, 4
mov	rsi, 0
mov	rdx, [rbp - 56]
xor	rax, rax
call	make_tuple
mov	[rbp - 48], rax
mov	rbx, [rbp - 16]
cmp	0, rbx
setz	al
movzx	rax, al
cmp	rax, 0
jz	cf1
cmp	2, rbx
setz	cl
movzx	rcx, cl
cmp	rcx, 0
jz	cf3
mov	[rbp - 64], rax
mov	[rbp - 16], rbx
mov	[rbp - 112], rcx
mov	rdi, 4
mov	rsi, 0
mov	rdx, [rbp - 24]
xor	rax, rax
call	make_tuple
mov	[rbp - 136], rax
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 136]
movshl	rcx, fn52
xor	rax, rax
call	make_tuple
mov	[rbp - 144], rax
movmem	[rbp - 8], [rbp - 48]
movmem	[rbp - 24], [rbp - 144]
mov	rsp, rbp
jmp	fn59
cf3:
mov	[rbp - 64], rax
mov	[rbp - 16], rbx
mov	[rbp - 112], rcx
mov	rdi, 4
mov	rsi, 0
mov	rdx, [rbp - 24]
xor	rax, rax
call	make_tuple
mov	[rbp - 120], rax
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 120]
movshl	rcx, fn48
xor	rax, rax
call	make_tuple
mov	[rbp - 128], rax
movmem	[rbp - 8], [rbp - 32]
mov	QWORD [rbp - 16], 0
movmem	[rbp - 24], [rbp - 128]
mov	rsp, rbp
jmp	fn77
cf1:
cmp	2, rbx
setz	cl
movzx	rcx, cl
cmp	rcx, 0
jz	cf2
mov	[rbp - 64], rax
mov	[rbp - 16], rbx
mov	[rbp - 72], rcx
mov	rdi, 4
mov	rsi, 0
mov	rdx, [rbp - 24]
xor	rax, rax
call	make_tuple
mov	[rbp - 96], rax
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 96]
movshl	rcx, fn41
xor	rax, rax
call	make_tuple
mov	[rbp - 104], rax
movmem	[rbp - 8], [rbp - 40]
mov	QWORD [rbp - 16], 0
movmem	[rbp - 24], [rbp - 104]
mov	rsp, rbp
jmp	fn74
cf2:
mov	[rbp - 64], rax
mov	[rbp - 16], rbx
mov	[rbp - 72], rcx
mov	rdi, 4
mov	rsi, 0
mov	rdx, [rbp - 24]
xor	rax, rax
call	make_tuple
mov	[rbp - 80], rax
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 80]
movshl	rcx, fn37
xor	rax, rax
call	make_tuple
mov	[rbp - 88], rax
movmem	[rbp - 8], [rbp - 48]
movmem	[rbp - 24], [rbp - 88]
mov	rsp, rbp
jmp	fn59

fn52:
sub	rsp, 24
movshr	r15, QWORD [rbp - 8]
sub	rsp, 0
mov	rax, [r15 + 8]
shr	rax, 1
movmem	[rbp - 8], [rax + 8]
movshr	r14, [rax + 16]
mov	rsp, rbp
jmp	r14

fn48:
sub	rsp, 24
movshr	r15, QWORD [rbp - 8]
sub	rsp, 0
mov	rax, [r15 + 8]
shr	rax, 1
movmem	[rbp - 8], [rax + 8]
movshr	r14, [rax + 16]
mov	rsp, rbp
jmp	r14

fn41:
sub	rsp, 24
movshr	r15, QWORD [rbp - 8]
sub	rsp, 0
mov	rax, [r15 + 8]
shr	rax, 1
movmem	[rbp - 8], [rax + 8]
movshr	r14, [rax + 16]
mov	rsp, rbp
jmp	r14

fn37:
sub	rsp, 24
movshr	r15, QWORD [rbp - 8]
sub	rsp, 0
mov	rax, [r15 + 8]
shr	rax, 1
movmem	[rbp - 8], [rax + 8]
movshr	r14, [rax + 16]
mov	rsp, rbp
jmp	r14

fn59:
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
mov	rax, rbx
sub	rax, 2
mov	[rbp - 40], rax
mov	[rbp - 16], rbx
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 32]
movshl	rcx, fn69
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

fn69:
sub	rsp, 24
mov	QWORD [rbp - 40], 0
mov	rdi, 12
call	gc_check
movshr	r15, QWORD [rbp - 8]
sub	rsp, 16
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 16]
mov	rcx, [r15 + 8]
xor	rax, rax
call	make_tuple
mov	[rbp - 24], rax
mov	rbx, [r15 + 16]
mov	rax, rbx
sub	rax, 4
mov	[r15 + 16], rbx
mov	[rbp - 32], rax
mov	rdi, 6
mov	rsi, 0
mov	rdx, [rbp - 24]
movshl	rcx, fn64
xor	rax, rax
call	make_tuple
mov	[rbp - 40], rax
mov	rax, [r15 + 24]
movmem	[rbp - 16], [rbp - 32]
movmem	[rbp - 24], [rbp - 40]
shr	rax, 1
movmem	[rbp - 8], [rax + 8]
movshr	r14, [rax + 16]
mov	rsp, rbp
jmp	r14

fn64:
sub	rsp, 24
movshr	r15, QWORD [rbp - 8]
sub	rsp, 0
mov	rcx, [rbp - 16]
mov	rbx, [r15 + 8]
mov	rax, rbx
add	rax, rcx
mov	rdx, [r15 + 16]
mov	[rbp - 16], rax
shr	rdx, 1
movmem	[rbp - 8], [rdx + 8]
movshr	r14, [rdx + 16]
mov	rsp, rbp
jmp	r14

fn74:
sub	rsp, 24
movshr	r15, QWORD [rbp - 8]
sub	rsp, 0
mov	rax, [rbp - 24]
mov	QWORD [rbp - 16], 2
shr	rax, 1
movmem	[rbp - 8], [rax + 8]
movshr	r14, [rax + 16]
mov	rsp, rbp
jmp	r14

fn77:
sub	rsp, 24
movshr	r15, QWORD [rbp - 8]
sub	rsp, 0
mov	rax, [rbp - 24]
mov	QWORD [rbp - 16], 2
shr	rax, 1
movmem	[rbp - 8], [rax + 8]
movshr	r14, [rax + 16]
mov	rsp, rbp
jmp	r14

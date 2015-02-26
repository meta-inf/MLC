#ifndef HEAP_INCLUDED
#define HEAP_INCLUDED

typedef u_int64_t qword;

typedef struct heap_t
{
	qword *s, *e, *cur;
} heap_t;

qword* halloc (qword);
void gc (qword *, qword *, qword, int);
void heap_init ();

#endif

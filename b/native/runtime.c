/* runtime.c: implementation of some built-in functions */

#include <assert.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include "heap.h"

extern heap_t heap[2];

void match_failure (int a)
{
	printf("Match failure\n");
	exit(1);
}

int equiv (qword a, qword b)
{
	qword header, *x, *y;
	int len, i;
	if (!(a & 1) || !(b & 1))
	{
		return a == b;
	}
	x = (qword*) (a >> 1);
	y = (qword*) (b >> 1);

	if (x[0] != y[0])
	{
		return 0;
	}
	len = x[0] >> 48; /* TODO: only tuples are considered. */
	for (i = 1; i < len; ++i)
	{
		if (!equiv (x[i], y[i]))
		{
			return 0;
		}
	}
	return 2;
}

void gc_check (qword size)
{
	if (heap[0].cur + size >= heap[0].e)
	{
		qword *rs, *re, rt;
		asm volatile ("movq\t%%rbp, %0": "=r" (rt));
		re = (qword*) rt;
		rs = (qword*) *re;
		gc (rs, re + 1, size, 0);
	}
}

qword* make_tuple (qword n, ...)
{
	va_list vl;
	qword cur, i, *ret;
	n >>= 1;

	va_start(vl, n);
	cur = va_arg(vl, qword);
	assert(! (cur & 1));

	ret = halloc(n);
	ret[0] = (n << 48ULL) | cur;
	for (i = 1; i < n; ++i)
	{
		ret[i] = va_arg (vl, qword);
	}

#ifdef DEBUG
	fprintf(stderr, "%lld\n", heap[0].cur - heap[0].s);
	fprintf (stderr, "make_tuple: %llx -> %llx: %llx", n, ret, cur);
	for (i = 1; i < n; ++i)
	{
		fprintf (stderr, " %llx", ret[i]);
	}
	fprintf (stderr, "\n");
#endif
	return (qword*)(((qword)ret << 1) | 1);
}

void dispi (int64_t n)
{
	printf("%lld\n", n >> 1);
}

void disp (qword n)
{
	printf("%s\n", (char*)(n >> 1));
}

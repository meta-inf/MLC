#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <memory.h>
#include "heap.h"

#define HSIZE (1 << 27) // 64 MB

heap_t heap[2];
qword *mem_s, *mem_e, *que;

qword *halloc (qword size)
{
	assert(heap[0].cur + size < heap[0].e);
	heap[0].cur += size;
	return heap[0].cur - size;
}

void heap_init ()
{
	mem_s = malloc(HSIZE * sizeof(qword));
	mem_e = mem_s + HSIZE;
	heap[0] = (heap_t){mem_s, mem_s + (HSIZE >> 1), mem_s};
	heap[1] = (heap_t){mem_s + (HSIZE >> 1), mem_e, mem_s + (HSIZE >> 1)};
	que = malloc((HSIZE >> 1) * sizeof(qword));
}

/*
 * Semi-space collector.
 * The used semispace heap[0] is used as the hash map for tuple's new address.
 * For tuple ptr, if *ptr & 1 == 0, then ptr is not moved yet;
 * otherwise it's moved to address *ptr >> 1.
 */

inline qword* gc_enque (qword *h)
{
	int len;

	assert(!(h[0] & 1)); 
	len = h[0] >> 48ULL;

	memcpy(heap[1].cur, h, len * sizeof(qword));

	h[0] = ((qword)heap[1].cur << 1) | 1; 

	heap[1].cur += len;
	return heap[1].cur - len;
}

void gc (qword *stk_s, qword *stk_e, qword nsize, int copyOnly)
{
//    fprintf(stderr, "gc invoked\n");
	int f = 0, r = 0;
	int used_orig;
	qword *i;
	used_orig = heap[0].cur - heap[0].s;
	heap[1].cur = heap[1].s;

	for (i = stk_s; i != stk_e; --i)
	{
		if ((*i & 1) && !(((qword*)(*i >> 1))[0] & 1))
			que[++r] = (qword) gc_enque((qword*) (*i >> 1));
	}

	while (f != r)
	{
		qword *h = (qword*) que[++f];
		int len = h[0] >> 48ULL, i;
		for (i = 1; i < len; ++i)
			if (h[i] & 1)
			{
				qword *g = (qword*) (h[i] >> 1);
				if (!(g[0] & 1))
				{
					que[++r] = (qword) gc_enque(g);
				}
			}
	}

	for (i = heap[1].s; i != heap[1].cur; ++i)
	{
		if (*i & 1)
		{
			*i = *(qword*)(*i >> 1);
		}
	}
	for (i = stk_s; i != stk_e; --i)
	{
		if (*i & 1)
		{
			*i = *(qword*)(*i >> 1);
		}
	}

	heap_t tmp = heap[0]; heap[0] = heap[1]; heap[1] = tmp;
	int used_now = heap[0].cur - heap[0].s,
		pct = (long long)(used_orig - used_now) * 100 / used_orig;

	if (copyOnly)
	{
		return;
	}

#ifdef PROFGC
	fprintf(stderr, "GC: recycled %d of %d QW (%lld %)\n", 
			used_orig - used_now, used_orig, pct);
#endif
	if (heap[0].cur + nsize >= heap[0].e || pct < 50)
	{
		qword size, *orig;
#ifdef DEBUG
		fprintf(stderr, "GC: Memory pool is full (cur = %d QW). Enlarging.\n", 2 * (heap[0].e - heap[0].s));
#endif
		size = 2 * (heap[0].e - heap[0].s);
		if (heap[0].s == heap[1].e)
		{
			orig = heap[1].s;
		}
		else
		{
			assert(heap[0].e == heap[1].s); // assert(I will not be called twice);
			orig = heap[0].s;
		}
		heap[1].s = malloc(sizeof(qword) * (2 * size));
		heap[1].e = heap[1].s + (2 * size);
		heap[1].cur = heap[1].s;
		gc(stk_s, stk_e, nsize, 1);
		free(que);
		que = orig;
		heap[1].e = heap[0].e;
		heap[0].e = heap[0].s + size;
		assert(heap[0].cur + nsize < heap[0].e);
		heap[1].s = heap[1].cur = heap[0].e;
	}
}


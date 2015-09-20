#include "lisc.h"
#ifdef TEST_PMOV
	#undef assert
	#define assert(x) assert_test(#x, x)
#endif

typedef struct RMap RMap;
struct RMap {
	int t[NReg];
	int r[NReg];
	Bits b;
	int n;
};

extern Ins insb[NIns], *curi;
static ulong regu;     /* registers used */
static Tmp *tmp;       /* function temporaries */
static struct {
	Ref src, dst;
	int wide;
} *pm;                 /* parallel move constructed */
static int cpm, npm;   /* capacity and size of pm */

static int
rfind(RMap *m, int t)
{
	int i;

	for (i=0; i<m->n; i++)
		if (m->t[i] == t)
			return m->r[i];
	return -1;
}

static Ref
rref(RMap *m, int t)
{
	int r, s;

	r = rfind(m, t);
	if (r == -1) {
		s = tmp[t].spill;
		assert(s && "should have spilled");
		return SLOT(s);
	} else
		return TMP(r);
}

static void
radd(RMap *m, int t, int r)
{
	assert((t >= Tmp0 || t == r) && "invalid temporary");
	assert(RAX <= r && r < RAX + NReg && "invalid register");
	assert(!BGET(m->b, t) && "temp has mapping");
	assert(!BGET(m->b, r) && "reg already allocated");
	assert(m->n <= NReg && "too many mappings");
	BSET(m->b, t);
	BSET(m->b, r);
	m->t[m->n] = t;
	m->r[m->n] = r;
	m->n++;
	regu |= BIT(r);
}

static Ref
ralloc(RMap *m, int t)
{
	int r;

	if (t < Tmp0) {
		assert(BGET(m->b, t));
		return TMP(t);
	}
	if (BGET(m->b, t)) {
		r = rfind(m, t);
		assert(r != -1);
	} else {
		r = tmp[t].hint;
		if (r == -1 || BGET(m->b, r))
			for (r=RAX; BGET(m->b, r); r++)
				if (r+1 == RAX+NReg)
					diag("rega: no more regs");
		radd(m, t, r);
		if (tmp[t].hint == -1)
			tmp[t].hint = r;
	}
	return TMP(r);
}

static int
rfree(RMap *m, int t)
{
	int i, r;

	if (!BGET(m->b, t))
		return -1;
	for (i=0; m->t[i] != t; i++)
		assert(i+1 < m->n);
	r = m->r[i];
	BCLR(m->b, t);
	BCLR(m->b, r);
	m->n--;
	memmove(&m->t[i], &m->t[i+1], (m->n-i) * sizeof m->t[0]);
	memmove(&m->r[i], &m->r[i+1], (m->n-i) * sizeof m->r[0]);
	return r;
}

static void
mdump(RMap *m)
{
	int i;

	for (i=0; i<m->n; i++)
		fprintf(stderr, " (%s, R%d)",
			tmp[m->t[i]].name,
			m->r[i]);
	fprintf(stderr, "\n");
}

static void
pmadd(Ref src, Ref dst, int w)
{
	if (npm == cpm) {
		cpm = cpm * 2 + 16;
		pm = realloc(pm, cpm * sizeof pm[0]);
		if (!pm)
			diag("pmadd: out of memory");
	}
	pm[npm].src = src;
	pm[npm].dst = dst;
	pm[npm].wide = w;
	npm++;
}

enum PMStat { ToMove, Moving, Moved };

static Ref
pmrec(enum PMStat *status, int i, int *w)
{
	Ref swp, swp1;
	int j, w1;

	if (req(pm[i].src, pm[i].dst))
		return R;
	status[i] = Moving;
	*w |= pm[i].wide;
	swp = R;
	for (j=0; j<npm; j++) {
		if (req(pm[j].src, pm[i].dst))
			switch (status[j]) {
			case ToMove:
				w1 = *w;
				swp1 = pmrec(status, j, &w1);
				if (!req(swp1, R)) {
					assert(req(swp, R));
					swp = swp1;
					*w = w1;
				}
				break;
			case Moving:
				assert(req(swp, R));
				swp = pm[i].dst;
				break;
			case Moved:
				break;
			}
	}
	status[i] = Moved;
	if (req(swp, R)) {
		*curi++ = (Ins){OCopy, pm[i].wide, pm[i].dst, {pm[i].src}};
		return R;
	} else if (!req(swp, pm[i].src)) {
		*curi++ = (Ins){OSwap, *w, R, {pm[i].src, pm[i].dst}};
		return swp;
	} else
		return R;

}

static void
pmgen()
{
	int i, w;
	enum PMStat *status;

	status = alloc(npm * sizeof status[0]);
	assert(!npm || status[npm-1] == ToMove);
	curi = insb;
	for (i=0; i<npm; i++)
		if (status[i] == ToMove) {
			w = 0;
			pmrec(status, i, &w);
		}
	free(status);
}

static void
move(int r, Ref to, RMap *m)
{
	int n, t, r1;

	r1 = req(to, R) ? -1 : rfree(m, to.val);
	if (BGET(m->b, r) && r1 != r) {
		/* r is used and not by to */
		for (n=0; m->r[n] != r; n++)
			assert(n+1 < m->n);
		t = m->t[n];
		rfree(m, t);
		BSET(m->b, r);
		ralloc(m, t);
		BCLR(m->b, r);
	}
	t = r1 == -1 ? r : to.val;
	radd(m, t, r);
}

static Ins *
dopm(Blk *b, Ins *i, RMap *m)
{
	RMap m0;
	int n, r, r1, t, nins;
	Ins *i1, *ib, *ip, *ir;
	ulong def;

	m0 = *m;
	i1 = i+1;
	for (;; i--) {
		move(i->arg[0].val, i->to, m);
		if (i == b->ins
		|| (i-1)->op != OCopy
		|| !isreg((i-1)->arg[0]))
			break;
	}
	assert(m0.n <= m->n);
	if (i > b->ins && (i-1)->op == OCall) {
		def = calldef(*i, 0);
		for (r=0; r<NRSave; r++)
			if (!(BIT(rsave[r]) & def))
				move(rsave[r], R, m);
	}
	for (npm=0, n=0; n<m->n; n++)
		if ((t = m->t[n]) >= Tmp0) {
			r1 = m->r[n];
			r = rfind(&m0, t);
			assert(r != -1);
			pmadd(TMP(r1), TMP(r), tmp[t].wide);
		}
	for (ip=i; ip<i1; ip++) {
		if (!req(ip->to, R))
			rfree(m, ip->to.val);
		r = ip->arg[0].val;
		if (rfind(m, r) == -1)
			radd(m, r, r);
	}
	pmgen();
#ifdef TEST_PMOV
	return 0;
#endif
	nins = curi-insb;
	ib = alloc((b->nins + nins - (i1-i)) * sizeof(Ins));
	memcpy(ip = ib, b->ins, (i - b->ins) * sizeof(Ins));
	ip += i - b->ins;
	memcpy(ir = ip, insb, nins * sizeof(Ins));
	ip += nins;
	memcpy(ip, i1, (&b->ins[b->nins] - i1) * sizeof(Ins));
	b->nins += nins - (i1-i);
	free(b->ins);
	b->ins = ib;
	return ir;
}

/* register allocation
 * depends on rpo, cost, (and obviously spill)
 */
void
rega(Fn *fn)
{
	int n, t, r, x;
	Blk *b, *b1, *s, ***ps, *blist;
	Ins *i;
	RMap *end, *beg, cur;
	Phi *p;
	uint a;
	Ref src, dst;
	ulong rs;

	regu = 0;
	tmp = fn->tmp;
	end = alloc(fn->nblk * sizeof end[0]);
	beg = alloc(fn->nblk * sizeof beg[0]);

	/* 1. gross hinting setup */
	for (t=Tmp0; t<fn->ntmp; t++)
		tmp[t].hint = -1;
	for (b=fn->start; b; b=b->link)
		for (i=b->ins; i-b->ins < b->nins; i++) {
			if (i->op != OCopy)
				continue;
			if (rtype(i->arg[0]) == RTmp && isreg(i->to))
				tmp[i->arg[0].val].hint = i->to.val;
			if (rtype(i->to) == RTmp && isreg(i->arg[0]))
				tmp[i->to.val].hint = i->arg[0].val;
		}

	/* 2. assign registers backwards */
	if (debug['R'])
		fprintf(stderr, "> Register mappings:\n");
	for (n=fn->nblk-1; n>=0; n--) {
		b = fn->rpo[n];
		cur.n = 0;
		cur.b = (Bits){{0}};
		for (x=0; x<2; x++)
			for (t=Tmp0; t<fn->ntmp; t++)
				if (BGET(b->out, t))
				if (!BGET(cur.b, t))
				if (x == 1
				|| ((r=tmp[t].hint) != -1 && !BGET(cur.b, r)))
					ralloc(&cur, t);
		/* process instructions */
		end[n] = cur;
		if (rtype(b->jmp.arg) == RTmp)
			b->jmp.arg = ralloc(&cur, b->jmp.arg.val);
		for (i=&b->ins[b->nins]; i!=b->ins;) {
			switch ((--i)->op) {
			case OCall:
				rs = calluse(*i, 0);
				for (r=0; r<NRSave; r++)
					if (!(BIT(rsave[r]) & rs))
						rfree(&cur, rsave[r]);
				continue;
			case OCopy:
				if (!isreg(i->arg[0]))
					break;
				i = dopm(b, i, &cur);
				continue;
			}
			if (!req(i->to, R)) {
				assert(rtype(i->to) == RTmp);
				r = rfree(&cur, i->to.val);
				if (r == -1) {
					*i = (Ins){.op = ONop};
					continue;
				}
				if (i->to.val >= Tmp0)
					i->to = TMP(r);
			} else
				r = 0;
			for (x=0; x<2; x++)
				if (rtype(i->arg[x]) == RTmp) {
					/* <arch>
				 	*   on Intel, we attempt to
				 	*   use the same register
				 	*   for the return and one
				 	*   argument
				 	*/
					t = i->arg[x].val;
					if (r && !BGET(cur.b, r))
					if (tmp[t].hint == -1)
						tmp[t].hint = r;
					i->arg[x] = ralloc(&cur, t);
				}
		}
		b->in = cur.b;
		for (p=b->phi; p; p=p->link)
			if (rtype(p->to) == RTmp)
				BCLR(b->in, p->to.val);
		beg[n] = cur;
		if (debug['R']) {
			fprintf(stderr, "\t%-10s beg", b->name);
			mdump(&beg[n]);
			fprintf(stderr, "\t           end");
			mdump(&end[n]);
		}
	}
	if (debug['R'])
		fprintf(stderr, "\n");

	/* 3. compose glue code */
	blist = 0;
	for (b=fn->start;; b=b->link) {
		ps = (Blk**[3]){&b->s1, &b->s2, (Blk*[1]){0}};
		for (; (s=**ps); ps++) {
			npm = 0;
			for (p=s->phi; p; p=p->link) {
				dst = p->to;
				assert(rtype(dst)==RSlot|| rtype(dst)==RTmp);
				if (rtype(dst) == RTmp) {
					r = rfind(&beg[s->id], dst.val);
					if (r == -1)
						continue;
					dst = TMP(r);
				}
				for (a=0; p->blk[a]!=b; a++)
					assert(a+1 < p->narg);
				src = p->arg[a];
				if (rtype(src) == RTmp)
					src = rref(&end[b->id], src.val);
				pmadd(src, dst, p->wide);
			}
			for (t=Tmp0; t<fn->ntmp; t++)
				if (BGET(s->in, t)) {
					src = rref(&end[b->id], t);
					dst = rref(&beg[s->id], t);
					pmadd(src, dst, tmp[t].wide);
				}
			pmgen();
			/* todo, moving this out of
			 * here would make sense */
			n = curi-insb;
			if (!n)
				continue;
			b1 = blocka();
			b1->loop = (b->loop+s->loop) / 2;
			b1->link = blist;
			blist = b1;
			fn->nblk++;
			sprintf(b1->name, "%s_%s", b->name, s->name);
			i = alloc(n * sizeof(Ins));
			memcpy(i, insb, n * sizeof(Ins));
			b1->ins = i;
			b1->nins = n;
			b1->jmp.type = JJmp;
			b1->s1 = s;
			**ps = b1;
		}
		if (!b->link) {
			b->link = blist;
			break;
		}
	}
	for (b=fn->start; b; b=b->link)
		while ((p=b->phi)) {
			b->phi = p->link;
			free(p);
		}
	fn->reg = regu;

	free(end);
	free(beg);
}

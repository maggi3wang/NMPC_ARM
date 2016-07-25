/* ../src/utcomsubs.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

extern struct {
    integer iprint, isumm, lines1, lines2;
} sol1cm_;

#define sol1cm_1 sol1cm_

extern struct {
    doublereal wmach[15];
} solmch_;

#define solmch_1 solmch_

extern struct {
    doublereal epspt3, epspt5, epspt8, epspt9;
} sol4cm_;

#define sol4cm_1 sol4cm_

extern struct {
    doublereal tolx0, tolinc;
    integer idegen, kdegen, ndegen, itnfix, nfix[2];
} sol3lc_;

#define sol3lc_1 sol3lc_

/* Table of constant values */

static integer c__1 = 1;
static integer c__6 = 6;
static doublereal c_b14 = 0.;
static integer c__2 = 2;
static doublereal c_b19 = 1.;
static doublereal c_b59 = .6;

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*     file  utcomsubs.f */

/*     cmAfac   cmcrsh   cmchzr   cmdgen   cmfeas   cminit   cmmsg1 */
/*     cmmul1   cmmul2   cmsetx   cmsinf   cmwrap */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int cmafac_(integer *inform__, logical *needtq, logical *
	gset, logical *unitq, logical *vertex, integer *nclin, integer *
	nactiv, integer *nartif, integer *nfree, integer *nz, integer *n, 
	integer *ldq, integer *lda, integer *ldt, integer *istate, integer *
	kactiv, integer *kx, doublereal *condmx, doublereal *errmax, integer *
	jmax, doublereal *a, doublereal *ax, doublereal *bl, doublereal *bu, 
	doublereal *g, doublereal *gq, doublereal *featol, doublereal *t, 
	doublereal *x, doublereal *q, doublereal *w, doublereal *c__, 
	doublereal *s)
{
    /* Format strings */
    static char fmt_9000[] = "(\002 XXX  Cannot satisfy the working-set cons"
	    "traints.\002)";

    /* System generated locals */
    integer a_dim1, a_offset, t_dim1, t_offset, q_dim1, q_offset, i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(void);

    /* Local variables */
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    integer ktry, nact1, i__, j, k;
    extern /* Subroutine */ int dload_(integer *, doublereal *, doublereal *, 
	    integer *), dgemv_(char *, integer *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, integer *, ftnlen), dcopy_(integer *, doublereal *, 
	    integer *, doublereal *, integer *), daxpy_(integer *, doublereal 
	    *, doublereal *, integer *, doublereal *, integer *), dtrsv_(char 
	    *, char *, char *, integer *, doublereal *, integer *, doublereal 
	    *, integer *, ftnlen, ftnlen, ftnlen);
    integer is, it;
    extern integer idamax_(integer *, doublereal *, integer *);
    integer ncolgq, nrejtd;
    extern /* Subroutine */ int rzadds_(logical *, logical *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *), cmqmul_(integer *, integer *, 
	    integer *, integer *, integer *, logical *, integer *, doublereal 
	    *, doublereal *, doublereal *);
    logical rowerr;
    doublereal bnd;

    /* Fortran I/O blocks */
    static cilist io___12 = { 0, 0, 0, fmt_9000, 0 };
    static cilist io___13 = { 0, 0, 0, fmt_9000, 0 };


/*     ================================================================== */
/*     cmAfac  computes the point on a working set that is closest in the */
/*             least-squares sense to the input vector x. */

/*     If the computed point gives a row error of more than the */
/*     feasibility tolerance, an extra step of iterative refinement is */
/*     used.  If  x  is still infeasible,  an error message is printed. */

/*     w, s and c are work vectors. */

/*     Original version written by PEG,  August 1991. */
/*     This version of  cmAfac  dated  09-May-93. */
/*     ================================================================== */
    /* Parameter adjustments */
    --s;
    --c__;
    --w;
    --x;
    --featol;
    --bu;
    --bl;
    --kx;
    --kactiv;
    --istate;
    q_dim1 = *ldq;
    q_offset = q_dim1 + 1;
    q -= q_offset;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    t_dim1 = *ldt;
    t_offset = t_dim1 + 1;
    t -= t_offset;
    --ax;
    --g;
    --gq;

    /* Function Body */
    *inform__ = 0;
    if (*needtq) {
/*        --------------------------------------------------------------- */
/*        Compute the TQ factorization of the working-set matrix. */
/*        Install the transformed linear term in gq. */
/*        --------------------------------------------------------------- */
	*unitq = TRUE_;
	*nz = *nfree;
	if (*nactiv > 0) {
	    it = *nactiv + 1;
	    nact1 = *nactiv;
	    *nactiv = 0;
	    ncolgq = 0;
	    rzadds_(unitq, vertex, &c__1, &nact1, &it, nactiv, nartif, nz, 
		    nfree, &nrejtd, &ncolgq, n, ldq, lda, ldt, &istate[1], &
		    kactiv[1], &kx[1], condmx, &a[a_offset], &t[t_offset], &w[
		    1], &q[q_offset], &w[1], &c__[1], &s[1]);
	}
	if (*gset) {
	    dcopy_(n, &g[1], &c__1, &gq[1], &c__1);
	    cmqmul_(&c__6, n, nz, nfree, ldq, unitq, &kx[1], &gq[1], &q[
		    q_offset], &w[1]);
	}
    }
/*     ------------------------------------------------------------------ */
/*     Move  x  onto the simple bounds in the working set. */
/*     ------------------------------------------------------------------ */
    i__1 = *n;
    for (k = *nfree + 1; k <= i__1; ++k) {
	j = kx[k];
	is = istate[j];
	bnd = bl[j];
	if (is >= 2) {
	    bnd = bu[j];
	}
	if (is != 4) {
	    x[j] = bnd;
	}
/* L100: */
    }
/*     ------------------------------------------------------------------ */
/*     Move  x  onto the general constraints in the working set. */
/*     ktry  attempts are made to get acceptable row errors. */
/*     ------------------------------------------------------------------ */
    ktry = 1;
    *jmax = 1;
    *errmax = 0.;
/*     ------------------------------------------------------------------ */
/* +    repeat */
L200:
    if (*nactiv > 0) {
/*           Set w = (residuals of constraints in the working set). */
/*           Solve for s, the smallest correction to x that gives a point */
/*           on the constraints in the working set.  Define  s = Y*(sY), */
/*           where  sY  solves the triangular system  T*(sY) = residuals. */
	i__1 = *nactiv;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    k = kactiv[i__];
	    j = *n + k;
	    if (istate[j] == 2) {
		bnd = bu[j];
	    } else {
		bnd = bl[j];
	    }
	    w[*nactiv - i__ + 1] = bnd - ddot_(n, &a[k + a_dim1], lda, &x[1], 
		    &c__1);
/* L220: */
	}
	dtrsv_("U", "N", "N", nactiv, &t[(*nz + 1) * t_dim1 + 1], ldt, &w[1], 
		&c__1, (ftnlen)1, (ftnlen)1, (ftnlen)1);
	dload_(n, &c_b14, &s[1], &c__1);
	dcopy_(nactiv, &w[1], &c__1, &s[*nz + 1], &c__1);
	cmqmul_(&c__2, n, nz, nfree, ldq, unitq, &kx[1], &s[1], &q[q_offset], 
		&w[1]);
	daxpy_(n, &c_b19, &s[1], &c__1, &x[1], &c__1);
    }
/*        --------------------------------------------------------------- */
/*        Initialize  Ax  for all the general constraints. */
/*        --------------------------------------------------------------- */
    if (*nclin > 0) {
	dgemv_("N", nclin, n, &c_b19, &a[a_offset], lda, &x[1], &c__1, &c_b14,
		 &ax[1], &c__1, (ftnlen)1);
    }
/*        --------------------------------------------------------------- */
/*        Check the row residuals. */
/*        --------------------------------------------------------------- */
    if (*nactiv > 0) {
	i__1 = *nactiv;
	for (k = 1; k <= i__1; ++k) {
	    i__ = kactiv[k];
	    j = *n + i__;
	    is = istate[j];
	    if (is == 1) {
		w[k] = bl[j] - ax[i__];
	    }
	    if (is >= 2) {
		w[k] = bu[j] - ax[i__];
	    }
/* L300: */
	}
	*jmax = idamax_(nactiv, &w[1], &c__1);
	*errmax = (d__1 = w[*jmax], abs(d__1));
    }
    ++ktry;
/* +    until    (errmax .le. featol(jmax) .or. ktry .gt. ntry */
    if (! (*errmax <= featol[*jmax] || ktry > 5)) {
	goto L200;
    }
/*     ------------------------------------------------------------------ */
    rowerr = *errmax > featol[*jmax];
    if (rowerr) {
	*inform__ = 2;
	if (sol1cm_1.iprint > 0) {
	    io___12.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___12);
	    e_wsfe();
	}
	if (sol1cm_1.isumm > 0) {
	    io___13.ciunit = sol1cm_1.isumm;
	    s_wsfe(&io___13);
	    e_wsfe();
	}
    }
    return 0;
/*     end of cmAfac */
} /* cmafac_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int cmcrsh_(char *start, logical *vertex, integer *nclin, 
	integer *nctotl, integer *nactiv, integer *nartif, integer *nfree, 
	integer *n, integer *lda, integer *istate, integer *kactiv, integer *
	kx, doublereal *bigbnd, doublereal *tolact, doublereal *a, doublereal 
	*ax, doublereal *bl, doublereal *bu, doublereal *featol, doublereal *
	x, doublereal *wx, doublereal *work, ftnlen start_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    integer imin, jmin, jfix;
    doublereal resl, resu;
    integer i__, j, k, jfree;
    doublereal flmax;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *);
    doublereal b1, b2;
    integer is;
    doublereal residl, biglow, bigupp, toobig, colmin, resmin, colsiz, tol;

/*     ================================================================== */
/*     cmcrsh  computes the quantities  istate (optionally),  kactiv, */
/*     nactiv,  nz  and  nfree  associated with the working set at x. */

/*     The computation depends upon the value of the input parameter */
/*     start,  as follows... */

/*     Start = 'cold'  An initial working set will be selected. First, */
/*                     nearly-satisfied or violated bounds are added. */
/*                     Next,  general linear constraints are added that */
/*                     have small residuals. */

/*     Start = 'warm'  The quantities kactiv, nactiv and nfree are */
/*                     initialized from istate,  specified by the user. */

/*     If vertex is true, an artificial vertex is defined by fixing some */
/*     variables on their bounds.  Infeasible variables selected for the */
/*     artificial vertex are fixed at their nearest bound.  Otherwise, */
/*     the variables are unchanged. */

/*     Values of istate(j).... */

/*        - 2         - 1         0           1          2         3 */
/*     a'x lt bl   a'x gt bu   a'x free   a'x = bl   a'x = bu   bl = bu */

/*     Original version written by  PEG, 31-October-1984. */
/*     This version of  cmcrsh  dated 11-May-1995. */
/*     ================================================================== */
    /* Parameter adjustments */
    --featol;
    --bu;
    --bl;
    --istate;
    --work;
    --wx;
    --x;
    --kx;
    --kactiv;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --ax;

    /* Function Body */
    flmax = solmch_1.wmach[6];
    biglow = -(*bigbnd);
    bigupp = *bigbnd;
/*     ------------------------------------------------------------------ */
/*     Move the variables inside their bounds. */
/*     ------------------------------------------------------------------ */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	b1 = bl[j];
	b2 = bu[j];
	tol = featol[j];
	if (b1 > biglow) {
	    if (x[j] < b1 - tol) {
		x[j] = b1;
	    }
	}
	if (b2 < bigupp) {
	    if (x[j] > b2 + tol) {
		x[j] = b2;
	    }
	}
/* L10: */
    }
    dcopy_(n, &x[1], &c__1, &wx[1], &c__1);
    *nfree = *n;
    *nactiv = 0;
    *nartif = 0;
    if (s_cmp(start, "cold", (ftnlen)4, (ftnlen)4) == 0) {
	i__1 = *nctotl;
	for (j = 1; j <= i__1; ++j) {
	    istate[j] = 0;
	    if (bl[j] == bu[j]) {
		istate[j] = 3;
	    }
/* L100: */
	}
    } else if (s_cmp(start, "warm", (ftnlen)4, (ftnlen)4) == 0) {
	i__1 = *nctotl;
	for (j = 1; j <= i__1; ++j) {
	    if (istate[j] > 3 || istate[j] < 0) {
		istate[j] = 0;
	    }
	    if (bl[j] <= biglow && bu[j] >= bigupp) {
		istate[j] = 0;
	    }
	    if (bl[j] <= biglow && istate[j] == 1) {
		istate[j] = 0;
	    }
	    if (bu[j] >= bigupp && istate[j] == 2) {
		istate[j] = 0;
	    }
	    if (bl[j] != bu[j] && istate[j] == 3) {
		istate[j] = 0;
	    }
/* L110: */
	}
    }
/*     Define nfree and kactiv. */
/*     Ensure that the number of bounds and general constraints in the */
/*     working set does not exceed n. */
    i__1 = *nctotl;
    for (j = 1; j <= i__1; ++j) {
	if (*nactiv == *nfree) {
	    istate[j] = 0;
	}
	if (istate[j] > 0) {
	    if (j <= *n) {
		--(*nfree);
		if (istate[j] == 1) {
		    wx[j] = bl[j];
		} else if (istate[j] >= 2) {
		    wx[j] = bu[j];
		}
	    } else {
		++(*nactiv);
		kactiv[*nactiv] = j - *n;
	    }
	}
/* L200: */
    }
/*     ------------------------------------------------------------------ */
/*     If a cold start is required,  attempt to add as many */
/*     constraints as possible to the working set. */
/*     ------------------------------------------------------------------ */
    if (s_cmp(start, "cold", (ftnlen)4, (ftnlen)4) == 0) {
/*        See if any bounds are violated or nearly satisfied. */
/*        If so,  add these bounds to the working set and set the */
/*        variables exactly on their bounds. */
	j = *n;
/* +       while (j .ge. 1  .and.  nactiv .lt. nfree) do */
L300:
	if (j >= 1 && *nactiv < *nfree) {
	    if (istate[j] == 0) {
		b1 = bl[j];
		b2 = bu[j];
		is = 0;
		if (b1 > biglow) {
		    if (wx[j] - b1 <= (abs(b1) + 1.) * *tolact) {
			is = 1;
		    }
		}
		if (b2 < bigupp) {
		    if (b2 - wx[j] <= (abs(b2) + 1.) * *tolact) {
			is = 2;
		    }
		}
		if (is > 0) {
		    istate[j] = is;
		    if (is == 1) {
			wx[j] = b1;
		    }
		    if (is == 2) {
			wx[j] = b2;
		    }
		    --(*nfree);
		}
	    }
	    --j;
	    goto L300;
/* +       end while */
	}
/*        --------------------------------------------------------------- */
/*        The following loop finds the linear constraint (if any) with */
/*        smallest residual less than or equal to tolact  and adds it */
/*        to the working set.  This is repeated until the working set */
/*        is complete or all the remaining residuals are too large. */
/*        --------------------------------------------------------------- */
/*        First, compute the residuals for all the constraints not in the */
/*        working set. */
	if (*nclin > 0 && *nactiv < *nfree) {
	    i__1 = *nclin;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (istate[*n + i__] <= 0) {
		    ax[i__] = ddot_(n, &a[i__ + a_dim1], lda, &wx[1], &c__1);
		}
/* L410: */
	    }
	    is = 1;
	    toobig = *tolact + *tolact;
/* +          while (is .gt. 0  .and.  nactiv .lt. nfree) do */
L500:
	    if (is > 0 && *nactiv < *nfree) {
		is = 0;
		resmin = *tolact;
		i__1 = *nclin;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    j = *n + i__;
		    if (istate[j] == 0) {
			b1 = bl[j];
			b2 = bu[j];
			resl = toobig;
			resu = toobig;
			if (b1 > biglow) {
			    resl = (d__1 = ax[i__] - b1, abs(d__1)) / (abs(b1)
				     + 1.);
			}
			if (b2 < bigupp) {
			    resu = (d__1 = ax[i__] - b2, abs(d__1)) / (abs(b2)
				     + 1.);
			}
			residl = min(resl,resu);
			if (residl < resmin) {
			    resmin = residl;
			    imin = i__;
			    is = 1;
			    if (resl > resu) {
				is = 2;
			    }
			}
		    }
/* L520: */
		}
		if (is > 0) {
		    ++(*nactiv);
		    kactiv[*nactiv] = imin;
		    j = *n + imin;
		    istate[j] = is;
		}
		goto L500;
/* +          end while */
	    }
	}
    }
    if (*vertex && *nactiv < *nfree) {
/*        --------------------------------------------------------------- */
/*        Find an initial vertex by temporarily fixing some variables. */
/*        --------------------------------------------------------------- */
/*        Compute lengths of columns of selected linear constraints */
/*        (just the ones corresponding to variables eligible to be */
/*        temporarily fixed). */
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    if (istate[j] == 0) {
		colsiz = 0.;
		i__2 = *nclin;
		for (k = 1; k <= i__2; ++k) {
		    if (istate[*n + k] > 0) {
			colsiz += (d__1 = a[k + j * a_dim1], abs(d__1));
		    }
/* L620: */
		}
		work[j] = colsiz;
	    }
/* L630: */
	}
/*        Find the  nartif  smallest such columns. */
/*        This is an expensive loop.  Later we can replace it by a */
/*        4-pass process (say), accepting the first col that is within */
/*        t  of  colmin, where  t = 0.0, 0.001, 0.01, 0.1 (say). */
/*        (This comment written in 1980). */
/* +       while (nactiv .lt. nfree) do */
L640:
	if (*nactiv < *nfree) {
	    colmin = flmax;
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		if (istate[j] == 0) {
		    if (*nclin == 0) {
			goto L660;
		    }
		    colsiz = work[j];
		    if (colmin > colsiz) {
			colmin = colsiz;
			jmin = j;
		    }
		}
/* L650: */
	    }
	    j = jmin;
/*           Fix x(j) at its current value. */
L660:
	    istate[j] = 4;
	    ++(*nartif);
	    --(*nfree);
	    goto L640;
/* +       end while */
	}
    }
    jfree = 1;
    jfix = *nfree + 1;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	if (istate[j] <= 0) {
	    kx[jfree] = j;
	    ++jfree;
	} else {
	    kx[jfix] = j;
	    ++jfix;
	}
/* L710: */
    }
/*     end of cmcrsh */
    return 0;
} /* cmcrsh_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int cmchzr_(logical *firstv, integer *n, integer *nclin, 
	integer *istate, doublereal *bigalf, doublereal *bigbnd, doublereal *
	pnorm, logical *hitlow, logical *move, logical *onbnd, logical *
	unbndd, doublereal *alfa, doublereal *alfap, integer *jhit, 
	doublereal *anorm, doublereal *ap, doublereal *ax, doublereal *bl, 
	doublereal *bu, doublereal *featol, doublereal *featlu, doublereal *p,
	 doublereal *x)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__, j;
    doublereal alfai, delta;
    integer jhitf;
    doublereal exact;
    integer jhiti;
    doublereal bound;
    integer js;
    logical blockf, blocki;
    doublereal atpabs, atpscd, biglow, bigupp, atpmxf, atpmxi, stepmn, tolpiv,
	     atp, res, atx;

/*     ================================================================== */
/*     cmchzr  finds a step alfa such that the point x + alfa*p reaches */
/*     one of the linear constraints (including bounds). */

/*     In this version of cmchzr, when x is infeasible, the number of */
/*     infeasibilities will never increase.  If the number stays the */
/*     same, the sum of infeasibilities will decrease.  If the number */
/*     decreases by one or more,  the sum of infeasibilities will usually */
/*     decrease also, but occasionally it will increase after the step */
/*     alfa  is taken.  (Convergence is still assured because the number */
/*     has decreased.) */

/*     Three possible steps are computed as follows: */

/*     alfaf = the maximum step that can be taken without violating */
/*              one of the constraints that are currently satisfied. */

/*     alfai = reaches a linear constraint that is currently violated. */
/*              Usually this will be the furthest such constraint along */
/*              p, subject to the angle between the constraint normal and */
/*              p being reasonably close to the maximum value among */
/*              infeasible constraints,  but if firstv = .true. it will */
/*              be the first one along p.  The latter case applies only */
/*              when the problem has been determined to be infeasible, */
/*              and the sum of infeasibilities are being minimized. */
/*              (Alfai is not defined when x is feasible.) */

/*     Alfai is needed occasionally when infeasible, to prevent */
/*     going unnecessarily far when alfaf is quite large.  It will */
/*     always come into effect when x is about to become feasible. */
/*     (The sum of infeasibilities will decrease initially as alfa */
/*     increases from zero, but may start increasing for larger steps. */
/*     Choosing a large alfai allows several elements of  x  to */
/*     become feasible at the same time. */

/*     In the end, we take  alfa = alfaf  if x is feasible, or if */
/*     alfai > alfap (where  alfap  is the perturbed step from pass 1). */
/*     Otherwise,  we take  alfa = alfai. */

/*     Input parameters */
/*     ---------------- */
/*     bigalf defines what should be treated as an unbounded step. */
/*     bigbnd provides insurance for detecting unboundedness. */
/*            If alfa reaches a bound as large as bigbnd, it is */
/*            classed as an unbounded step. */
/*     featol is the array of current feasibility tolerances used by */
/*            cmsinf.  Typically in the range 0.5*tolx to 0.99*tolx, */
/*            where tolx is the featol specified by the user. */
/*     tolinc (in common) is used to determine stepmn (see below), */
/*            the minimum positive step. */
/*     istate is set as follows: */
/*            istate(j) = -2  if a'x .lt. bl - featol */
/*                      = -1  if a'x .gt. bu + featol */
/*                      =  0  if a'x is not in the working set */
/*                      =  1  if a'x is in the working set at bl */
/*                      =  2  if a'x is in the working set at bu */
/*                      =  3  if a'x is in the working set (an equality) */
/*                      =  4  if x(j) is temporarily fixed. */
/*            values -2 and -1 do not occur once feasible. */
/*     bl     the lower bounds on the variables. */
/*     bu     the upper bounds on ditto. */
/*     x      the values of       ditto. */
/*     p      the search direction. */


/*     Output Parameters */
/*     ----------------- */
/*     hitlow  = true  if a lower bound restricted alfa. */
/*             = false otherwise. */
/*     move    = true  if  exact ge stepmn  (defined at end of code). */
/*     onbnd   = true  if  alfa = exact.  This means that the step  alfa */
/*                     moves x  exactly onto one of its constraints, */
/*                     namely  bound. */
/*             = false if the exact step would be too small */
/*                     ( exact .lt. stepmn ). */
/*               (with these definitions,  move = onbnd). */
/*     unbndd  = true  if alfa = bigalf.  Jhit may possibly be zero. */
/*               The parameters hitlow, move, onbnd, bound and exact */
/*               should not be used. */
/*     jhit    = the index (if any) such that constraint jhit reaches */
/*               a bound. */
/*     bound   = the bound value bl(jhit) or bu(jhit) corresponding */
/*               to hitlow. */
/*     exact   = the step that would take constraint jhit exactly onto */
/*               bound. */
/*     alfa    = an allowable, positive step. */
/*               if unbndd is true,  alfa = stepmx. */
/*               otherwise,          alfa = max( stepmn, exact ). */


/*     Cmchzr is based on MINOS 5.2 routine m5chzr, which implements the */
/*     expand procedure to deal with degeneracy. The step alfaf is */
/*     chosen as in the two-pass approach of Paula Harris (1973), except */
/*     that this version insists on returning a positive step, alfa. */
/*     Two features make this possible: */

/*        1. Featol increases slightly each iteration. */

/*        2. The blocking constraint, when added to the working set, */
/*           retains the value Ax(jhit) + alfa * Ap(jhit), */
/*           even if this is not exactly on the blocking bound. */

/*     For infeasible variables moving towards their bound, we require */
/*     the rate of change of the chosen constraint to be at least gamma */
/*     times as large as the biggest available.  This still gives us */
/*     freedom in pass 2. */
/*     gamma = 0.1 and 0.01 seemed to inhibit phase 1 somewhat. */
/*     gamma = 0.001 seems to be safe. */

/*     19 Apr 1988: first version. */
/*     04 Nov 2001: Current version of  cmchzr */
/*     ================================================================== */
/*     tolpiv is a tolerance to exclude negligible elements of a'p. */
    /* Parameter adjustments */
    --x;
    --p;
    --featlu;
    --featol;
    --bu;
    --bl;
    --istate;
    --anorm;
    --ap;
    --ax;

    /* Function Body */
    biglow = -(*bigbnd);
    bigupp = *bigbnd;
    tolpiv = sol4cm_1.epspt3 * sol4cm_1.epspt3 * *pnorm;
/*     ------------------------------------------------------------------ */
/*     First pass -- find steps to perturbed constraints, so that */
/*     alfap will be slightly larger than the true step. */
/*     In degenerate cases, this strategy gives us some freedom in the */
/*     second pass.  The general idea follows that described by P.M.J. */
/*     Harris, p.21 of Mathematical Programming 5, 1 (1973), 1--28. */
/*     ------------------------------------------------------------------ */
    atpmxi = 0.;
    *alfap = *bigalf;
    jhitf = 0;
    i__1 = *n + *nclin;
    for (j = 1; j <= i__1; ++j) {
	js = istate[j];
	if (js <= 0) {
	    delta = featol[j];
	    if (j <= *n) {
		atx = x[j];
		atp = p[j];
		atpabs = abs(atp);
		atpscd = atpabs;
	    } else {
		i__ = j - *n;
		atx = ax[i__];
		atp = ap[i__];
		atpabs = abs(atp);
		atpscd = atpabs / (anorm[i__] + 1.);
	    }
	    if (atpscd <= tolpiv) {
/*              --------------------------------------------------------- */
/*              This constraint appears to be constant along p.  It is */
/*              not used to compute the step.  Give the residual a value */
/*              that can be spotted in the debug output. */
/*              --------------------------------------------------------- */
		res = -1.;
	    } else if (atp <= 0. && js != -2) {
/*              --------------------------------------------------------- */
/*              a'x  is decreasing and the lower bound is not violated. */
/*              --------------------------------------------------------- */
/*              Test for smaller alfap. */
/*              If the upper bound is violated. Test for bigger atp. */
		if (bl[j] > biglow) {
		    res = atx - bl[j] + delta;
		    if (res < *alfap * atpabs) {
			*alfap = res / atpabs;
			jhitf = j;
		    }
		}
		if (js == -1) {
		    atpmxi = max(atpmxi,atpscd);
		}
	    } else if (atp > 0. && js != -1) {
/*              --------------------------------------------------------- */
/*              a'x  is increasing and the upper bound is not violated. */
/*              --------------------------------------------------------- */
/*              Test for smaller alfap. */
/*              If the lower bound is violated. Test for bigger atp. */
		if (bu[j] < bigupp) {
		    res = bu[j] - atx + delta;
		    if (res < *alfap * atp) {
			*alfap = res / atp;
			jhitf = j;
		    }
		}
		if (js == -2) {
		    atpmxi = max(atpmxi,atpscd);
		}
	    }
	}
/* L200: */
    }
/*     ------------------------------------------------------------------ */
/*     Second pass. */
/*     For feasible variables, recompute steps without perturbation. */
/*     amongst constraints that are closer than alfap, choose the one */
/*     That makes the largest angle with the search direction. */
/*     For infeasible variables, find the largest step subject to a'p */
/*     being no smaller than gamma * max(a'p). */
/*     ------------------------------------------------------------------ */
    if (*firstv) {
	alfai = *bigalf;
    } else {
	alfai = 0.;
    }
    atpmxf = 0.;
    atpmxi *= .001;
    jhiti = 0;
    i__1 = *n + *nclin;
    for (j = 1; j <= i__1; ++j) {
	js = istate[j];
	if (js <= 0) {
	    if (j <= *n) {
		atx = x[j];
		atp = p[j];
		atpabs = abs(atp);
		atpscd = atpabs;
	    } else {
		i__ = j - *n;
		atx = ax[i__];
		atp = ap[i__];
		atpabs = abs(atp);
		atpscd = atpabs / (anorm[i__] + 1.);
	    }
	    if (atpscd <= tolpiv) {
/*              --------------------------------------------------------- */
/*              This constraint appears to be constant along p.  It is */
/*              not used to compute the step.  Give the residual a value */
/*              that can be spotted in the debug output. */
/*              --------------------------------------------------------- */
		res = -1.;
	    } else if (atp <= 0. && js != -2) {
/*              --------------------------------------------------------- */
/*              a'x  is decreasing. */
/*              --------------------------------------------------------- */
/*              Test for bigger a'p if the lower bound is satisfied. */
/*              Test for smaller alfaf. */
		if (atpscd > atpmxf) {
		    if (bl[j] > biglow) {
			res = atx - bl[j];
			if (res <= *alfap * atpabs) {
			    atpmxf = atpscd;
			    jhitf = j;
			}
		    }
		}
		if (js == -1) {
/*                 The upper bound is violated. */
/*                 Test for bigger or smaller alfai,  depending on the */
/*                 value of firstv. */
		    if (*firstv) {
			res = atx - bu[j];
			if (res <= alfai * atpabs) {
			    alfai = res / atpabs;
			    jhiti = j;
			}
		    } else if (atpscd >= atpmxi) {
			res = atx - bu[j];
			if (res > alfai * atpabs) {
			    alfai = res / atpabs;
			    jhiti = j;
			}
		    }
		}
	    } else if (atp > 0. && js != -1) {
/*              --------------------------------------------------------- */
/*              a'x  is increasing and the upper bound is not violated. */
/*              --------------------------------------------------------- */
/*              Test for smaller alfap. */
		if (atpscd > atpmxf) {
		    if (bu[j] < bigupp) {
			res = bu[j] - atx;
			if (res <= *alfap * atp) {
			    atpmxf = atpscd;
			    jhitf = j;
			}
		    }
		}
		if (js == -2) {
/*                 The lower bound is violated. */
/*                 Test for bigger or smaller alfai,  depending on the */
/*                 value of firstv. */
		    if (*firstv) {
			res = bl[j] - atx;
			if (res <= alfai * atp) {
			    alfai = res / atp;
			    jhiti = j;
			}
		    } else if (atpscd >= atpmxi) {
			res = bl[j] - atx;
			if (res > alfai * atp) {
			    alfai = res / atp;
			    jhiti = j;
			}
		    }
		}
	    }
	}
/* L300: */
    }
/*     ------------------------------------------------------------------ */
/*     See if a feasible and/or infeasible constraint blocks. */
/*     ------------------------------------------------------------------ */
    blockf = jhitf > 0;
    blocki = jhiti > 0;
    *unbndd = ! (blockf || blocki);
    if (*unbndd) {
	goto L900;
    }
    if (blockf) {
/*        --------------------------------------------------------------- */
/*        A constraint is hit which is currently feasible. */
/*        The corresponding step alfaf is not used, so no need to get it, */
/*        but we know that alfaf .le. alfap, the step from pass 1. */
/*        --------------------------------------------------------------- */
	*jhit = jhitf;
	if (*jhit <= *n) {
	    atp = p[*jhit];
	} else {
	    atp = ap[*jhit - *n];
	}
	*hitlow = atp < 0.;
    }
/*     If there is a choice between alfaf and alfai, it is probably best */
/*     to take alfai.  However, we can't if alfai is bigger than alfap. */
    if (blocki && alfai <= *alfap) {
/*        --------------------------------------------------------------- */
/*        An infeasible variable reaches its violated bound. */
/*        --------------------------------------------------------------- */
	*jhit = jhiti;
	if (*jhit <= *n) {
	    atp = p[*jhit];
	} else {
	    atp = ap[*jhit - *n];
	}
	*hitlow = atp > 0.;
    }
    if (*jhit <= *n) {
	atx = x[*jhit];
    } else {
	atx = ax[*jhit - *n];
    }
/*     ------------------------------------------------------------------ */
/*     Try to step exactly onto bound, but make sure the exact step */
/*     is sufficiently positive.  (Exact will be alfaf or alfai.) */
/*     Since featol increases by  tolinc  each iteration, we know that */
/*     a step as large as  stepmn  (below) will not cause any feasible */
/*     variables to become infeasible (where feasibility is measured */
/*     by the current featol). */
/*     ------------------------------------------------------------------ */
    if (*hitlow) {
	bound = bl[*jhit];
    } else {
	bound = bu[*jhit];
    }
    *unbndd = abs(bound) >= *bigbnd;
    if (*unbndd) {
	goto L900;
    }
    stepmn = sol3lc_1.tolinc * featlu[*jhit] / abs(atp);
    exact = (bound - atx) / atp;
    *alfa = max(stepmn,exact);
    *onbnd = *alfa == exact;
    *move = exact >= stepmn;
    if (! (*move)) {
	++sol3lc_1.ndegen;
    }
    return 0;
/*     ------------------------------------------------------------------ */
/*     Unbounded. */
/*     ------------------------------------------------------------------ */
L900:
    *alfa = *bigalf;
    *move = TRUE_;
    *onbnd = FALSE_;
/*     end of  cmchzr. */
    return 0;
} /* cmchzr_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int cmdgen_(char *job, integer *msglvl, integer *n, integer *
	nclin, integer *nmoved, integer *iter, integer *numinf, integer *
	istate, doublereal *bl, doublereal *bu, doublereal *featol, 
	doublereal *featlu, doublereal *x, ftnlen job_len)
{
    /* Format strings */
    static char fmt_1000[] = "(\002 Itn\002,i6,\002 --\002,i7,\002  variable"
	    "s moved to their bounds.\002)";

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static doublereal tolz;
    doublereal d__;
    static doublereal tolx1;
    integer j, is;
    doublereal epsmch;
    integer maxfix;

    /* Fortran I/O blocks */
    static cilist io___64 = { 0, 0, 0, fmt_1000, 0 };
    static cilist io___65 = { 0, 0, 0, fmt_1000, 0 };


/*     ================================================================== */
/*     cmdgen performs most of the manoeuvres associated with degeneracy. */
/*     the degeneracy-resolving strategy operates in the following way. */

/*     Over a cycle of iterations, the feasibility tolerance featol */
/*     increases slightly (from tolx0 to tolx1 in steps of tolinc). */
/*     this ensures that all steps taken will be positive. */

/*     After kdegen consecutive iterations, variables within */
/*     featol of their bounds are set exactly on their bounds and x is */
/*     recomputed to satisfy the general constraints in the working set. */
/*     Featol is then reduced to tolx0 for the next cycle of iterations. */

/*     featlu  is the array of user-supplied feasibility tolerances. */
/*     featol  is the array of current feasibility tolerances. */

/*     If job = 'i', cmdgen initializes the parameters in */
/*     common block sol3lc: */

/*     tolx0   is the minimum (scaled) feasibility tolerance. */
/*     tolx1   is the maximum (scaled) feasibility tolerance. */
/*     tolinc  is the scaled increment to the current featol. */
/*     idegen  is the expand frequency. It is the frequency of resetting */
/*             featol to (scaled) tolx0. */
/*     kdegen  (specified by the user) is the initial value of idegen. */
/*     ndegen  counts the number of degenerate steps (incremented */
/*             by cmchzr). */
/*     itnfix  is the last iteration at which a job 'e' or 'o' entry */
/*             caused an x to be put on a constraint. */
/*     nfix(j) counts the number of times a job 'o' entry has */
/*             caused the variables to be placed on the working set, */
/*             where j=1 if infeasible, j=2 if feasible. */

/*     tolx0*featlu and tolx1*featlu are both close to the feasibility */
/*     tolerance featlu specified by the user.  (They must both be less */
/*     than featlu.) */


/*     If job = 'e',  cmdgen has been called after a cycle of kdegen */
/*     iterations.  Constraints in the working set are examined to see if */
/*     any are off their bounds by an amount approaching featol.  Nmoved */
/*     returns how many.  If nmoved is positive,  x  is moved onto the */
/*     constraints in the working set.  It is assumed that the calling */
/*     routine will then continue iterations. */


/*     If job = 'o',  cmdgen is being called after a subproblem has been */
/*     judged optimal, infeasible or unbounded.  Constraint violations */
/*     are examined as above. */

/*     Cmdgen is based on */

/*     19-Apr-1988. Original version based on MINOS routine m5dgen. */
/*     09-Apr-1994. Expand frequency allowed to expand. This allows */
/*                  small initial values of kdegen. */
/*     28-Jul-1994. Current version. */
/*     ================================================================== */
    /* Parameter adjustments */
    --x;
    --featlu;
    --featol;
    --bu;
    --bl;
    --istate;

    /* Function Body */
    *nmoved = 0;
    if (*(unsigned char *)job == 'i' || *(unsigned char *)job == 'I') {
/*        --------------------------------------------------------------- */
/*        Job = 'Initialize'. */
/*        Initialize at the start of each linear problem. */
/*        kdegen  is the expand frequency      and */
/*        featlu  are the user-supplied feasibility tolerances. */
/*        They are not changed. */
/*        --------------------------------------------------------------- */
	epsmch = solmch_1.wmach[2];
	sol3lc_1.ndegen = 0;
	sol3lc_1.itnfix = 0;
	sol3lc_1.nfix[0] = 0;
	sol3lc_1.nfix[1] = 0;
	sol3lc_1.tolx0 = .5;
	tolx1 = .99;
	tolz = pow_dd(&epsmch, &c_b59);
	sol3lc_1.idegen = sol3lc_1.kdegen;
	if (sol3lc_1.kdegen < 9999999) {
	    sol3lc_1.tolinc = (tolx1 - sol3lc_1.tolx0) / sol3lc_1.idegen;
	} else {
	    sol3lc_1.tolinc = 0.;
	}
	i__1 = *n + *nclin;
	for (j = 1; j <= i__1; ++j) {
	    featol[j] = sol3lc_1.tolx0 * featlu[j];
/* L100: */
	}
    } else {
/*        --------------------------------------------------------------- */
/*        Job = 'end of cycle' or 'optimal'. */
/*        Initialize local variables maxfix and tolz. */
/*        --------------------------------------------------------------- */
	maxfix = 2;
	if (*(unsigned char *)job == 'o' || *(unsigned char *)job == 'O') {
/*           ------------------------------------------------------------ */
/*           Job = 'optimal'. */
/*           Return with nmoved = 0 if the last call was at the same */
/*           iteration,  or if there have already been maxfix calls with */
/*           the same state of feasibility. */
/*           ------------------------------------------------------------ */
	    if (sol3lc_1.itnfix == *iter) {
		return 0;
	    }
	    if (*numinf > 0) {
		j = 1;
	    } else {
		j = 2;
	    }
	    if (sol3lc_1.nfix[j - 1] >= maxfix) {
		return 0;
	    }
	    ++sol3lc_1.nfix[j - 1];
	}
/*        Increase the expand frequency. */
/*        Reset featol to its minimum value. */
	sol3lc_1.idegen += 10;
	if (sol3lc_1.kdegen < 9999999) {
	    sol3lc_1.tolinc = (tolx1 - sol3lc_1.tolx0) / sol3lc_1.idegen;
	    sol3lc_1.idegen += *iter;
	} else {
	    sol3lc_1.tolinc = 0.;
	}
	i__1 = *n + *nclin;
	for (j = 1; j <= i__1; ++j) {
	    featol[j] = sol3lc_1.tolx0 * featlu[j];
/* L250: */
	}
/*        Count the number of times a variable is moved a nontrivial */
/*        distance onto its bound. */
	sol3lc_1.itnfix = *iter;
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    is = istate[j];
	    if (is > 0 && is < 4) {
		if (is == 1) {
		    d__ = (d__1 = x[j] - bl[j], abs(d__1));
		} else {
		    d__ = (d__1 = x[j] - bu[j], abs(d__1));
		}
		if (d__ > tolz) {
		    ++(*nmoved);
		}
	    }
/* L300: */
	}
	if (*nmoved > 0) {
/*           Some variables were moved onto their bounds. */
	    if (sol1cm_1.iprint > 0) {
		io___64.ciunit = sol1cm_1.iprint;
		s_wsfe(&io___64);
		do_fio(&c__1, (char *)&(*iter), (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&(*nmoved), (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	    if (sol1cm_1.isumm > 0) {
		io___65.ciunit = sol1cm_1.isumm;
		s_wsfe(&io___65);
		do_fio(&c__1, (char *)&(*iter), (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&(*nmoved), (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
    }
/*     end of cmdgen */
    return 0;
} /* cmdgen_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int cmfeas_(integer *n, integer *nclin, integer *istate, 
	doublereal *bigbnd, integer *nviol, integer *jmax, doublereal *errmax,
	 doublereal *ax, doublereal *bl, doublereal *bu, doublereal *featol, 
	doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    integer j;
    doublereal feasj;
    integer is;
    doublereal biglow, bigupp, con, res;

/*     ================================================================== */
/*     cmfeas  checks the residuals of the constraints that are believed */
/*     to be feasible.  The number of constraints violated by more than */
/*     featol is computed, along with the maximum constraint violation. */

/*     Original version written by PEG,   April    1984. */
/*     This version of  cmfeas  dated  30-Jun-1988. */
/*     ================================================================== */
    /* Parameter adjustments */
    --x;
    --featol;
    --bu;
    --bl;
    --istate;
    --ax;

    /* Function Body */
    biglow = -(*bigbnd);
    bigupp = *bigbnd;
/*     ================================================================== */
/*     Compute the number of constraints (nviol) violated by more than */
/*     featol and  the maximum constraint violation (errmax). */
/*     (The residual of a constraint in the working set is treated as if */
/*     it were an equality constraint fixed at that bound.) */
/*     ================================================================== */
    *nviol = 0;
    *jmax = 0;
    *errmax = 0.;
    i__1 = *n + *nclin;
    for (j = 1; j <= i__1; ++j) {
	is = istate[j];
	if (is >= 0) {
	    feasj = featol[j];
	    if (j <= *n) {
		con = x[j];
	    } else {
		con = ax[j - *n];
	    }
/*           Check for constraint violations. */
	    if (bl[j] > biglow) {
		res = bl[j] - con;
		if (res > feasj) {
		    ++(*nviol);
		    goto L190;
		}
	    }
	    if (bu[j] < bigupp) {
		res = bu[j] - con;
		if (res < -feasj) {
		    ++(*nviol);
		    res = -res;
		    goto L190;
		}
	    }
/*           This constraint is satisfied,  but count a large residual */
/*           as a violation if the constraint is in the working set. */
	    res = 0.;
	    if (is == 1) {
		res = (d__1 = bl[j] - con, abs(d__1));
	    } else if (is == 2) {
		res = (d__1 = bu[j] - con, abs(d__1));
	    } else if (is == 3) {
		res = (d__1 = bu[j] - con, abs(d__1));
	    }
	    if (res > feasj) {
		++(*nviol);
	    }
L190:
	    if (res > *errmax) {
		*jmax = j;
		*errmax = res;
	    }
	}
/* L200: */
    }
/*     end of cmfeas */
    return 0;
} /* cmfeas_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int cminit_(integer *nerror, integer *msglvl, char *start, 
	integer *liwork, integer *lwork, integer *litotl, integer *lwtotl, 
	integer *n, integer *nclin, integer *ncnln, integer *istate, logical *
	named, char *names, doublereal *bigbnd, doublereal *bl, doublereal *
	bu, doublereal *clamda, doublereal *x, ftnlen start_len, ftnlen 
	names_len)
{
    /* Initialized data */

    static char id[5*3] = "varbl" "lncon" "nlcon";

    /* Format strings */
    static char fmt_1200[] = "(/\002 XXX  nclin  is out of range...\002,i10)";
    static char fmt_1210[] = "(/\002 XXX  ncnln  is out of range...\002,i10)";
    static char fmt_1100[] = "(/\002 Workspace provided is     iw(\002,i8"
	    ",\002),  w(\002,i8,\002).\002/\002 To solve problem we need  iw"
	    "(\002,i8,\002),  w(\002,i8,\002).\002)";
    static char fmt_1110[] = "(/\002 XXX  Not enough workspace to solve prob"
	    "lem.\002)";
    static char fmt_1310[] = "(/\002 XXX  The equal bounds on  \002,a16,\002"
	    "  are infinite.   Bounds =\002,g16.7,\002  bigbnd =\002,g16.7)";
    static char fmt_1315[] = "(/\002 XXX  The bounds on  \002,a16,\002  are "
	    "inconsistent.   bl =\002,g16.7,\002   bu =\002,g16.7)";
    static char fmt_1300[] = "(/\002 XXX  The equal bounds on  \002,a5,i3"
	    ",\002  are infinite.   Bounds =\002,g16.7,\002  bigbnd =\002,g16"
	    ".7)";
    static char fmt_1305[] = "(/\002 XXX  The bounds on  \002,a5,i3,\002  ar"
	    "e inconsistent.   bl =\002,g16.7,\002   bu =\002,g16.7)";
    static char fmt_1500[] = "(/\002 XXX  Component\002,i5,\002  of  istate "
	    " is out of\002,\002 range...\002,i10)";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    doublereal cmul;
    integer i__, j, k, l;
    doublereal b1, b2;
    logical ok;
    integer is;

    /* Fortran I/O blocks */
    static cilist io___74 = { 0, 0, 0, fmt_1200, 0 };
    static cilist io___75 = { 0, 0, 0, fmt_1200, 0 };
    static cilist io___76 = { 0, 0, 0, fmt_1210, 0 };
    static cilist io___77 = { 0, 0, 0, fmt_1210, 0 };
    static cilist io___79 = { 0, 0, 0, fmt_1100, 0 };
    static cilist io___80 = { 0, 0, 0, fmt_1110, 0 };
    static cilist io___81 = { 0, 0, 0, fmt_1100, 0 };
    static cilist io___82 = { 0, 0, 0, fmt_1110, 0 };
    static cilist io___83 = { 0, 0, 0, fmt_1100, 0 };
    static cilist io___89 = { 0, 0, 0, fmt_1310, 0 };
    static cilist io___90 = { 0, 0, 0, fmt_1310, 0 };
    static cilist io___91 = { 0, 0, 0, fmt_1315, 0 };
    static cilist io___92 = { 0, 0, 0, fmt_1315, 0 };
    static cilist io___93 = { 0, 0, 0, fmt_1300, 0 };
    static cilist io___94 = { 0, 0, 0, fmt_1300, 0 };
    static cilist io___95 = { 0, 0, 0, fmt_1305, 0 };
    static cilist io___96 = { 0, 0, 0, fmt_1305, 0 };
    static cilist io___98 = { 0, 0, 0, fmt_1500, 0 };
    static cilist io___99 = { 0, 0, 0, fmt_1500, 0 };


/*     ================================================================== */
/*     cminit   checks the data. */

/*     First version written by PEG,  15-Nov-1990. */
/*     This version of cminit dated   13-Jul-94. */
/*     ================================================================== */
    /* Parameter adjustments */
    --x;
    --clamda;
    --bu;
    --bl;
    --istate;
    names -= 16;

    /* Function Body */
    *nerror = 0;
/*     ------------------------------------------------------------------ */
/*     Check nclin and ncnln. */
/*     ------------------------------------------------------------------ */
    if (*nclin < 0 || *ncnln < 0) {
	if (*nclin < 0) {
	    ++(*nerror);
	    if (sol1cm_1.iprint > 0) {
		io___74.ciunit = sol1cm_1.iprint;
		s_wsfe(&io___74);
		do_fio(&c__1, (char *)&(*nclin), (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	    if (sol1cm_1.isumm > 0) {
		io___75.ciunit = sol1cm_1.isumm;
		s_wsfe(&io___75);
		do_fio(&c__1, (char *)&(*nclin), (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
	if (*ncnln < 0) {
	    ++(*nerror);
	    if (sol1cm_1.iprint > 0) {
		io___76.ciunit = sol1cm_1.iprint;
		s_wsfe(&io___76);
		do_fio(&c__1, (char *)&(*ncnln), (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	    if (sol1cm_1.isumm > 0) {
		io___77.ciunit = sol1cm_1.isumm;
		s_wsfe(&io___77);
		do_fio(&c__1, (char *)&(*ncnln), (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
	return 0;
    }
/*     ------------------------------------------------------------------ */
/*     Check that there is enough workspace to solve the problem. */
/*     ------------------------------------------------------------------ */
    ok = *litotl <= *liwork && *lwtotl <= *lwork;
    if (! ok) {
	++(*nerror);
	if (sol1cm_1.iprint > 0) {
	    io___79.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___79);
	    do_fio(&c__1, (char *)&(*liwork), (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&(*lwork), (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&(*litotl), (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&(*lwtotl), (ftnlen)sizeof(integer));
	    e_wsfe();
	    io___80.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___80);
	    e_wsfe();
	}
	if (sol1cm_1.isumm > 0) {
	    io___81.ciunit = sol1cm_1.isumm;
	    s_wsfe(&io___81);
	    do_fio(&c__1, (char *)&(*liwork), (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&(*lwork), (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&(*litotl), (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&(*lwtotl), (ftnlen)sizeof(integer));
	    e_wsfe();
	    io___82.ciunit = sol1cm_1.isumm;
	    s_wsfe(&io___82);
	    e_wsfe();
	}
    } else if (*msglvl > 0) {
	if (sol1cm_1.iprint > 0) {
	    io___83.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___83);
	    do_fio(&c__1, (char *)&(*liwork), (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&(*lwork), (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&(*litotl), (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&(*lwtotl), (ftnlen)sizeof(integer));
	    e_wsfe();
	}
    }
/*     ------------------------------------------------------------------ */
/*     Check the bounds on all variables and constraints. */
/*     ------------------------------------------------------------------ */
    i__1 = *n + *nclin + *ncnln;
    for (j = 1; j <= i__1; ++j) {
	b1 = bl[j];
	b2 = bu[j];
	ok = b1 < b2 || b1 == b2 && abs(b1) < *bigbnd;
	if (! ok) {
	    ++(*nerror);
	    if (j > *n + *nclin) {
		k = j - *n - *nclin;
		l = 3;
	    } else if (j > *n) {
		k = j - *n;
		l = 2;
	    } else {
		k = j;
		l = 1;
	    }
	    if (*named) {
		if (b1 == b2) {
		    if (sol1cm_1.iprint > 0) {
			io___89.ciunit = sol1cm_1.iprint;
			s_wsfe(&io___89);
			do_fio(&c__1, names + (j << 4), (ftnlen)16);
			do_fio(&c__1, (char *)&b1, (ftnlen)sizeof(doublereal))
				;
			do_fio(&c__1, (char *)&(*bigbnd), (ftnlen)sizeof(
				doublereal));
			e_wsfe();
		    }
		    if (sol1cm_1.isumm > 0) {
			io___90.ciunit = sol1cm_1.isumm;
			s_wsfe(&io___90);
			do_fio(&c__1, names + (j << 4), (ftnlen)16);
			do_fio(&c__1, (char *)&b1, (ftnlen)sizeof(doublereal))
				;
			do_fio(&c__1, (char *)&(*bigbnd), (ftnlen)sizeof(
				doublereal));
			e_wsfe();
		    }
		} else {
		    if (sol1cm_1.iprint > 0) {
			io___91.ciunit = sol1cm_1.iprint;
			s_wsfe(&io___91);
			do_fio(&c__1, names + (j << 4), (ftnlen)16);
			do_fio(&c__1, (char *)&b1, (ftnlen)sizeof(doublereal))
				;
			do_fio(&c__1, (char *)&b2, (ftnlen)sizeof(doublereal))
				;
			e_wsfe();
		    }
		    if (sol1cm_1.isumm > 0) {
			io___92.ciunit = sol1cm_1.isumm;
			s_wsfe(&io___92);
			do_fio(&c__1, names + (j << 4), (ftnlen)16);
			do_fio(&c__1, (char *)&b1, (ftnlen)sizeof(doublereal))
				;
			do_fio(&c__1, (char *)&b2, (ftnlen)sizeof(doublereal))
				;
			e_wsfe();
		    }
		}
	    } else {
		if (b1 == b2) {
		    if (sol1cm_1.iprint > 0) {
			io___93.ciunit = sol1cm_1.iprint;
			s_wsfe(&io___93);
			do_fio(&c__1, id + (l - 1) * 5, (ftnlen)5);
			do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&b1, (ftnlen)sizeof(doublereal))
				;
			do_fio(&c__1, (char *)&(*bigbnd), (ftnlen)sizeof(
				doublereal));
			e_wsfe();
		    }
		    if (sol1cm_1.isumm > 0) {
			io___94.ciunit = sol1cm_1.isumm;
			s_wsfe(&io___94);
			do_fio(&c__1, id + (l - 1) * 5, (ftnlen)5);
			do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&b1, (ftnlen)sizeof(doublereal))
				;
			do_fio(&c__1, (char *)&(*bigbnd), (ftnlen)sizeof(
				doublereal));
			e_wsfe();
		    }
		} else {
		    if (sol1cm_1.iprint > 0) {
			io___95.ciunit = sol1cm_1.iprint;
			s_wsfe(&io___95);
			do_fio(&c__1, id + (l - 1) * 5, (ftnlen)5);
			do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&b1, (ftnlen)sizeof(doublereal))
				;
			do_fio(&c__1, (char *)&b2, (ftnlen)sizeof(doublereal))
				;
			e_wsfe();
		    }
		    if (sol1cm_1.isumm > 0) {
			io___96.ciunit = sol1cm_1.isumm;
			s_wsfe(&io___96);
			do_fio(&c__1, id + (l - 1) * 5, (ftnlen)5);
			do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&b1, (ftnlen)sizeof(doublereal))
				;
			do_fio(&c__1, (char *)&b2, (ftnlen)sizeof(doublereal))
				;
			e_wsfe();
		    }
		}
	    }
	}
/* L200: */
    }
/*     ------------------------------------------------------------------ */
/*     Check  istate. */
/*     ------------------------------------------------------------------ */
    if (s_cmp(start, "warm", (ftnlen)4, (ftnlen)4) == 0 || s_cmp(start, "hot "
	    , (ftnlen)4, (ftnlen)4) == 0) {
	i__1 = *n + *nclin + *ncnln;
	for (j = 1; j <= i__1; ++j) {
	    is = istate[j];
	    ok = is >= -2 && is <= 4;
	    if (! ok) {
		++(*nerror);
		if (sol1cm_1.iprint > 0) {
		    io___98.ciunit = sol1cm_1.iprint;
		    s_wsfe(&io___98);
		    do_fio(&c__1, (char *)&j, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&is, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
		if (sol1cm_1.isumm > 0) {
		    io___99.ciunit = sol1cm_1.isumm;
		    s_wsfe(&io___99);
		    do_fio(&c__1, (char *)&j, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&is, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    }
/* L420: */
	}
	if (*nerror == 0) {
	    i__1 = *ncnln;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		j = *n + *nclin + i__;
		is = istate[j];
		cmul = clamda[j];
		if (is == 0) {
		    cmul = 0.;
		} else if (is == 1) {
		    if (bl[j] <= -(*bigbnd)) {
			is = 0;
		    }
		    if (cmul < 0. || is == 0) {
			cmul = 0.;
		    }
		} else if (is == 2) {
		    if (bu[j] >= *bigbnd) {
			is = 0;
		    }
		    if (cmul > 0. || is == 0) {
			cmul = 0.;
		    }
		} else if (is == 3) {
		    if (bl[j] < bu[j]) {
			is = 0;
		    }
		}
		istate[j] = is;
		clamda[j] = cmul;
/* L430: */
	    }
	}
    }
    return 0;
/*     end of cminit */
} /* cminit_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int cmmsg1_(char *subr, char *msg, doublereal *v, integer *
	lenv, ftnlen subr_len, ftnlen msg_len)
{
    /* Format strings */
    static char fmt_1000[] = "(/\002 //\002,a6,\002//  \002,a)";
    static char fmt_1100[] = "(/\002 //\002,a6,\002//  \002,a,\002 ... \002/"
	    "(1p,5e15.5))";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    integer i__;

    /* Fortran I/O blocks */
    static cilist io___102 = { 0, 0, 0, fmt_1000, 0 };
    static cilist io___103 = { 0, 0, 0, fmt_1100, 0 };


/*     ================================================================== */
/*     cmmsg1  prints the array v in debug format. */

/*     Original version dated 17-Jul-1987. */
/*     This version of  cmmsg1  dated  31-Jan-1988. */
/*     ================================================================== */
    /* Parameter adjustments */
    --v;

    /* Function Body */
    if (*lenv <= 0) {
	io___102.ciunit = sol1cm_1.iprint;
	s_wsfe(&io___102);
	do_fio(&c__1, subr, (ftnlen)6);
	do_fio(&c__1, msg, msg_len);
	e_wsfe();
    } else {
	io___103.ciunit = sol1cm_1.iprint;
	s_wsfe(&io___103);
	do_fio(&c__1, subr, (ftnlen)6);
	do_fio(&c__1, msg, msg_len);
	i__1 = *lenv;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_fio(&c__1, (char *)&v[i__], (ftnlen)sizeof(doublereal));
	}
	e_wsfe();
    }
    return 0;
/*     end of cmmsg1 */
} /* cmmsg1_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int cmmul1_(char *prbtyp, integer *msglvl, integer *n, 
	integer *lda, integer *ldt, integer *nactiv, integer *nfree, integer *
	nz, integer *istate, integer *kactiv, integer *kx, doublereal *zerolm,
	 integer *notopt, integer *numinf, doublereal *trusml, doublereal *
	smllst, integer *jsmlst, integer *ksmlst, doublereal *tinyst, integer 
	*jtiny, integer *jinf, doublereal *trubig, doublereal *biggst, 
	integer *jbigst, integer *kbigst, doublereal *a, doublereal *anorms, 
	doublereal *gq, doublereal *rlamda, doublereal *t, doublereal *wtinf, 
	ftnlen prbtyp_len)
{
    /* Format strings */
    static char fmt_1100[] = "(/\002 Multipliers for the \002,a2,\002 bound "
	    " constraints   \002/4(i5,1pe11.2))";
    static char fmt_1200[] = "(/\002 Multipliers for the \002,a2,\002 linear"
	    " constraints   \002/4(i5,1pe11.2))";

    /* System generated locals */
    integer a_dim1, a_offset, t_dim1, t_offset, i__1, i__2;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    doublereal blam, rlam;
    integer i__, j, k, l;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *), dtrsv_(char *, char *, char *, integer *
	    , doublereal *, integer *, doublereal *, integer *, ftnlen, 
	    ftnlen, ftnlen);
    integer is;
    doublereal scdlam;
    integer nfixed;
    doublereal anormj;

    /* Fortran I/O blocks */
    static cilist io___115 = { 0, 0, 0, fmt_1100, 0 };
    static cilist io___116 = { 0, 0, 0, fmt_1200, 0 };


/*     ================================================================== */
/*     cmmul1  first computes the Lagrange multiplier estimates for the */
/*     given working set.  It then determines the values and indices of */
/*     certain significant multipliers.  In this process, the multipliers */
/*     for inequalities at their upper bounds are adjusted so that a */
/*     negative multiplier for an inequality constraint indicates non- */
/*     optimality.  All adjusted multipliers are scaled by the 2-norm */
/*     of the associated constraint row.  In the following, the term */
/*     minimum refers to the ordering of numbers on the real line,  and */
/*     not to their magnitude. */

/*     jsmlst          is the index of the constraint whose multiplier is */
/*                     the minimum of the set of adjusted multipliers */
/*                     with values less than  small. */
/*     rlamda(ksmlst)  is the associated multiplier. */

/*     jbigst          is the index of the constraint whose multiplier is */
/*                     the largest of the set of adjusted multipliers with */
/*                     values greater than (1 + small). */
/*     rlamda(kbigst)  is the associated multiplier. */

/*     On exit,  elements  1  thru  nactiv  of  rlamda  contain the */
/*     unadjusted multipliers for the general constraints.  Elements */
/*     nactiv  onwards of  rlamda  contain the unadjusted multipliers */
/*     for the bounds. */

/*     Original version written 31-October-1984. */
/*     Based on a version of  lsmuls  dated 30-June-1986. */
/*     This version of  cmmul1  dated 14-Sep-92. */
/*     ================================================================== */
    /* Parameter adjustments */
    --rlamda;
    --gq;
    --kx;
    --kactiv;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    t_dim1 = *ldt;
    t_offset = t_dim1 + 1;
    t -= t_offset;
    --istate;
    --anorms;
    --wtinf;

    /* Function Body */
    nfixed = *n - *nfree;
    *jtiny = 0;
    *jsmlst = 0;
    *ksmlst = 0;
    *jbigst = 0;
    *kbigst = 0;
/*     ------------------------------------------------------------------ */
/*     Compute  jsmlst  for regular constraints and temporary bounds. */
/*     ------------------------------------------------------------------ */
/*     First, compute the Lagrange multipliers for the general */
/*     constraints in the working set, by solving  T'*lamda = Y'g. */
    if (*n > *nz) {
	i__1 = *n - *nz;
	dcopy_(&i__1, &gq[*nz + 1], &c__1, &rlamda[1], &c__1);
    }
    if (*nactiv > 0) {
	dtrsv_("U", "T", "N", nactiv, &t[(*nz + 1) * t_dim1 + 1], ldt, &
		rlamda[1], &c__1, (ftnlen)1, (ftnlen)1, (ftnlen)1);
    }
/*     ----------------------------------------------------------------- */
/*     Now set elements  nactiv, nactiv+1,... of  rlamda  equal to */
/*     the multipliers for the bound constraints. */
/*     ----------------------------------------------------------------- */
    i__1 = nfixed;
    for (l = 1; l <= i__1; ++l) {
	j = kx[*nfree + l];
	blam = rlamda[*nactiv + l];
	i__2 = *nactiv;
	for (k = 1; k <= i__2; ++k) {
	    i__ = kactiv[k];
	    blam -= a[i__ + j * a_dim1] * rlamda[*nactiv - k + 1];
/* L170: */
	}
	rlamda[*nactiv + l] = blam;
/* L190: */
    }
/*     ----------------------------------------------------------------- */
/*     Find  jsmlst  and  ksmlst. */
/*     ----------------------------------------------------------------- */
    i__1 = *n - *nz;
    for (k = 1; k <= i__1; ++k) {
	if (k > *nactiv) {
	    j = kx[*nz + k];
	} else {
	    j = kactiv[*nactiv - k + 1] + *n;
	}
	is = istate[j];
	i__ = j - *n;
	if (j <= *n) {
	    anormj = 1.;
	}
	if (j > *n) {
	    anormj = anorms[i__];
	}
	rlam = rlamda[k];
/*        Change the sign of the estimate if the constraint is in */
/*        the working set at its upper bound. */
	if (is == 2) {
	    rlam = -rlam;
	}
	if (is == 3) {
	    rlam = abs(rlam);
	}
	if (is == 4) {
	    rlam = -abs(rlam);
	}
	if (is != 3) {
	    scdlam = rlam * anormj;
	    if (scdlam < *zerolm) {
		if (*numinf == 0) {
		    ++(*notopt);
		}
		if (scdlam < *smllst) {
		    *smllst = scdlam;
		    *trusml = rlamda[k];
		    *jsmlst = j;
		    *ksmlst = k;
		}
	    } else if (scdlam < *tinyst) {
		*tinyst = scdlam;
		*jtiny = j;
	    }
	}
	scdlam = rlam / wtinf[j];
	if (scdlam > *biggst && j > *jinf) {
	    *biggst = scdlam;
	    *trubig = rlamda[k];
	    *jbigst = j;
	    *kbigst = k;
	}
/* L330: */
    }
/*     ----------------------------------------------------------------- */
/*     If required, print the multipliers. */
/*     ----------------------------------------------------------------- */
    if (*msglvl >= 20) {
	if (nfixed > 0) {
	    io___115.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___115);
	    do_fio(&c__1, prbtyp, (ftnlen)2);
	    i__1 = nfixed;
	    for (k = 1; k <= i__1; ++k) {
		do_fio(&c__1, (char *)&kx[*nfree + k], (ftnlen)sizeof(integer)
			);
		do_fio(&c__1, (char *)&rlamda[*nactiv + k], (ftnlen)sizeof(
			doublereal));
	    }
	    e_wsfe();
	}
	if (*nactiv > 0) {
	    io___116.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___116);
	    do_fio(&c__1, prbtyp, (ftnlen)2);
	    i__1 = *nactiv;
	    for (k = 1; k <= i__1; ++k) {
		do_fio(&c__1, (char *)&kactiv[k], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&rlamda[*nactiv - k + 1], (ftnlen)
			sizeof(doublereal));
	    }
	    e_wsfe();
	}
    }
    return 0;
/*     end of cmmul1 */
} /* cmmul1_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int cmmul2_(integer *msglvl, integer *n, integer *nrz, 
	integer *nz, doublereal *zerolm, integer *notopt, integer *numinf, 
	doublereal *trusml, doublereal *smllst, integer *jsmlst, doublereal *
	tinyst, integer *jtiny, doublereal *gq)
{
    /* Format strings */
    static char fmt_1000[] = "(/\002 Multipliers for the artificial constrai"
	    "nts        \002/4(5x,1pe11.2))";

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    doublereal rlam;
    integer j, k;

    /* Fortran I/O blocks */
    static cilist io___119 = { 0, 0, 0, fmt_1000, 0 };


/*     ================================================================== */
/*     cmmul2  updates jsmlst and smllst when there are artificial */
/*     constraints. */

/*     On input,  jsmlst  is the index of the minimum of the set of */
/*     adjusted multipliers. */
/*     On output, a negative jsmlst defines the index in Q'g of the */
/*     artificial constraint to be deleted. */

/*     Original version written 17-Jan-1988. */
/*     This version of cmmul2 dated  23-Jul-1991. */
/*     ================================================================== */
    /* Parameter adjustments */
    --gq;

    /* Function Body */
    i__1 = *nz;
    for (j = *nrz + 1; j <= i__1; ++j) {
	rlam = -(d__1 = gq[j], abs(d__1));
	if (rlam < *zerolm) {
	    if (*numinf == 0) {
		++(*notopt);
	    }
	    if (rlam < *smllst) {
		*trusml = gq[j];
		*smllst = rlam;
		*jsmlst = -j;
	    }
	} else if (rlam < *tinyst) {
	    *tinyst = rlam;
	    *jtiny = -j;
	}
/* L100: */
    }
    if (*msglvl >= 20) {
	io___119.ciunit = sol1cm_1.iprint;
	s_wsfe(&io___119);
	i__1 = *nz;
	for (k = *nrz + 1; k <= i__1; ++k) {
	    do_fio(&c__1, (char *)&gq[k], (ftnlen)sizeof(doublereal));
	}
	e_wsfe();
    }
    return 0;
/*     end of cmmul2 */
} /* cmmul2_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int cmsetx_(logical *rowerr, logical *unitq, integer *nclin, 
	integer *nactiv, integer *nfree, integer *nz, integer *n, integer *
	ldq, integer *lda, integer *ldt, integer *istate, integer *kactiv, 
	integer *kx, integer *jmax, doublereal *errmax, doublereal *xnorm, 
	doublereal *a, doublereal *ax, doublereal *bl, doublereal *bu, 
	doublereal *featol, doublereal *t, doublereal *x, doublereal *q, 
	doublereal *p, doublereal *work)
{
    /* System generated locals */
    integer a_dim1, a_offset, t_dim1, t_offset, q_dim1, q_offset, i__1;
    doublereal d__1;

    /* Local variables */
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    integer ktry;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    integer i__, j, k;
    extern /* Subroutine */ int dload_(integer *, doublereal *, doublereal *, 
	    integer *), dgemv_(char *, integer *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, integer *, ftnlen), dcopy_(integer *, doublereal *, 
	    integer *, doublereal *, integer *), daxpy_(integer *, doublereal 
	    *, doublereal *, integer *, doublereal *, integer *), dtrsv_(char 
	    *, char *, char *, integer *, doublereal *, integer *, doublereal 
	    *, integer *, ftnlen, ftnlen, ftnlen);
    integer is;
    extern integer idamax_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int cmqmul_(integer *, integer *, integer *, 
	    integer *, integer *, logical *, integer *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal bnd;

/*     ================================================================== */
/*     cmsetx  computes the point on a working set that is closest in the */
/*     least-squares sense to the input vector X. */

/*     If the computed point gives a row error of more than the */
/*     feasibility tolerance, an extra step of iterative refinement is */
/*     used.  If  X  is still infeasible,  the logical variable ROWERR */
/*     is set. */

/*     Original version derived from lssetx January-1987. */
/*     This version of  cmsetx  dated   5-Jul-1989. */
/*     ================================================================== */
/*     ------------------------------------------------------------------ */
/*     Move  x  onto the simple bounds in the working set. */
/*     ------------------------------------------------------------------ */
    /* Parameter adjustments */
    --work;
    --p;
    --x;
    --featol;
    --bu;
    --bl;
    --kx;
    --kactiv;
    --istate;
    q_dim1 = *ldq;
    q_offset = q_dim1 + 1;
    q -= q_offset;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    t_dim1 = *ldt;
    t_offset = t_dim1 + 1;
    t -= t_offset;
    --ax;

    /* Function Body */
    i__1 = *n;
    for (k = *nfree + 1; k <= i__1; ++k) {
	j = kx[k];
	is = istate[j];
	bnd = bl[j];
	if (is >= 2) {
	    bnd = bu[j];
	}
	if (is != 4) {
	    x[j] = bnd;
	}
/* L100: */
    }
/*     ------------------------------------------------------------------ */
/*     Move  x  onto the general constraints in the working set. */
/*     ntry  attempts are made to get acceptable row errors. */
/*     ------------------------------------------------------------------ */
    ktry = 1;
    *jmax = 1;
    *errmax = 0.;
/*     repeat */
L200:
    if (*nactiv > 0) {
/*           Set work = residuals for constraints in the working set. */
/*           Solve for P, the smallest correction to x that gives a point */
/*           on the constraints in the working set.  Define  P = Y*(py), */
/*           where  py  solves the triangular system  T*(py) = residuals. */
	i__1 = *nactiv;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    k = kactiv[i__];
	    j = *n + k;
	    bnd = bl[j];
	    if (istate[j] == 2) {
		bnd = bu[j];
	    }
	    work[*nactiv - i__ + 1] = bnd - ddot_(n, &a[k + a_dim1], lda, &x[
		    1], &c__1);
/* L220: */
	}
	dtrsv_("u", "n", "n", nactiv, &t[(*nz + 1) * t_dim1 + 1], ldt, &work[
		1], &c__1, (ftnlen)1, (ftnlen)1, (ftnlen)1);
	dload_(n, &c_b14, &p[1], &c__1);
	dcopy_(nactiv, &work[1], &c__1, &p[*nz + 1], &c__1);
	cmqmul_(&c__2, n, nz, nfree, ldq, unitq, &kx[1], &p[1], &q[q_offset], 
		&work[1]);
	daxpy_(n, &c_b19, &p[1], &c__1, &x[1], &c__1);
    }
/*        --------------------------------------------------------------- */
/*        Compute the 2-norm of  x. */
/*        Initialize  Ax  for all the general constraints. */
/*        --------------------------------------------------------------- */
    *xnorm = dnrm2_(n, &x[1], &c__1);
    if (*nclin > 0) {
	dgemv_("n", nclin, n, &c_b19, &a[a_offset], lda, &x[1], &c__1, &c_b14,
		 &ax[1], &c__1, (ftnlen)1);
    }
/*        --------------------------------------------------------------- */
/*        Check the row residuals. */
/*        --------------------------------------------------------------- */
    if (*nactiv > 0) {
	i__1 = *nactiv;
	for (k = 1; k <= i__1; ++k) {
	    i__ = kactiv[k];
	    j = *n + i__;
	    is = istate[j];
	    if (is == 1) {
		work[k] = bl[j] - ax[i__];
	    }
	    if (is >= 2) {
		work[k] = bu[j] - ax[i__];
	    }
/* L300: */
	}
	*jmax = idamax_(nactiv, &work[1], &c__1);
	*errmax = (d__1 = work[*jmax], abs(d__1));
    }
    ++ktry;
/*     until    (errmax .le. featol(jmax) .or. ktry .gt. ntry */
    if (! (*errmax <= featol[*jmax] || ktry > 5)) {
	goto L200;
    }
    *rowerr = *errmax > featol[*jmax];
/*     end of cmsetx */
    return 0;
} /* cmsetx_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int cmsinf_(integer *n, integer *nclin, integer *lda, 
	integer *istate, doublereal *bigbnd, integer *numinf, doublereal *
	suminf, doublereal *bl, doublereal *bu, doublereal *a, doublereal *
	featol, doublereal *cvec, doublereal *x, doublereal *wtinf)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1;

    /* Local variables */
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    integer j, k;
    extern /* Subroutine */ int dload_(integer *, doublereal *, doublereal *, 
	    integer *);
    doublereal s, feasj;
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *, 
	    integer *, doublereal *, integer *);
    doublereal biglow, bigupp, weight, ctx;

/*     ================================================================== */
/*     cmsinf  finds the number and weighted sum of infeasibilities for */
/*     the bounds and linear constraints.   An appropriate gradient */
/*     is returned in cvec. */

/*     Positive values of  istate(j)  will not be altered.  These mean */
/*     the following... */

/*               1             2           3 */
/*           a'x = bl      a'x = bu     bl = bu */

/*     Other values of  istate(j)  will be reset as follows... */
/*           a'x lt bl     a'x gt bu     a'x free */
/*              - 2           - 1           0 */

/*     Original version written 31-October-1984. */
/*     This version of cmsinf dated  1-January-1987. */
/*     ================================================================== */
    /* Parameter adjustments */
    --x;
    --cvec;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --istate;
    --bl;
    --bu;
    --featol;
    --wtinf;

    /* Function Body */
    bigupp = *bigbnd;
    biglow = -(*bigbnd);
    *numinf = 0;
    *suminf = 0.;
    dload_(n, &c_b14, &cvec[1], &c__1);
    i__1 = *n + *nclin;
    for (j = 1; j <= i__1; ++j) {
	if (istate[j] <= 0) {
	    feasj = featol[j];
	    if (j <= *n) {
		ctx = x[j];
	    } else {
		k = j - *n;
		ctx = ddot_(n, &a[k + a_dim1], lda, &x[1], &c__1);
	    }
	    istate[j] = 0;
/*           See if the lower bound is violated. */
	    if (bl[j] > biglow) {
		s = bl[j] - ctx;
		if (s > feasj) {
		    istate[j] = -2;
		    weight = -wtinf[j];
		    goto L160;
		}
	    }
/*           See if the upper bound is violated. */
	    if (bu[j] >= bigupp) {
		goto L200;
	    }
	    s = ctx - bu[j];
	    if (s <= feasj) {
		goto L200;
	    }
	    istate[j] = -1;
	    weight = wtinf[j];
/*           Add the infeasibility. */
L160:
	    ++(*numinf);
	    *suminf += abs(weight) * s;
	    if (j <= *n) {
		cvec[j] = weight;
	    } else {
		daxpy_(n, &weight, &a[k + a_dim1], lda, &cvec[1], &c__1);
	    }
	}
L200:
	;
    }
/*     end of cmsinf */
    return 0;
} /* cmsinf_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int cmwrap_(logical *needlm, integer *nfree, integer *lda, 
	integer *n, integer *nclin, integer *nctotl, integer *nactiv, integer 
	*istate, integer *kactiv, integer *kx, doublereal *a, doublereal *bl, 
	doublereal *bu, doublereal *c__, doublereal *clamda, doublereal *
	featol, doublereal *r__, doublereal *rlamda, doublereal *x)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1;

    /* Local variables */
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    doublereal rlam;
    integer i__, j, k;
    extern /* Subroutine */ int dload_(integer *, doublereal *, doublereal *, 
	    integer *);
    integer nplin;
    doublereal b1, b2, rj;
    integer is, nz, nfixed;
    doublereal tol, slk1, slk2;

/*     ================================================================== */
/*     cmwrap  creates the expanded Lagrange multiplier vector clamda. */
/*     and resets istate for the printed solution. */

/*     This version of cmwrap is for upper-triangular T. */

/*     For npopt, qpopt and lpopt, kactiv holds the general constraint */
/*     indices in the order in which they were added to the working set. */
/*     The reverse ordering is used for T since new rows are added at */
/*     the front of T. */

/*     Original Fortran 77 version written  05-May-93. */
/*     This version of  cmwrap  dated  09-Oct-95. */
/*     ================================================================== */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --x;
    --rlamda;
    --kx;
    --kactiv;
    --r__;
    --featol;
    --clamda;
    --bu;
    --bl;
    --istate;
    --c__;

    /* Function Body */
    nfixed = *n - *nfree;
    nplin = *n + *nclin;
    nz = *nfree - *nactiv;
    if (*needlm) {
/*        Expand multipliers for bounds and linear constraints */
/*        into the  clamda  array.  This will have been done */
/*        when wrapping up an SQP iteration. */
	dload_(nctotl, &c_b14, &clamda[1], &c__1);
	i__1 = *nactiv + nfixed;
	for (k = 1; k <= i__1; ++k) {
	    if (k <= *nactiv) {
		j = kactiv[k] + *n;
		rlam = rlamda[*nactiv - k + 1];
	    } else {
		j = kx[nz + k];
		rlam = rlamda[k];
	    }
	    clamda[j] = rlam;
/* L150: */
	}
    }
/*     Reset istate if necessary. */
    i__1 = *nctotl;
    for (j = 1; j <= i__1; ++j) {
	b1 = bl[j];
	b2 = bu[j];
	if (j <= *n) {
	    rj = x[j];
	} else if (j <= nplin) {
	    i__ = j - *n;
	    rj = ddot_(n, &a[i__ + a_dim1], lda, &x[1], &c__1);
	} else {
	    i__ = j - nplin;
	    rj = c__[i__];
	}
	is = istate[j];
	slk1 = rj - b1;
	slk2 = b2 - rj;
	tol = featol[j];
	if (slk1 < -tol) {
	    is = -2;
	}
	if (slk2 < -tol) {
	    is = -1;
	}
	if (is == 1 && slk1 > tol) {
	    is = 0;
	}
	if (is == 2 && slk2 > tol) {
	    is = 0;
	}
	istate[j] = is;
	r__[j] = rj;
/* L500: */
    }
/*     end of cmwrap */
    return 0;
} /* cmwrap_ */


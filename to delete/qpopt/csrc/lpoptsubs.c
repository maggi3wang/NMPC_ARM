/* ../src/lpoptsubs.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

extern struct {
    doublereal wmach[15];
} solmch_;

#define solmch_1 solmch_

extern struct {
    integer iprint, isumm, lines1, lines2;
} sol1cm_;

#define sol1cm_1 sol1cm_

extern struct {
    integer lennam, ldt, ncolt, ldq;
} sol3cm_;

#define sol3cm_1 sol3cm_

extern struct {
    doublereal epspt3, epspt5, epspt8, epspt9;
} sol4cm_;

#define sol4cm_1 sol4cm_

extern struct {
    doublereal asize, dtmax, dtmin;
} sol5cm_;

#define sol5cm_1 sol5cm_

extern struct {
    integer loclc[20];
} sol1lc_;

#define sol1lc_1 sol1lc_

extern struct {
    doublereal alfa, trulam;
    integer isdel, jdel, jadd;
    logical header, prnt;
} sol4lc_;

#define sol4lc_1 sol4lc_

extern struct {
    integer ipsvlc[30], itmax1, itmax2, kchk, kcycle, lcrash, lprob, maxact, 
	    mxfree, maxnz, mm, minsum, msglc, nn, nnclin, nprob, ipadlc[15];
} lcpar1_;

#define lcpar1_1 lcpar1_

extern struct {
    doublereal rpsvlc[30], bigbnd, bigdx, bndlow, bndupp, tolact, tolfea, 
	    tolopt, tolrnk, rpadlc[22];
} lcpar2_;

#define lcpar2_1 lcpar2_

extern struct {
    doublereal tolx0, tolinc;
    integer idegen, kdegen, ndegen, itnfix, nfix[2];
} sol3lc_;

#define sol3lc_1 sol3lc_

extern struct {
    logical newopt, listop;
    integer ncalls;
} sol2lc_;

#define sol2lc_1 sol2lc_

/* Table of constant values */

static doublereal c_b2 = .33;
static doublereal c_b3 = .8;
static doublereal c_b4 = .9;
static integer c__1 = 1;
static integer c__6 = 6;
static logical c_true = TRUE_;
static integer c__30 = 30;
static doublereal c_b101 = 0.;
static doublereal c_b122 = 1.;
static doublereal c_b126 = -1.;
static integer c__48 = 48;
static integer c__22 = 22;
static integer c__10 = 10;

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*                                                                      * */
/*    L P O P T         Version 1.0-10(2)       November 4, 2001        * */
/*                                                                      * */
/*    Philip E. Gill    Walter  Murray          Michael A. Saunders     * */
/*    UC San Diego      Stanford University     Stanford University     * */
/*                                                                      * */
/* ----------------------------------------------------------------------* */
/*     (C) 1992--2001  Regents of the University of California          * */
/*                     and the Trustees of Stanford University          * */
/*                                                                      * */
/*     This software is NOT in the public domain. Its use is governed   * */
/*     by a license agreement with either Stanford University or the    * */
/*     University of California.  It is a breach of copyright to make   * */
/*     copies except as authorized by the license agreement.            * */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*     File  lpoptsubs.f */

/*     lpopt    lpcolr   lpcore   lpdflt   lpkey    lploc    lpnkey */
/*     lpprm    lpprmi   lpprmr   lpprms   lpprnt */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int lpopt_(integer *n, integer *nclin, integer *lda, 
	doublereal *a, doublereal *bl, doublereal *bu, doublereal *x, 
	doublereal *ax, integer *inform__, integer *iter, integer *istate, 
	doublereal *clamda, doublereal *obj, doublereal *cvec, integer *iw, 
	integer *leniw, doublereal *w, integer *lenw)
{
    /* Initialized data */

    static char title[40] = "LPOPT  ---  Version 1.0-10(2)  Nov  2001";

    /* Format strings */
    static char fmt_2002[] = "(/\002 Exit LPOPT - Optimal \002,a2,\002 solut"
	    "ion.\002)";
    static char fmt_2001[] = "(/\002 Exit LPOPT - Feasible point found.    "
	    " \002)";
    static char fmt_2010[] = "(/\002 Exit LPOPT - Weak \002,a2,\002 soluti"
	    "on.\002)";
    static char fmt_2020[] = "(/\002 Exit LPOPT - \002,a2,\002 solution is u"
	    "nbounded.\002)";
    static char fmt_2030[] = "(/\002 Exit LPOPT - No feasible point for the "
	    "linear\002,\002 constraints.\002)";
    static char fmt_2035[] = "(/\002 Exit LPOPT - Cannot satisfy the constra"
	    "ints to the\002,\002 accuracy requested.\002)";
    static char fmt_2040[] = "(/\002 Exit LPOPT - Too many iterations.\002)";
    static char fmt_2060[] = "(/\002 Exit LPOPT - \002,i10,\002 errors found"
	    " in the input\002,\002 parameters.  Problem abandoned.\002)";
    static char fmt_2070[] = "(\002 Exit LPOPT - Problem type not recogniz"
	    "ed.\002,\002 Problem abandoned.\002)";
    static char fmt_3000[] = "(/\002 Final \002,a2,\002 objective value ="
	    "\002,g16.7)";
    static char fmt_3010[] = "(/\002 Sum of infeasibilities =\002,g16.7)";
    static char fmt_3011[] = "(/\002 Minimum sum of infeasibilities =\002,g1"
	    "6.7)";
    static char fmt_3015[] = "(/\002 Maximum row error =\002,g16.7)";
    static char fmt_3020[] = "(/\002 Final sum of infeasibilities =\002,g16."
	    "7)";
    static char fmt_2000[] = "(/\002 Exit  \002,a2,\002 phase.   Inform ="
	    " \002,i2,\002   iters = \002,i5)";

    /* System generated locals */
    integer a_dim1, a_offset, i__1;
    doublereal d__1;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_wsfe(cilist *), do_fio(
	    integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    logical cold;
    doublereal amin;
    logical done;
    integer jinf;
    logical cset;
    integer jmax;
    logical warm, rset;
    integer lwrk, nact1;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    integer j;
    extern /* Subroutine */ int dload_(integer *, doublereal *, doublereal *, 
	    integer *);
    logical named;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *), dcond_(integer *, doublereal *, integer *, doublereal 
	    *, doublereal *);
    integer nfree;
    char names[16*1];
    integer lrlam, ncnln;
    extern /* Subroutine */ int lploc_(logical *, integer *, integer *, 
	    integer *, integer *);
    logical found;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *);
    integer itmax;
    extern /* Subroutine */ int icopy_(integer *, integer *, integer *, 
	    integer *, integer *);
    integer nviol;
    char start[4];
    logical unitq;
    doublereal rteps, xnorm;
    integer ld, lq;
    extern /* Subroutine */ int cmdgen_(char *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, ftnlen);
    logical halted;
    integer lt;
    doublereal feamax, feamin;
    integer it, nz;
    extern /* Subroutine */ int mchpar_(void);
    doublereal epsmch;
    integer minact, lfeatu;
#define iprmlc ((integer *)&lcpar1_1 + 30)
    doublereal condmx;
    extern /* Subroutine */ int lpdflt_(integer *, integer *, char *, ftnlen);
    integer minfxd;
#define rprmlc ((doublereal *)&lcpar2_1 + 30)
    integer lkactv, lanorm, nctotl;
#define msglvl ((integer *)&lcpar1_1 + 41)
    integer litotl, lwtinf;
    logical vertex;
    extern /* Subroutine */ int cminit_(integer *, integer *, char *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, logical *, char *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, ftnlen, ftnlen);
    extern /* Subroutine */ int lpprnt_();
    logical rowerr;
    char prbtyp[2];
    integer nerror, llptyp, lwtotl, nmoved, numinf, ianrmj;
    extern /* Subroutine */ int cmcrsh_(char *, logical *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, integer *, doublereal *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, ftnlen);
    integer nactiv, nartif;
    extern /* Subroutine */ int rzadds_(logical *, logical *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    integer nrejtd;
    extern /* Subroutine */ int cmqmul_(integer *, integer *, integer *, 
	    integer *, integer *, logical *, integer *, doublereal *, 
	    doublereal *, doublereal *), cmsetx_(logical *, logical *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    doublereal errmax;
    extern /* Subroutine */ int lpcore_(char *, char *, logical *, logical *, 
	    char *, logical *, logical *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, U_fp, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, ftnlen, 
	    ftnlen, ftnlen), cmwrap_(logical *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), cmprnt_(integer *, integer *, integer *, integer *,
	     doublereal *, logical *, char *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, ftnlen);
    integer lcq, lgq;
    char msg[6];
    integer ngq;
    logical hot;
    integer lkx, nzr;

    /* Fortran I/O blocks */
    static cilist io___66 = { 0, 0, 0, fmt_2002, 0 };
    static cilist io___67 = { 0, 0, 0, fmt_2001, 0 };
    static cilist io___68 = { 0, 0, 0, fmt_2010, 0 };
    static cilist io___69 = { 0, 0, 0, fmt_2020, 0 };
    static cilist io___70 = { 0, 0, 0, fmt_2030, 0 };
    static cilist io___71 = { 0, 0, 0, fmt_2035, 0 };
    static cilist io___72 = { 0, 0, 0, fmt_2040, 0 };
    static cilist io___73 = { 0, 0, 0, fmt_2060, 0 };
    static cilist io___74 = { 0, 0, 0, fmt_2070, 0 };
    static cilist io___75 = { 0, 0, 0, fmt_3000, 0 };
    static cilist io___76 = { 0, 0, 0, fmt_3010, 0 };
    static cilist io___77 = { 0, 0, 0, fmt_3011, 0 };
    static cilist io___78 = { 0, 0, 0, fmt_3015, 0 };
    static cilist io___79 = { 0, 0, 0, fmt_3020, 0 };
    static cilist io___80 = { 0, 0, 0, fmt_2000, 0 };
    static cilist io___81 = { 0, 0, 0, fmt_2000, 0 };


/*     ================================================================== */
/*     lpopt  solves the linear programming problem */

/*           minimize               c' x */
/*              x */
/*                                 (  x ) */
/*           subject to    bl  .le.(    ).ge.  bu, */
/*                                 ( Ax ) */

/*     where  A  is a constant  nclin by n  matrix. */
/*     The feasible region is defined by a mixture of linear equality or */
/*     inequality constraints on  x. */

/*     n  is the number of varibles (dimension of x). */
/*        (n must be positive.) */

/*     nclin  is the number of general linear constraints (rows of  A). */
/*        (nclin may be zero.) */

/*     The first  n  components of  bl  and   bu  are lower and upper */
/*     bounds on the variables.  The next  nclin  components are */
/*     lower and upper bounds on the general linear constraints. */

/*     The matrix  A  of coefficients in the general linear constraints */
/*     is entered as the two-dimensional array  A  (of dimension */
/*     ldA by n).  If nclin = 0,  A is not referenced. */

/*     The vector  x  must contain an initial estimate of the solution, */
/*     and will contain the computed solution on output. */

/*     Documentation for  LPOPT  is coming Real Soon Now. */
/*     Wait for the release of  users guide for LPOPT (Version 1.00), */
/*     by  P. E. Gill, W. Murray and M. A. Saunders, */

/*     Version 1.0-6  Jun 30, 1991.  (Nag Mk 16 version.) */
/*     Version 1.0-7  Mar 21, 1993.  Summary file added. */
/*     Version 1.0-8  Apr 10, 1994.  Sum of infeas. added as an option. */
/*     Version 1.0-9  Jul 15, 1994.  Debug printing removed. */
/*     Version 1.0-10 Sep  9, 1995.  New document. */

/*     This version of  LPOPT  dated 04 November 2001. */
/*     (C) 1992--2001  Regents of the University of California and the */
/*                     Trustees of Stanford University */
/*     ================================================================== */
/*     +Include lcparm-Sep-95++++++++++++++++++++++++++++++++++++++++++++ */
/*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*     local variables. */
    /* Parameter adjustments */
    --x;
    --clamda;
    --istate;
    --bu;
    --bl;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --ax;
    --cvec;
    --iw;
    --w;

    /* Function Body */
/*     Set the machine-dependent constants. */
    mchpar_();
    epsmch = solmch_1.wmach[2];
    rteps = solmch_1.wmach[3];
    sol4cm_1.epspt3 = pow_dd(&epsmch, &c_b2);
    sol4cm_1.epspt5 = rteps;
    sol4cm_1.epspt8 = pow_dd(&epsmch, &c_b3);
    sol4cm_1.epspt9 = pow_dd(&epsmch, &c_b4);
    named = FALSE_;
    *inform__ = 0;
    *iter = 0;
    sol4lc_1.header = TRUE_;
    sol4lc_1.prnt = TRUE_;
/* -->  condmx = max( one/epspt5, hundrd ) */
/* -->  condmx = max( one/epspt3, hundrd ) */
/* Computing MAX */
    d__1 = 1. / sol4cm_1.epspt5;
    condmx = max(d__1,100.);
/*     Set the default values of the parameters. */
    lpdflt_(n, nclin, title, (ftnlen)40);
    llptyp = lcpar1_1.lprob;
    nctotl = *n + *nclin;
/*     Set all parameters determined by the problem type. */
    if (llptyp == 1) {
	s_copy(prbtyp, "FP", (ftnlen)2, (ftnlen)2);
	cset = FALSE_;
    } else if (llptyp == 2) {
	s_copy(prbtyp, "LP", (ftnlen)2, (ftnlen)2);
	cset = TRUE_;
    } else {
	s_copy(prbtyp, "illegal", (ftnlen)2, (ftnlen)7);
	s_copy(msg, "noprob", (ftnlen)6, (ftnlen)6);
	goto L800;
    }
/*     Assign the dimensions of arrays in the parameter list of lpcore. */
/*     economies of storage are possible if the minimum number of active */
/*     constraints and the minimum number of fixed variables are known in */
/*     advance.  The expert user should alter minact and minfxd */
/*     accordingly. */
/*     If a linear program is being solved and the matrix of general */
/*     constraints has fewer rows than columns, i.e.,  nclin .lt. n,  a */
/*     non-zero value is */
/*     known for minfxd.  Note that in this case, vertex must be */
/*     set  .true.. */
    vertex = *nclin < *n;
    minfxd = *n - lcpar1_1.mxfree;
    minact = lcpar1_1.mxfree - lcpar1_1.maxnz;
    sol3cm_1.ldt = max(lcpar1_1.maxnz,lcpar1_1.maxact);
    sol3cm_1.ncolt = lcpar1_1.mxfree;
    if (*nclin == 0) {
	sol3cm_1.ldq = 1;
    } else {
	sol3cm_1.ldq = max(1,lcpar1_1.mxfree);
    }
    ncnln = 0;
    sol3cm_1.lennam = 1;
/*     ================================================================== */
/*     Cold start:  Only  x  is provided. */
/*     Warm start:  Initial working set is specified in  istate. */
/*     Hot  start:  The work arrays  iw  and  w  are assumed to have been */
/*                  initialized during a previous run. */
/*                  The first three components of  iw  contain details */
/*                  on the dimension of the initial working set. */
/*     ================================================================== */
    if (lcpar1_1.lcrash == 0) {
	s_copy(start, "cold", (ftnlen)4, (ftnlen)4);
    } else if (lcpar1_1.lcrash == 1) {
	s_copy(start, "warm", (ftnlen)4, (ftnlen)4);
    } else if (lcpar1_1.lcrash == 2) {
	s_copy(start, "hot ", (ftnlen)4, (ftnlen)4);
    }
    cold = lcpar1_1.lcrash == 0;
    warm = lcpar1_1.lcrash == 1;
    hot = lcpar1_1.lcrash == 2;
/*     Allocate remaining work arrays. */
    litotl = 3;
    lwtotl = 0;
    lploc_(&cset, n, nclin, &litotl, &lwtotl);
    lkactv = sol1lc_1.loclc[0];
    lkx = sol1lc_1.loclc[1];
    lfeatu = sol1lc_1.loclc[2];
    lanorm = sol1lc_1.loclc[3];
    ld = sol1lc_1.loclc[6];
    lgq = sol1lc_1.loclc[7];
    lcq = sol1lc_1.loclc[8];
    lrlam = sol1lc_1.loclc[9];
    lt = sol1lc_1.loclc[11];
    lq = sol1lc_1.loclc[12];
    lwtinf = sol1lc_1.loclc[13];
    lwrk = sol1lc_1.loclc[14];
/*     Check input parameters and storage limits. */
    cminit_(&nerror, msglvl, start, leniw, lenw, &litotl, &lwtotl, n, nclin, &
	    ncnln, &istate[1], &named, names, &lcpar2_1.bigbnd, &bl[1], &bu[1]
	    , &clamda[1], &x[1], (ftnlen)4, (ftnlen)16);
    if (nerror > 0) {
	s_copy(msg, "errors", (ftnlen)6, (ftnlen)6);
	goto L800;
    }
/*     ------------------------------------------------------------------ */
/*     Define the initial feasibility tolerances in clamda. */
/*     ------------------------------------------------------------------ */
    if (lcpar2_1.tolfea > 0.) {
	i__1 = *n + *nclin;
	dload_(&i__1, &lcpar2_1.tolfea, &w[lfeatu], &c__1);
    }
    cmdgen_("Initialize anti-cycling variables", msglvl, n, nclin, &nmoved, 
	    iter, &numinf, &istate[1], &bl[1], &bu[1], &clamda[1], &w[lfeatu],
	     &x[1], (ftnlen)33);
    if (cold || warm) {
/*        --------------------------------------------------------------- */
/*        Cold or warm start.  Just about everything must be initialized. */
/*        The only exception is istate during a warm start. */
/*        --------------------------------------------------------------- */
	ianrmj = lanorm;
	i__1 = *nclin;
	for (j = 1; j <= i__1; ++j) {
	    w[ianrmj] = dnrm2_(n, &a[j + a_dim1], lda);
	    ++ianrmj;
/* L200: */
	}
	if (*nclin > 0) {
	    dcond_(nclin, &w[lanorm], &c__1, &sol5cm_1.asize, &amin);
	}
	dcond_(&nctotl, &w[lfeatu], &c__1, &feamax, &feamin);
	dcopy_(&nctotl, &w[lfeatu], &c__1, &w[lwtinf], &c__1);
	d__1 = 1. / feamin;
	dscal_(&nctotl, &d__1, &w[lwtinf], &c__1);
/*        --------------------------------------------------------------- */
/*        Define the initial working set. */
/*               nfree ,  nactiv,  kactiv, kx, */
/*               istate (if start  = 'cold') */
/*               nartif (if vertex = 'true') */
/*        --------------------------------------------------------------- */
	cmcrsh_(start, &vertex, nclin, &nctotl, &nactiv, &nartif, &nfree, n, 
		lda, &istate[1], &iw[lkactv], &iw[lkx], &lcpar2_1.bigbnd, &
		lcpar2_1.tolact, &a[a_offset], &ax[1], &bl[1], &bu[1], &
		clamda[1], &x[1], &w[lgq], &w[lwrk], (ftnlen)4);
/*        --------------------------------------------------------------- */
/*        Compute the TQ factorization of the working-set matrix. */
/*        --------------------------------------------------------------- */
	unitq = TRUE_;
	nz = nfree;
	if (nactiv > 0) {
	    it = nactiv + 1;
	    nact1 = nactiv;
	    nactiv = 0;
	    ngq = 0;
	    rzadds_(&unitq, &vertex, &c__1, &nact1, &it, &nactiv, &nartif, &
		    nz, &nfree, &nrejtd, &ngq, n, &sol3cm_1.ldq, lda, &
		    sol3cm_1.ldt, &istate[1], &iw[lkactv], &iw[lkx], &condmx, 
		    &a[a_offset], &w[lt], &w[lgq], &w[lq], &w[lwrk], &w[ld], &
		    w[lrlam]);
	}
    } else if (hot) {
/*        --------------------------------------------------------------- */
/*        Arrays  iw  and  w  have been defined in a previous run. */
/*        The first three elements of  iw  are  unitQ,  nfree and nactiv. */
/*        --------------------------------------------------------------- */
	unitq = iw[1] == 1;
	nfree = iw[2];
	nactiv = iw[3];
	nz = nfree - nactiv;
    }
    if (cset) {
/*        Install the transformed linear term in cq. */
	dcopy_(n, &cvec[1], &c__1, &w[lcq], &c__1);
	cmqmul_(&c__6, n, &nz, &nfree, &sol3cm_1.ldq, &unitq, &iw[lkx], &w[
		lcq], &w[lq], &w[lwrk]);
    }
    rset = FALSE_;
    itmax = lcpar1_1.itmax2;
    jinf = 0;
/* +    Take your pick when minimizing the sum of infeasibilities: */
/* +    nZr    =  nZ  implies steepest-descent in the two-norm. */
/* +    nZr    =  0   implies steepest-descent in the infinity norm. */
    nzr = 0;
/*     ================================================================== */
/*     repeat               (until working set residuals are acceptable) */
/*        --------------------------------------------------------------- */
/*        Move x onto the constraints in the working set. */
/*        --------------------------------------------------------------- */
L300:
    cmsetx_(&rowerr, &unitq, nclin, &nactiv, &nfree, &nz, n, &sol3cm_1.ldq, 
	    lda, &sol3cm_1.ldt, &istate[1], &iw[lkactv], &iw[lkx], &jmax, &
	    errmax, &xnorm, &a[a_offset], &ax[1], &bl[1], &bu[1], &w[lfeatu], 
	    &w[lt], &x[1], &w[lq], &w[ld], &w[lwrk]);
    if (rowerr) {
	s_copy(msg, "rowerr", (ftnlen)6, (ftnlen)6);
	numinf = 1;
	goto L800;
    }
    lpcore_(prbtyp, msg, &cset, &named, names, &rset, &unitq, iter, &itmax, &
	    jinf, &nviol, n, nclin, lda, &nactiv, &nfree, &nzr, &nz, &istate[
	    1], &iw[lkactv], &iw[lkx], (U_fp)lpprnt_, obj, &numinf, &xnorm, &
	    a[a_offset], &ax[1], &bl[1], &bu[1], &cvec[1], &clamda[1], &w[
	    lfeatu], &x[1], &iw[1], &w[1], (ftnlen)2, (ftnlen)6, (ftnlen)16);
    found = s_cmp(msg, "feasbl", (ftnlen)6, (ftnlen)6) == 0 || s_cmp(msg, 
	    "optiml", (ftnlen)6, (ftnlen)6) == 0 || s_cmp(msg, "weak  ", (
	    ftnlen)6, (ftnlen)6) == 0 || s_cmp(msg, "unbndd", (ftnlen)6, (
	    ftnlen)6) == 0 || s_cmp(msg, "infeas", (ftnlen)6, (ftnlen)6) == 0;
    halted = s_cmp(msg, "itnlim", (ftnlen)6, (ftnlen)6) == 0;
    if (found) {
	cmdgen_("optimal", msglvl, n, nclin, &nmoved, iter, &numinf, &istate[
		1], &bl[1], &bu[1], &clamda[1], &w[lfeatu], &x[1], (ftnlen)7);
    }
    done = found && nviol == 0 && nmoved == 0;
/*     until      done  .or.  halted */
    if (! (done || halted)) {
	goto L300;
    }
/*     =========================================================== */
/*     Set   clamda.  Print the full solution. */
/*     Clean up.  Save values for a subsequent hot start. */
/*     ------------------------------------------------------------------ */
    cmwrap_(&c_true, &nfree, lda, n, nclin, &nctotl, &nactiv, &istate[1], &iw[
	    lkactv], &iw[lkx], &a[a_offset], &bl[1], &bu[1], &x[1], &clamda[1]
	    , &w[lfeatu], &w[lwrk], &w[lrlam], &x[1]);
    cmprnt_(msglvl, n, nclin, &nctotl, &lcpar2_1.bigbnd, &named, names, &
	    istate[1], &bl[1], &bu[1], &clamda[1], &w[lfeatu], &w[lwrk], (
	    ftnlen)16);
    iw[1] = 0;
    if (unitq) {
	iw[1] = 1;
    }
    iw[2] = nfree;
    iw[3] = nactiv;
/*     ================================================================== */
/*     Print messages if required. */
/*     Recover the optional parameters set by the user. */
/*     ================================================================== */
L800:
    sol4lc_1.prnt = *msglvl > 0 && sol1cm_1.iprint > 0;
    if (s_cmp(msg, "optiml", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 0;
	if (sol4lc_1.prnt) {
	    io___66.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___66);
	    do_fio(&c__1, prbtyp, (ftnlen)2);
	    e_wsfe();
	}
    } else if (s_cmp(msg, "feasbl", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 0;
	if (sol4lc_1.prnt) {
	    io___67.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___67);
	    e_wsfe();
	}
    } else if (s_cmp(msg, "weak  ", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 1;
	if (sol4lc_1.prnt) {
	    io___68.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___68);
	    do_fio(&c__1, prbtyp, (ftnlen)2);
	    e_wsfe();
	}
    } else if (s_cmp(msg, "unbndd", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 2;
	if (sol4lc_1.prnt) {
	    io___69.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___69);
	    do_fio(&c__1, prbtyp, (ftnlen)2);
	    e_wsfe();
	}
    } else if (s_cmp(msg, "infeas", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 3;
	if (sol4lc_1.prnt) {
	    io___70.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___70);
	    e_wsfe();
	}
    } else if (s_cmp(msg, "rowerr", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 3;
	if (sol4lc_1.prnt) {
	    io___71.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___71);
	    e_wsfe();
	}
    } else if (s_cmp(msg, "itnlim", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 4;
	if (sol4lc_1.prnt) {
	    io___72.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___72);
	    e_wsfe();
	}
    } else if (s_cmp(msg, "errors", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 6;
	if (sol4lc_1.prnt) {
	    io___73.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___73);
	    do_fio(&c__1, (char *)&nerror, (ftnlen)sizeof(integer));
	    e_wsfe();
	}
    } else if (s_cmp(msg, "noprob", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 7;
	if (sol4lc_1.prnt) {
	    io___74.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___74);
	    e_wsfe();
	}
    }
    if (sol4lc_1.prnt) {
	if (*inform__ < 5) {
	    if (numinf == 0) {
		if (s_cmp(prbtyp, "FP", (ftnlen)2, (ftnlen)2) != 0) {
		    io___75.ciunit = sol1cm_1.iprint;
		    s_wsfe(&io___75);
		    do_fio(&c__1, prbtyp, (ftnlen)2);
		    do_fio(&c__1, (char *)&(*obj), (ftnlen)sizeof(doublereal))
			    ;
		    e_wsfe();
		}
	    } else if (*inform__ == 3) {
		if (s_cmp(msg, "infeas", (ftnlen)6, (ftnlen)6) == 0) {
		    if (lcpar1_1.minsum == 0) {
			io___76.ciunit = sol1cm_1.iprint;
			s_wsfe(&io___76);
			do_fio(&c__1, (char *)&(*obj), (ftnlen)sizeof(
				doublereal));
			e_wsfe();
		    } else {
			io___77.ciunit = sol1cm_1.iprint;
			s_wsfe(&io___77);
			do_fio(&c__1, (char *)&(*obj), (ftnlen)sizeof(
				doublereal));
			e_wsfe();
		    }
		} else if (s_cmp(msg, "rowerr", (ftnlen)6, (ftnlen)6) == 0) {
		    io___78.ciunit = sol1cm_1.iprint;
		    s_wsfe(&io___78);
		    do_fio(&c__1, (char *)&errmax, (ftnlen)sizeof(doublereal))
			    ;
		    e_wsfe();
		}
	    } else {
		io___79.ciunit = sol1cm_1.iprint;
		s_wsfe(&io___79);
		do_fio(&c__1, (char *)&(*obj), (ftnlen)sizeof(doublereal));
		e_wsfe();
	    }
	}
    }
    if (*inform__ < 6) {
	if (*msglvl > 0) {
	    if (sol1cm_1.iprint > 0) {
		io___80.ciunit = sol1cm_1.iprint;
		s_wsfe(&io___80);
		do_fio(&c__1, prbtyp, (ftnlen)2);
		do_fio(&c__1, (char *)&(*inform__), (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&(*iter), (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	    if (sol1cm_1.isumm > 0) {
		io___81.ciunit = sol1cm_1.isumm;
		s_wsfe(&io___81);
		do_fio(&c__1, prbtyp, (ftnlen)2);
		do_fio(&c__1, (char *)&(*inform__), (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&(*iter), (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
    }
    icopy_(&c__30, lcpar1_1.ipsvlc, &c__1, iprmlc, &c__1);
    dcopy_(&c__30, lcpar2_1.rpsvlc, &c__1, rprmlc, &c__1);
    return 0;
/*     end of lpopt */
} /* lpopt_ */

#undef msglvl
#undef rprmlc
#undef iprmlc


/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int lpcolr_(integer *nzr, integer *ldr, doublereal *r__, 
	doublereal *rzz)
{
    /* System generated locals */
    integer r_dim1, r_offset, i__1;

    /* Local variables */
    extern /* Subroutine */ int dload_(integer *, doublereal *, doublereal *, 
	    integer *);

/*     ================================================================== */
/*     lpcolr  loads the last column of the  nZr x nZr  triangular factor */
/*     Rz  with the multiple  Rzz  of the  nZr-th unit vector. */

/*     Original version written by PEG,  23-Jul-87. */
/*     This version of  lpcolr  dated 17-Jul-90. */
/*     (C) 1992--1997  Regents of the University of California and the */
/*                     Trustees of Stanford University */
/*     ================================================================== */
    /* Parameter adjustments */
    r_dim1 = *ldr;
    r_offset = r_dim1 + 1;
    r__ -= r_offset;

    /* Function Body */
    if (*nzr == 0) {
	return 0;
    }
    i__1 = *nzr - 1;
    dload_(&i__1, &c_b101, &r__[*nzr * r_dim1 + 1], &c__1);
    r__[*nzr + *nzr * r_dim1] = *rzz;
/*     end of lpcolr */
    return 0;
} /* lpcolr_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int lpcore_(char *prbtyp, char *msg, logical *cset, logical *
	named, char *names, logical *rset, logical *unitq, integer *iter, 
	integer *itmax, integer *jinf, integer *nviol, integer *n, integer *
	nclin, integer *lda, integer *nactiv, integer *nfree, integer *nzr, 
	integer *nz, integer *istate, integer *kactiv, integer *kx, S_fp 
	lpprnt, doublereal *obj, integer *numinf, doublereal *xnorm, 
	doublereal *a, doublereal *ax, doublereal *bl, doublereal *bu, 
	doublereal *cvec, doublereal *featol, doublereal *featlu, doublereal *
	x, integer *iw, doublereal *w, ftnlen prbtyp_len, ftnlen msg_len, 
	ftnlen names_len)
{
    /* Format strings */
    static char fmt_2100[] = "(\002 XXX  Iterative refinement.  The maximum "
	    "violation is \002,1p,e14.2,\002 in constraint\002,i5)";

    /* System generated locals */
    integer a_dim1, a_offset, i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    integer iadd, kdel;
    extern doublereal ddiv_(doublereal *, doublereal *, logical *), ddot_(
	    integer *, doublereal *, integer *, doublereal *, integer *);
    integer ifix, jmax;
    logical move;
    integer lwrk;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    integer j;
    extern /* Subroutine */ int dload_(integer *, doublereal *, doublereal *, 
	    integer *);
    doublereal alfap;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *);
    logical onbnd;
    extern /* Subroutine */ int dgemv_(char *, integer *, integer *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, integer *, ftnlen);
    doublereal flmax;
    integer lrlam;
    doublereal condt;
    extern /* Subroutine */ int rzadd_(logical *, logical *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), dcopy_(
	    integer *, doublereal *, integer *, doublereal *, integer *);
    doublereal dinky;
    extern /* Subroutine */ int rzdel_(logical *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    doublereal dnorm;
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *, 
	    integer *, doublereal *, integer *);
    doublereal tollm;
    integer jtiny;
    extern /* Subroutine */ int cmmul1_(char *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, doublereal *, integer *, integer *, doublereal *, 
	    doublereal *, integer *, integer *, doublereal *, integer *, 
	    integer *, doublereal *, doublereal *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, ftnlen), cmmul2_(integer *, integer *,
	     integer *, integer *, doublereal *, integer *, integer *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *);
    integer ld;
    logical fp, lp;
    integer it, lr, lt, lq, is;
    logical unbndd;
#define iprmlc ((integer *)&lcpar1_1 + 30)
    logical overfl;
#define rprmlc ((doublereal *)&lcpar2_1 + 30)
    integer lanorm, lwtinf;
#define msglvl ((integer *)&lcpar1_1 + 41)
    logical hitlow;
    integer nctotl;
    static logical firstv;
    doublereal condmx;
    extern /* Subroutine */ int cmsinf_(integer *, integer *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal suminf;
    extern /* Subroutine */ int cmqmul_(integer *, integer *, integer *, 
	    integer *, integer *, logical *, integer *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal gznorm, gzrnrm, gfnorm, condrz;
    integer notopt;
    doublereal objsiz, wssize;
    integer nfixed;
    doublereal zerolm, smllst, biggst, tinyst, trusml;
    integer jsmlst, ksmlst;
    doublereal trubig;
    integer jbigst, kbigst;
    extern /* Subroutine */ int lpcolr_(integer *, integer *, doublereal *, 
	    doublereal *);
    doublereal bigalf;
    extern /* Subroutine */ int cmchzr_(logical *, integer *, integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, logical *, 
	    logical *, logical *, logical *, doublereal *, doublereal *, 
	    integer *, doublereal *, doublereal *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    doublereal alfhit;
    integer inform__;
    extern /* Subroutine */ int cmfeas_(integer *, integer *, integer *, 
	    doublereal *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    doublereal errmax;
    extern /* Subroutine */ int cmdgen_(char *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, ftnlen);
    integer nmoved, ntfixd, lad, lcq, ldr, lgq, ngq;
    doublereal dzz;

    /* Fortran I/O blocks */
    static cilist io___145 = { 0, 0, 0, fmt_2100, 0 };
    static cilist io___146 = { 0, 0, 0, fmt_2100, 0 };


/*     ================================================================== */
/*     lpcore  is a subroutine for linear programming. */
/*     On entry, it is assumed that an initial working set of */
/*     linear constraints and bounds is available.  The arrays  istate, */
/*     kactiv  and  kx  will have been set accordingly */
/*     and the arrays  T  and  Q  will contain the TQ factorization of */
/*     the matrix whose rows are the gradients of the active linear */
/*     constraints with the columns corresponding to the active bounds */
/*     removed.  The TQ factorization of the resulting (nactiv by nfree) */
/*     matrix is  A(free)*Q = (0 T),  where Q is (nfree by nfree) and T */
/*     is upper-triangular. */

/*     kactiv holds the general constraint indices in the order in which */
/*     they were added.  The reverse ordering is used for T since new */
/*     rows are added at the front of T. */

/*     Over a cycle of iterations, the feasibility tolerance featol */
/*     increases slightly (from tolx0 to tolx1 in steps of tolinc). */
/*     this ensures that all steps taken will be positive. */

/*     After idegen consecutive iterations, variables within featol of */
/*     their bounds are set exactly on their bounds and iterative */
/*     refinement is used to satisfy the constraints in the working set. */
/*     Featol is then reduced to tolx0 for the next cycle of iterations. */


/*     Values of istate(j) for the linear constraints....... */

/*     Istate(j) */
/*     --------- */
/*          0    constraint j is not in the working set. */
/*          1    constraint j is in the working set at its lower bound. */
/*          2    constraint j is in the working set at its upper bound. */
/*          3    constraint j is in the working set as an equality. */

/*     Constraint j may be violated by as much as featol(j). */

/*     This version of  lpcore  dated  04-Jan-96. */
/*     (C) 1992--1997  Regents of the University of California and the */
/*                     Trustees of Stanford University */
/*     ================================================================== */
/*     +Include lcparm-Sep-95++++++++++++++++++++++++++++++++++++++++++++ */
/*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*     Specify the machine-dependent parameters. */
    /* Parameter adjustments */
    names -= 16;
    --x;
    --kx;
    --kactiv;
    --featlu;
    --featol;
    --bu;
    --bl;
    --istate;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --ax;
    --cvec;
    --iw;
    --w;

    /* Function Body */
    flmax = solmch_1.wmach[6];
    if (*cset) {
	ngq = 2;
    } else {
	ngq = 1;
    }
    lp = s_cmp(prbtyp, "lp", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(prbtyp, 
	    "LP", (ftnlen)2, (ftnlen)2) == 0;
    fp = ! lp;
    ldr = sol3cm_1.ldt;
    it = 1;
    lanorm = sol1lc_1.loclc[3];
    lad = sol1lc_1.loclc[4];
    ld = sol1lc_1.loclc[6];
    lgq = sol1lc_1.loclc[7];
    lcq = sol1lc_1.loclc[8];
    lrlam = sol1lc_1.loclc[9];
    lr = sol1lc_1.loclc[10];
    lt = sol1lc_1.loclc[11];
    lq = sol1lc_1.loclc[12];
    lwtinf = sol1lc_1.loclc[13];
    lwrk = sol1lc_1.loclc[14];
    if (*iter == 0) {
/*        ------------------------- */
/*        First entry.  Initialize. */
/*        ------------------------- */
	sol4lc_1.jadd = 0;
	sol4lc_1.jdel = 0;
	sol4lc_1.isdel = 0;
	firstv = FALSE_;
	sol4lc_1.alfa = 0.;
	dzz = 1.;
    }
    nctotl = *n + *nclin;
    *nviol = 0;
    condmx = flmax;
    cmsinf_(n, nclin, lda, &istate[1], &lcpar2_1.bigbnd, numinf, &suminf, &bl[
	    1], &bu[1], &a[a_offset], &featol[1], &w[lgq], &x[1], &w[lwtinf]);
    if (*numinf > 0) {
	cmqmul_(&c__6, n, nz, nfree, &sol3cm_1.ldq, unitq, &kx[1], &w[lgq], &
		w[lq], &w[lwrk]);
    } else if (lp) {
	dcopy_(n, &w[lcq], &c__1, &w[lgq], &c__1);
    }
    if (*numinf == 0 && lp) {
	*obj = ddot_(n, &cvec[1], &c__1, &x[1], &c__1);
    } else {
	*obj = suminf;
    }
    s_copy(msg, "      ", (ftnlen)6, (ftnlen)6);
/* *    ======================Start of main loop========================== */
/* +    do while (msg .eq. empty) */
L100:
    if (s_cmp(msg, "      ", (ftnlen)6, (ftnlen)6) == 0) {
	gznorm = 0.;
	if (*nz > 0) {
	    gznorm = dnrm2_(nz, &w[lgq], &c__1);
	}
	if (*nzr == *nz) {
	    gzrnrm = gznorm;
	} else {
	    gzrnrm = 0.;
	    if (*nzr > 0) {
		gzrnrm = dnrm2_(nzr, &w[lgq], &c__1);
	    }
	}
	gfnorm = gznorm;
	if (*nfree > 0 && *nactiv > 0) {
	    gfnorm = dnrm2_(nfree, &w[lgq], &c__1);
	}
/*        --------------------------------------------------------------- */
/*        Print the details of this iteration. */
/*        --------------------------------------------------------------- */
/*        Define small quantities that reflect the size of x, R and */
/*        the constraints in the working set. */
	if (sol4lc_1.prnt) {
	    condt = 1.;
	    if (*nactiv > 0) {
		condt = ddiv_(&sol5cm_1.dtmax, &sol5cm_1.dtmin, &overfl);
	    }
	    (*lpprnt)(prbtyp, &sol4lc_1.header, rset, msglvl, iter, &
		    sol4lc_1.isdel, &sol4lc_1.jdel, &sol4lc_1.jadd, n, nclin, 
		    nactiv, nfree, nz, nzr, &ldr, &sol3cm_1.ldt, &istate[1], &
		    sol4lc_1.alfa, &condrz, &condt, &dzz, &gznorm, numinf, &
		    suminf, &notopt, obj, &sol4lc_1.trulam, &ax[1], &w[lr], &
		    w[lt], &x[1], &w[lwrk], (ftnlen)2);
	    sol4lc_1.jdel = 0;
	    sol4lc_1.jadd = 0;
	    sol4lc_1.alfa = 0.;
	}
	if (*numinf > 0) {
	    dinky = 0.;
	    tollm = 0.;
	} else {
	    objsiz = abs(*obj) + 1.;
	    wssize = 0.;
	    if (*nactiv > 0) {
		wssize = sol5cm_1.dtmax;
	    }
/* Computing MAX */
	    d__1 = max(wssize,objsiz);
	    dinky = sol4cm_1.epspt8 * max(d__1,gfnorm);
/* Computing MAX */
	    d__1 = max(wssize,objsiz);
	    tollm = lcpar2_1.tolopt * max(d__1,gfnorm);
	}
/*        If the reduced gradient Z'g is small enough, */
/*        Lagrange multipliers will be computed. */
	if (*numinf == 0 && fp) {
	    s_copy(msg, "feasbl", (ftnlen)6, (ftnlen)6);
	    nfixed = *n - *nfree;
	    i__1 = *nactiv + nfixed;
	    dload_(&i__1, &c_b101, &w[lrlam], &c__1);
	    goto L100;
	}
	if (gzrnrm <= dinky) {
/*           ============================================================ */
/*           The point  x  is a constrained stationary point. */
/*           Compute Lagrange multipliers. */
/*           ============================================================ */
/*           Define what we mean by 'tiny' and non-optimal multipliers. */
	    notopt = 0;
	    sol4lc_1.jdel = 0;
	    zerolm = -tollm;
	    smllst = -tollm;
	    biggst = tollm + 1.;
	    tinyst = tollm;
	    cmmul1_(prbtyp, msglvl, n, lda, &sol3cm_1.ldt, nactiv, nfree, nz, 
		    &istate[1], &kactiv[1], &kx[1], &zerolm, &notopt, numinf, 
		    &trusml, &smllst, &jsmlst, &ksmlst, &tinyst, &jtiny, jinf,
		     &trubig, &biggst, &jbigst, &kbigst, &a[a_offset], &w[
		    lanorm], &w[lgq], &w[lrlam], &w[lt], &w[lwtinf], (ftnlen)
		    2);
	    if (*nzr < *nz) {
		cmmul2_(msglvl, n, nzr, nz, &zerolm, &notopt, numinf, &trusml,
			 &smllst, &jsmlst, &tinyst, &jtiny, &w[lgq]);
	    }
	    if (abs(jsmlst) > 0) {
/*              --------------------------------------------------------- */
/*              Delete a constraint. */
/*              --------------------------------------------------------- */
/*              cmmul1  or  cmmul2  found a non-optimal multiplier. */
		sol4lc_1.trulam = trusml;
		sol4lc_1.jdel = jsmlst;
		if (jsmlst > 0) {
/*                 Regular constraint. */
		    kdel = ksmlst;
		    sol4lc_1.isdel = istate[sol4lc_1.jdel];
		    istate[sol4lc_1.jdel] = 0;
		}
	    } else if (lcpar1_1.minsum > 0) {
		if (*numinf > 0 && jbigst > 0) {
/*                 No feasible point exists for the constraints but the */
/*                 sum of the constraint violations can be reduced by */
/*                 moving off constraints with multipliers greater than 1. */
		    sol4lc_1.jdel = jbigst;
		    kdel = kbigst;
		    sol4lc_1.isdel = istate[sol4lc_1.jdel];
		    if (trubig <= 0.) {
			is = -1;
		    }
		    if (trubig > 0.) {
			is = -2;
		    }
		    istate[sol4lc_1.jdel] = is;
		    sol4lc_1.trulam = trubig;
		    firstv = TRUE_;
		    ++(*numinf);
		}
	    }
	    if (sol4lc_1.jdel == 0) {
		if (*numinf > 0) {
		    s_copy(msg, "infeas", (ftnlen)6, (ftnlen)6);
		} else {
		    s_copy(msg, "optiml", (ftnlen)6, (ftnlen)6);
		}
		goto L100;
	    }
/*           Constraint  jdel  has been deleted. */
/*           Update the  TQ  factorization. */
	    rzdel_(unitq, &it, n, nactiv, nfree, &ngq, nz, nzr, lda, &
		    sol3cm_1.ldq, &sol3cm_1.ldt, &sol4lc_1.jdel, &kdel, &
		    kactiv[1], &kx[1], &a[a_offset], &w[lt], &w[lgq], &w[lq], 
		    &w[lwrk], &w[ld], &w[lrlam]);
	    if (*rset) {
		lpcolr_(nzr, &ldr, &w[lr], &c_b122);
	    }
	    sol4lc_1.prnt = FALSE_;
	} else {
/*           ============================================================ */
/*           Compute a search direction. */
/*           ============================================================ */
	    if (*iter >= *itmax) {
		s_copy(msg, "itnlim", (ftnlen)6, (ftnlen)6);
		goto L100;
	    }
	    sol4lc_1.prnt = TRUE_;
	    ++(*iter);
	    dcopy_(nzr, &w[lgq], &c__1, &w[ld], &c__1);
	    dscal_(nzr, &c_b126, &w[ld], &c__1);
	    dnorm = dnrm2_(nzr, &w[ld], &c__1);
	    cmqmul_(&c__1, n, nzr, nfree, &sol3cm_1.ldq, unitq, &kx[1], &w[ld]
		    , &w[lq], &w[lwrk]);
	    dgemv_("No transpose", nclin, n, &c_b122, &a[a_offset], lda, &w[
		    ld], &c__1, &c_b101, &w[lad], &c__1, (ftnlen)12);
/*           ------------------------------------------------------------ */
/*           Find the constraint we bump into along d. */
/*           Update  x  and  Ax  if the step alfa is nonzero. */
/*           ------------------------------------------------------------ */
/*           alfhit is initialized to bigalf. If it remains that value */
/*           after the call to  cmchzr, it is regarded as infinite. */
	    bigalf = ddiv_(&lcpar2_1.bigdx, &dnorm, &overfl);
	    cmchzr_(&firstv, n, nclin, &istate[1], &bigalf, &lcpar2_1.bigbnd, 
		    &dnorm, &hitlow, &move, &onbnd, &unbndd, &alfhit, &alfap, 
		    &sol4lc_1.jadd, &w[lanorm], &w[lad], &ax[1], &bl[1], &bu[
		    1], &featol[1], &featlu[1], &w[ld], &x[1]);
	    if (unbndd) {
		s_copy(msg, "unbndd", (ftnlen)6, (ftnlen)6);
		goto L100;
	    }
	    sol4lc_1.alfa = alfhit;
	    daxpy_(n, &sol4lc_1.alfa, &w[ld], &c__1, &x[1], &c__1);
	    if (*nclin > 0) {
		daxpy_(nclin, &sol4lc_1.alfa, &w[lad], &c__1, &ax[1], &c__1);
	    }
	    *xnorm = dnrm2_(n, &x[1], &c__1);
/*           ------------------------------------------------------------ */
/*           Add a constraint to the working set. */
/*           Update the  TQ  factors of the working set. */
/*           Use  d  as temporary work space. */
/*           ------------------------------------------------------------ */
	    if (bl[sol4lc_1.jadd] == bu[sol4lc_1.jadd]) {
		istate[sol4lc_1.jadd] = 3;
	    } else if (hitlow) {
		istate[sol4lc_1.jadd] = 1;
	    } else {
		istate[sol4lc_1.jadd] = 2;
	    }
	    if (sol4lc_1.jadd > *n) {
		iadd = sol4lc_1.jadd - *n;
	    } else {
		if (sol4lc_1.alfa >= 0.) {
		    if (hitlow) {
			x[sol4lc_1.jadd] = bl[sol4lc_1.jadd];
		    } else {
			x[sol4lc_1.jadd] = bu[sol4lc_1.jadd];
		    }
		}
		i__1 = *nfree;
		for (ifix = 1; ifix <= i__1; ++ifix) {
		    if (kx[ifix] == sol4lc_1.jadd) {
			goto L520;
		    }
/* L510: */
		}
L520:
		;
	    }
	    rzadd_(unitq, rset, &inform__, &ifix, &iadd, &sol4lc_1.jadd, &it, 
		    nactiv, nz, nfree, nzr, &ngq, n, lda, &sol3cm_1.ldq, &ldr,
		     &sol3cm_1.ldt, &kx[1], &condmx, &dzz, &a[a_offset], &w[
		    lr], &w[lt], &w[lgq], &w[lq], &w[lwrk], &w[lrlam], &w[ld])
		    ;
	    --(*nz);
	    --(*nzr);
	    if (sol4lc_1.jadd <= *n) {
/*              A simple bound has been added. */
		--(*nfree);
	    } else {
/*              A general constraint has been added. */
		++(*nactiv);
		kactiv[*nactiv] = iadd;
	    }
/*           Increment featol. */
	    daxpy_(&nctotl, &sol3lc_1.tolinc, &featlu[1], &c__1, &featol[1], &
		    c__1);
	    if (*iter % lcpar1_1.kchk == 0) {
/*              --------------------------------------------------------- */
/*              Check the feasibility of constraints with non-negative */
/*              istate values.  If some violations have occurred, force */
/*              iterative refinement and switch to phase 1. */
/*              --------------------------------------------------------- */
		cmfeas_(n, nclin, &istate[1], &lcpar2_1.bigbnd, nviol, &jmax, 
			&errmax, &ax[1], &bl[1], &bu[1], &featol[1], &x[1]);
		if (*nviol > 0) {
		    if (*msglvl > 0) {
			if (sol1cm_1.iprint > 0) {
			    io___145.ciunit = sol1cm_1.iprint;
			    s_wsfe(&io___145);
			    do_fio(&c__1, (char *)&errmax, (ftnlen)sizeof(
				    doublereal));
			    do_fio(&c__1, (char *)&jmax, (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			}
			if (sol1cm_1.isumm > 0) {
			    io___146.ciunit = sol1cm_1.isumm;
			    s_wsfe(&io___146);
			    do_fio(&c__1, (char *)&errmax, (ftnlen)sizeof(
				    doublereal));
			    do_fio(&c__1, (char *)&jmax, (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			}
		    }
		}
	    }
	    if (*iter % sol3lc_1.idegen == 0) {
/*              Every  idegen  iterations, reset  featol  and */
/*              move  x  on to the working set if it is close. */
		cmdgen_("End of cycle", msglvl, n, nclin, &nmoved, iter, 
			numinf, &istate[1], &bl[1], &bu[1], &featol[1], &
			featlu[1], &x[1], (ftnlen)12);
		*nviol += nmoved;
	    }
	    if (*nviol > 0) {
		s_copy(msg, "resetx", (ftnlen)6, (ftnlen)6);
		goto L100;
	    }
	    if (*numinf != 0) {
		cmsinf_(n, nclin, lda, &istate[1], &lcpar2_1.bigbnd, numinf, &
			suminf, &bl[1], &bu[1], &a[a_offset], &featol[1], &w[
			lgq], &x[1], &w[lwtinf]);
		if (*numinf > 0) {
		    cmqmul_(&c__6, n, nz, nfree, &sol3cm_1.ldq, unitq, &kx[1],
			     &w[lgq], &w[lq], &w[lwrk]);
		} else if (lp) {
		    dcopy_(n, &w[lcq], &c__1, &w[lgq], &c__1);
		}
	    }
	    if (*numinf == 0 && lp) {
		*obj = ddot_(n, &cvec[1], &c__1, &x[1], &c__1);
	    } else {
		*obj = suminf;
	    }
	}
	goto L100;
/* +    end while */
    }
/*     ======================end of main loop============================ */

    if (s_cmp(msg, "optiml", (ftnlen)6, (ftnlen)6) == 0) {
	if (lp) {
	    if (*nzr < *nz) {
		s_copy(msg, "weak  ", (ftnlen)6, (ftnlen)6);
	    } else {
		ntfixd = 0;
		i__1 = *n;
		for (j = 1; j <= i__1; ++j) {
		    if (istate[j] == 4) {
			++ntfixd;
		    }
/* L900: */
		}
		if (ntfixd > 0) {
		    s_copy(msg, "weak  ", (ftnlen)6, (ftnlen)6);
		}
	    }
	    if (abs(jtiny) > 0) {
		s_copy(msg, "weak  ", (ftnlen)6, (ftnlen)6);
	    }
	}
    } else if (s_cmp(msg, "unbndd", (ftnlen)6, (ftnlen)6) == 0 && *numinf > 0)
	     {
	s_copy(msg, "infeas", (ftnlen)6, (ftnlen)6);
    }
    return 0;
/*     end of lpcore */
} /* lpcore_ */

#undef msglvl
#undef rprmlc
#undef iprmlc


/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int lpdflt_(integer *n, integer *nclin, char *title, ftnlen 
	title_len)
{
    /* Initialized data */

    static char noyes[3*2] = " No" "Yes";
    static char icrsh[4*3] = "Cold" "Warm" "Hot ";
    static char lptype[7*10] = "     FP" "     LP" "illegal" "illegal" "ille"
	    "gal" "illegal" "       " "       " "       " "illegal";

    /* Format strings */
    static char fmt_2000[] = "(//\002 Parameters\002/\002 ----------\002)";
    static char fmt_2100[] = "(/\002 Problem type...........\002,3x,a7/\002 "
	    "Linear constraints.....\002,i10,2x,1x,a4,\002 start............"
	    ".\002,12x,\002 Min. Sum of Infeas.....\002,7x,a3/\002 Variables."
	    ".............\002,i10,2x,\002 Infinite bound size....\002,1p,e10"
	    ".2,2x,\002 Feasibility tolerance..\002,1p,e10.2/\002 Check frequ"
	    "ency........\002,i10,2x,\002 Infinite step size.....\002,1p,e10."
	    "2,2x,\002 Optimality tolerance...\002,e10.2/\002 Max degrees of "
	    "freedom.\002,i10,2x,\002 Expand frequency.......\002,i10,2x,\002"
	    " Crash tolerance........\002,e10.2/\002 Max free variables...."
	    ".\002,i10/\002 Max active constraints.\002,i10)";
    static char fmt_2200[] = "(/\002 Print level............\002,i10,2x,\002"
	    " Print file.............\002,i10,2x,\002 Feasibility phase itns"
	    ".\002,i10/\002 Unit round-off.........\002,1p,e10.2,2x,\002 Summ"
	    "ary file...........\002,i10,2x,\002 Optimality  phase itns.\002,"
	    "i10)";

    /* System generated locals */
    integer i__1, i__2;
    cilist ci__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_wsfe(cilist *), do_fio(integer *, char *,
	     ftnlen), e_wsfe(void);

    /* Local variables */
    integer lent, j;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *), icopy_(integer *, integer *, integer *, 
	    integer *, integer *), mcout_(integer *, integer *);
    doublereal epsmch;
    integer nspace;
#define iprmlc ((integer *)&lcpar1_1 + 30)
#define rprmlc ((doublereal *)&lcpar2_1 + 30)
    extern /* Subroutine */ int lpnkey_(void);
#define msglvl ((integer *)&lcpar1_1 + 41)
    integer iprntr, isumry;

    /* Fortran I/O blocks */
    static cilist io___162 = { 0, 0, 0, fmt_2000, 0 };
    static cilist io___163 = { 0, 0, 0, fmt_2100, 0 };
    static cilist io___164 = { 0, 0, 0, fmt_2200, 0 };


/*     ================================================================== */
/*     lpdflt  loads the default values of parameters not set by the */
/*     user. */

/*     30 Dec 1986: first version. */
/*     04 Nov 2001: Current version of  lpdflt. */
/*     ================================================================== */
/*     +Include lcparm-Sep-95++++++++++++++++++++++++++++++++++++++++++++ */
/*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
    epsmch = solmch_1.wmach[2];
/*     Make a dummy call to lpnkey to ensure that the defaults are set. */
    lpnkey_();
    sol2lc_1.newopt = TRUE_;
/*     Save the optional parameters set by the user.  The values in */
/*     rprmlc and iprmlc may be changed to their default values. */
    icopy_(&c__30, iprmlc, &c__1, lcpar1_1.ipsvlc, &c__1);
    dcopy_(&c__30, rprmlc, &c__1, lcpar2_1.rpsvlc, &c__1);
    if (sol1cm_1.iprint < 0) {
	mcout_(&sol1cm_1.iprint, &isumry);
    }
    if (sol1cm_1.isumm < 0) {
	mcout_(&iprntr, &sol1cm_1.isumm);
    }
    if (sol1cm_1.isumm == sol1cm_1.iprint) {
	sol1cm_1.iprint = 0;
    }
    if (lcpar1_1.kchk <= 0) {
	lcpar1_1.kchk = 50;
    }
    if (lcpar1_1.kcycle <= 0) {
	lcpar1_1.kcycle = 5;
    }
    if (lcpar1_1.kcycle > 9999999) {
	lcpar1_1.kcycle = 9999999;
    }
    sol3lc_1.kdegen = lcpar1_1.kcycle;
    if (lcpar1_1.lprob < 0) {
	lcpar1_1.lprob = 2;
    }
    if (lcpar1_1.lcrash < 0 || lcpar1_1.lcrash > 2) {
	lcpar1_1.lcrash = 0;
    }
    if (lcpar1_1.itmax1 < 0) {
/* Computing MAX */
	i__1 = 50, i__2 = (*n + *nclin) * 5;
	lcpar1_1.itmax1 = max(i__1,i__2);
    }
    if (lcpar1_1.itmax2 < 0) {
/* Computing MAX */
	i__1 = 50, i__2 = (*n + *nclin) * 5;
	lcpar1_1.itmax2 = max(i__1,i__2);
    }
    if (lcpar1_1.maxact < 0 || lcpar1_1.maxact > *n || lcpar1_1.maxact > *
	    nclin) {
/* Computing MAX */
	i__1 = 1, i__2 = min(*n,*nclin);
	lcpar1_1.maxact = max(i__1,i__2);
    }
    if (lcpar1_1.maxnz < 0 || lcpar1_1.maxnz > *n) {
	lcpar1_1.maxnz = *n;
    }
    if (lcpar1_1.mxfree < 0 || lcpar1_1.mxfree > *n) {
	lcpar1_1.mxfree = *n;
    }
    if (lcpar1_1.mxfree < lcpar1_1.maxnz) {
	lcpar1_1.mxfree = lcpar1_1.maxnz;
    }
    if (lcpar1_1.minsum < 0) {
	lcpar1_1.minsum = 0;
    }
    if (*nclin < *n) {
	lcpar1_1.mxfree = *nclin + 1;
	lcpar1_1.maxnz = lcpar1_1.mxfree;
    }
    if (*msglvl == -11111) {
	*msglvl = 10;
    }
    if (lcpar2_1.tolact < 0.) {
	lcpar2_1.tolact = .01;
    }
    if (lcpar2_1.tolfea == -11111. || lcpar2_1.tolfea >= 0. && 
	    lcpar2_1.tolfea < epsmch) {
	lcpar2_1.tolfea = sol4cm_1.epspt5;
    }
    if (lcpar2_1.tolopt == -11111. || lcpar2_1.tolopt >= 0. && 
	    lcpar2_1.tolopt < epsmch) {
	lcpar2_1.tolopt = sol4cm_1.epspt5;
    }
    if (lcpar2_1.bigbnd <= 0.) {
	lcpar2_1.bigbnd = 9.9999e19;
    }
    if (lcpar2_1.bigdx <= 0.) {
	lcpar2_1.bigdx = max(9.9999e19,lcpar2_1.bigbnd);
    }
    if (*msglvl > 0) {
/*        ---------------- */
/*        Print the title. */
/*        ---------------- */
	lent = i_len(title, title_len);
	if (lent > 0) {
	    nspace = (81 - lent) / 2 + 1;
	    if (sol1cm_1.iprint > 0) {
		ci__1.cierr = 0;
		ci__1.ciunit = sol1cm_1.iprint;
		ci__1.cifmt = "(///// (80a1) )";
		s_wsfe(&ci__1);
		i__1 = nspace;
		for (j = 1; j <= i__1; ++j) {
		    do_fio(&c__1, " ", (ftnlen)1);
		}
		i__2 = lent;
		for (j = 1; j <= i__2; ++j) {
		    do_fio(&c__1, title + (j - 1), (ftnlen)1);
		}
		e_wsfe();
		ci__1.cierr = 0;
		ci__1.ciunit = sol1cm_1.iprint;
		ci__1.cifmt = "(80a1 //)";
		s_wsfe(&ci__1);
		i__1 = nspace;
		for (j = 1; j <= i__1; ++j) {
		    do_fio(&c__1, " ", (ftnlen)1);
		}
		i__2 = lent;
		for (j = 1; j <= i__2; ++j) {
		    do_fio(&c__1, "=", (ftnlen)1);
		}
		e_wsfe();
	    }
	    if (sol1cm_1.isumm > 0) {
		ci__1.cierr = 0;
		ci__1.ciunit = sol1cm_1.isumm;
		ci__1.cifmt = "(///// (80a1) )";
		s_wsfe(&ci__1);
		i__1 = nspace;
		for (j = 1; j <= i__1; ++j) {
		    do_fio(&c__1, " ", (ftnlen)1);
		}
		i__2 = lent;
		for (j = 1; j <= i__2; ++j) {
		    do_fio(&c__1, title + (j - 1), (ftnlen)1);
		}
		e_wsfe();
		ci__1.cierr = 0;
		ci__1.ciunit = sol1cm_1.isumm;
		ci__1.cifmt = "(80a1 //)";
		s_wsfe(&ci__1);
		i__1 = nspace;
		for (j = 1; j <= i__1; ++j) {
		    do_fio(&c__1, " ", (ftnlen)1);
		}
		i__2 = lent;
		for (j = 1; j <= i__2; ++j) {
		    do_fio(&c__1, "=", (ftnlen)1);
		}
		e_wsfe();
	    }
	}
	if (sol1cm_1.iprint > 0) {
	    io___162.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___162);
	    e_wsfe();
	    io___163.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___163);
	    do_fio(&c__1, lptype + (lcpar1_1.lprob - 1) * 7, (ftnlen)7);
	    do_fio(&c__1, (char *)&(*nclin), (ftnlen)sizeof(integer));
	    do_fio(&c__1, icrsh + (lcpar1_1.lcrash << 2), (ftnlen)4);
	    do_fio(&c__1, noyes + lcpar1_1.minsum * 3, (ftnlen)3);
	    do_fio(&c__1, (char *)&(*n), (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&lcpar2_1.bigbnd, (ftnlen)sizeof(doublereal)
		    );
	    do_fio(&c__1, (char *)&lcpar2_1.tolfea, (ftnlen)sizeof(doublereal)
		    );
	    do_fio(&c__1, (char *)&lcpar1_1.kchk, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&lcpar2_1.bigdx, (ftnlen)sizeof(doublereal))
		    ;
	    do_fio(&c__1, (char *)&lcpar2_1.tolopt, (ftnlen)sizeof(doublereal)
		    );
	    do_fio(&c__1, (char *)&lcpar1_1.maxnz, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&sol3lc_1.kdegen, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&lcpar2_1.tolact, (ftnlen)sizeof(doublereal)
		    );
	    do_fio(&c__1, (char *)&lcpar1_1.mxfree, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&lcpar1_1.maxact, (ftnlen)sizeof(integer));
	    e_wsfe();
	    io___164.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___164);
	    do_fio(&c__1, (char *)&(*msglvl), (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&sol1cm_1.iprint, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&lcpar1_1.itmax1, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&epsmch, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&sol1cm_1.isumm, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&lcpar1_1.itmax2, (ftnlen)sizeof(integer));
	    e_wsfe();
	}
    }
    return 0;
/*     end of lpdflt */
} /* lpdflt_ */

#undef msglvl
#undef rprmlc
#undef iprmlc


/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int lpkey_(integer *iprint, integer *isumm, logical *listop, 
	char *buffer, char *key, ftnlen buffer_len, ftnlen key_len)
{
    /* Initialized data */

    static char keys[16*48] = "BEGIN           " "CENTRAL         " "CHEAP  "
	    "         " "CHECK           " "COLD            " "CONDITION     "
	    "  " "CONSTRAINTS     " "CRASH           " "DEFAULTS        " 
	    "DERIVATIVE      " "DIFFERENCE      " "END             " "EXPAND"
	    "          " "FEASIBILITY     " "FUNCTION        " "HESSIAN      "
	    "   " "HOT             " "INFINITE        " "IPRMLC          " 
	    "ITERATIONS      " "ITERS:ITERATIONS" "ITNS :ITERATIONS" "LINEAR"
	    "          " "LIST            " "LINESEARCH      " "LOWER        "
	    "   " "MAX.    :MAXIMUM" "MAX     :MAXIMUM" "MAXIMUM         " 
	    "MIN.    :MINIMIZ" "MIN     :MINIMIZ" "MINIMIZ         " "NO    "
	    "          " "NOLIST          " "OPTIMALITY      " "PRINT        "
	    "   " "PROBLEM         " "RANK            " "RPRMLC          " 
	    "SAVE            " "START           " "STEP            " "STOP  "
	    "          " "SUMMARY         " "UPPER           " "VARIABLES    "
	    "   " "VERIFY          " "WARM            ";
    static char ties[16*22] = "ACTIVE          " "BOUND           " "CONSTRA"
	    "INTS     " "DEGREES         " "FEASIBILITY     " "FILE          "
	    "  " "FREE            " "FREQUENCY       " "GRADIENTS       " 
	    "LEVEL           " "LINESEARCH      " "NO              " "NO.   "
	    "   :NUMBER" "NUMBER          " "OBJECTIVE       " "PHASE        "
	    "   " "RUN             " "STEP            " "SUM             " 
	    "TOLERANCE       " "TYPE            " "YES             ";
    static char type__[16*10] = "FP              " "LC              " "LINEA"
	    "R       :LP" "LP              " "QP          :QP2" "QP1         "
	    "    " "QP2             " "QP3             " "QP4             " 
	    "QUADRATIC   :QP2";

    /* Format strings */
    static char fmt_2320[] = "(\002 XXX  Second keyword not recognized:  "
	    "\002,a)";
    static char fmt_2400[] = "(\002 XXX  The PARM subscript is out of range"
	    ":\002,i10)";
    static char fmt_2330[] = "(\002 XXX  Third  keyword not recognized:  "
	    "\002,a)";
    static char fmt_2300[] = "(\002 XXX  Keyword not recognized:         "
	    "\002,a)";

    /* System generated locals */
    icilist ici__1;

    /* Builtin functions */
    integer i_indx(char *, char *, ftnlen, ftnlen), i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rsfi(icilist *), do_fio(
	    integer *, char *, ftnlen), e_rsfi(void), s_wsfe(cilist *), 
	    e_wsfe(void);

    /* Local variables */
    integer ndls;
    logical more;
    integer i__;
    doublereal cdint, fdint;
    char value[16];
    doublereal dxlim, epsrf;
    char token[16*10];
    extern /* Subroutine */ int mcout_(integer *, integer *);
    integer jvrfy1;
    doublereal hcndbd;
    integer jvrfy2, lenbuf, ldifgz;
#define iprmlc ((integer *)&lcpar1_1 + 30)
    logical number;
    integer lvlder, ivalue;
#define rprmlc ((doublereal *)&lcpar2_1 + 30)
    extern logical opnumb_(char *, ftnlen);
#define msglvl ((integer *)&lcpar1_1 + 41)
    integer ntoken;
    extern /* Subroutine */ int optokn_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    doublereal rvalue;
    extern /* Subroutine */ int oplook_(integer *, char *, logical *, char *, 
	    integer *, ftnlen, ftnlen);
    integer lverfy;
    doublereal eta;
    integer loc1, loc2, loc3;
    char key2[16], key3[16];

    /* Fortran I/O blocks */
    static cilist io___188 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___189 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___191 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___192 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___194 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___195 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___197 = { 0, 0, 0, fmt_2400, 0 };
    static cilist io___198 = { 0, 0, 0, fmt_2400, 0 };
    static cilist io___199 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___200 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___202 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___203 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___204 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___205 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___206 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___207 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___209 = { 0, 0, 0, fmt_2330, 0 };
    static cilist io___210 = { 0, 0, 0, fmt_2330, 0 };
    static cilist io___211 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___212 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___213 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___214 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___215 = { 0, 0, 0, fmt_2400, 0 };
    static cilist io___216 = { 0, 0, 0, fmt_2400, 0 };
    static cilist io___218 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___219 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___222 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___223 = { 0, 0, 0, fmt_2320, 0 };
    static cilist io___225 = { 0, 0, 0, fmt_2300, 0 };
    static cilist io___226 = { 0, 0, 0, fmt_2300, 0 };


/*     ================================================================== */
/*     lpkey   decodes the option contained in  buffer  in order to set */
/*     a parameter value in the relevant element of  iprmlc  or  rprmlc. */


/*     Input: */
/*        iPrint   the print   file for error messages */
/*        iSumm    the summary file for error messages. */
/*     Output: */
/*        key    The first keyword contained in buffer. */

/*        lpkey  calls opnumb and the subprograms */
/*               lookup, scannrl tokens, upcase */
/*        (now called oplook, opscan, optokn, opuppr) */
/*        supplied by Informatics General, Inc., Palo Alto, California. */

/*     This version of lpkey dated 14-Sep-95. */
/*     (C) 1992--1997  Regents of the University of California and the */
/*                     Trustees of Stanford University */
/*     ================================================================== */
/*     +Include lcparm-Sep-95++++++++++++++++++++++++++++++++++++++++++++ */
/*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* ----------------------------------------------------------------------- */
/*     Eliminate comments and empty lines. */
/*     A '*' appearing anywhere in BUFFER terminates the string. */
    i__ = i_indx(buffer, "*", buffer_len, (ftnlen)1);
    if (i__ == 0) {
	lenbuf = i_len(buffer, buffer_len);
    } else {
	lenbuf = i__ - 1;
    }
    if (lenbuf <= 0) {
	s_copy(key, "*", (ftnlen)16, (ftnlen)1);
	goto L900;
    }
/*     ------------------------------------------------------------------ */
/*     Extract up to maxtok tokens from the record. */
/*     ntoken returns how many were actually found. */
/*     key, key2, key3 are the first tokens if any, otherwise blank. */
/*     ------------------------------------------------------------------ */
    ntoken = 10;
    optokn_(buffer, &ntoken, token, lenbuf, (ftnlen)16);
    s_copy(key, token, (ftnlen)16, (ftnlen)16);
    s_copy(key2, token + 16, (ftnlen)16, (ftnlen)16);
    s_copy(key3, token + 32, (ftnlen)16, (ftnlen)16);
/*     Certain keywords require no action. */
    if (s_cmp(key, " ", (ftnlen)16, (ftnlen)1) == 0 || s_cmp(key, "BEGIN", (
	    ftnlen)16, (ftnlen)5) == 0) {
	goto L900;
    }
    if (s_cmp(key, "LIST", (ftnlen)16, (ftnlen)4) == 0 || s_cmp(key, "NOLIST",
	     (ftnlen)16, (ftnlen)6) == 0) {
	goto L900;
    }
    if (s_cmp(key, "END", (ftnlen)16, (ftnlen)3) == 0) {
	goto L900;
    }
/*     Most keywords will have an associated integer or real value, */
/*     so look for it no matter what the keyword. */
    i__ = 1;
    number = FALSE_;
L50:
    if (i__ < ntoken && ! number) {
	++i__;
	s_copy(value, token + (i__ - 1 << 4), (ftnlen)16, (ftnlen)16);
	number = opnumb_(value, (ftnlen)16);
	goto L50;
    }
    if (number) {
	ici__1.icierr = 0;
	ici__1.iciend = 0;
	ici__1.icirnum = 1;
	ici__1.icirlen = 16;
	ici__1.iciunit = value;
	ici__1.icifmt = "(bn, e16.0)";
	s_rsfi(&ici__1);
	do_fio(&c__1, (char *)&rvalue, (ftnlen)sizeof(doublereal));
	e_rsfi();
    } else {
	rvalue = 0.;
    }
/*     Convert the keywords to their most fundamental form */
/*     (upper case, no abbreviations). */
/*     SORTED says whether the dictionaries are in alphabetic order. */
/*     LOCi   says where the keywords are in the dictionaries. */
/*     LOCi = 0 signals that the keyword wasn't there. */
    oplook_(&c__48, keys, &c_true, key, &loc1, (ftnlen)16, (ftnlen)16);
    oplook_(&c__22, ties, &c_true, key2, &loc2, (ftnlen)16, (ftnlen)16);
/*     ------------------------------------------------------------------ */
/*     Decide what to do about each keyword. */
/*     The second keyword (if any) might be needed to break ties. */
/*     Some seemingly redundant testing of MORE is used */
/*     to avoid compiler limits on the number of consecutive ELSE IFs. */
/*     ------------------------------------------------------------------ */
    more = TRUE_;
    if (more) {
	more = FALSE_;
	if (s_cmp(key, "CENTRAL     ", (ftnlen)16, (ftnlen)12) == 0) {
	    cdint = rvalue;
	} else if (s_cmp(key, "CHEAP       ", (ftnlen)16, (ftnlen)12) == 0) {
	    ldifgz = (integer) rvalue;
	} else if (s_cmp(key, "CHECK       ", (ftnlen)16, (ftnlen)12) == 0) {
	    lcpar1_1.kchk = (integer) rvalue;
	} else if (s_cmp(key, "COLD        ", (ftnlen)16, (ftnlen)12) == 0) {
	    lcpar1_1.lcrash = 0;
	} else if (s_cmp(key, "CONDITION   ", (ftnlen)16, (ftnlen)12) == 0) {
	    hcndbd = rvalue;
	} else if (s_cmp(key, "CONSTRAINTS ", (ftnlen)16, (ftnlen)12) == 0) {
	    lcpar1_1.nnclin = (integer) rvalue;
	} else if (s_cmp(key, "CRASH       ", (ftnlen)16, (ftnlen)12) == 0) {
	    lcpar2_1.tolact = rvalue;
	} else if (s_cmp(key, "DEFAULTS    ", (ftnlen)16, (ftnlen)12) == 0) {
	    mcout_(iprint, isumm);
	    *listop = TRUE_;
	    for (i__ = 1; i__ <= 30; ++i__) {
		iprmlc[i__ - 1] = -11111;
		rprmlc[i__ - 1] = -11111.;
/* L20: */
	    }
	} else if (s_cmp(key, "DERIVATIVE  ", (ftnlen)16, (ftnlen)12) == 0) {
	    if (s_cmp(key2, "LEVEL       ", (ftnlen)16, (ftnlen)12) == 0) {
		lvlder = (integer) rvalue;
	    }
	    if (s_cmp(key2, "LINESEARCH  ", (ftnlen)16, (ftnlen)12) == 0) {
		ndls = 0;
	    }
	    if (loc2 == 0) {
		if (*iprint > 0) {
		    io___188.ciunit = *iprint;
		    s_wsfe(&io___188);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
		if (*isumm > 0) {
		    io___189.ciunit = *isumm;
		    s_wsfe(&io___189);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
	    }
	} else if (s_cmp(key, "DIFFERENCE  ", (ftnlen)16, (ftnlen)12) == 0) {
	    fdint = rvalue;
	} else if (s_cmp(key, "EXPAND      ", (ftnlen)16, (ftnlen)12) == 0) {
	    lcpar1_1.kcycle = (integer) rvalue;
	} else if (s_cmp(key, "FEASIBILITY ", (ftnlen)16, (ftnlen)12) == 0) {
	    if (s_cmp(key2, "PHASE       ", (ftnlen)16, (ftnlen)12) == 0) {
		lcpar1_1.itmax1 = (integer) rvalue;
	    }
	    if (s_cmp(key2, "TOLERANCE   ", (ftnlen)16, (ftnlen)12) == 0) {
		lcpar2_1.tolfea = rvalue;
	    }
	    if (loc2 == 0) {
		if (*iprint > 0) {
		    io___191.ciunit = *iprint;
		    s_wsfe(&io___191);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
		if (*isumm > 0) {
		    io___192.ciunit = *isumm;
		    s_wsfe(&io___192);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
	    }
	} else if (s_cmp(key, "FUNCTION    ", (ftnlen)16, (ftnlen)12) == 0) {
	    epsrf = rvalue;
	} else {
	    more = TRUE_;
	}
    }
    if (more) {
	more = FALSE_;
	if (s_cmp(key, "HESSIAN     ", (ftnlen)16, (ftnlen)12) == 0) {
	    lcpar1_1.mm = (integer) rvalue;
	} else if (s_cmp(key, "HOT         ", (ftnlen)16, (ftnlen)12) == 0) {
	    lcpar1_1.lcrash = 2;
	} else if (s_cmp(key, "INFINITE    ", (ftnlen)16, (ftnlen)12) == 0) {
	    if (s_cmp(key2, "BOUND       ", (ftnlen)16, (ftnlen)12) == 0) {
		lcpar2_1.bigbnd = rvalue * .99999;
	    }
	    if (s_cmp(key2, "STEP        ", (ftnlen)16, (ftnlen)12) == 0) {
		lcpar2_1.bigdx = rvalue;
	    }
	    if (loc2 == 0) {
		if (*iprint > 0) {
		    io___194.ciunit = *iprint;
		    s_wsfe(&io___194);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
		if (*isumm > 0) {
		    io___195.ciunit = *isumm;
		    s_wsfe(&io___195);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
	    }
	} else if (s_cmp(key, "IPRMLC      ", (ftnlen)16, (ftnlen)12) == 0) {
/*           Allow things like  IPRMLC 21 = 100  to set IPRMLC(21) = 100 */
	    ivalue = (integer) rvalue;
	    if (ivalue >= 1 && ivalue <= 30) {
		ici__1.icierr = 0;
		ici__1.iciend = 0;
		ici__1.icirnum = 1;
		ici__1.icirlen = 16;
		ici__1.iciunit = key3;
		ici__1.icifmt = "(bn, i16)";
		s_rsfi(&ici__1);
		do_fio(&c__1, (char *)&iprmlc[ivalue - 1], (ftnlen)sizeof(
			integer));
		e_rsfi();
	    } else {
		if (*iprint > 0) {
		    io___197.ciunit = *iprint;
		    s_wsfe(&io___197);
		    do_fio(&c__1, (char *)&ivalue, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
		if (*isumm > 0) {
		    io___198.ciunit = *isumm;
		    s_wsfe(&io___198);
		    do_fio(&c__1, (char *)&ivalue, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    }
	} else if (s_cmp(key, "ITERATIONS  ", (ftnlen)16, (ftnlen)12) == 0) {
	    lcpar1_1.itmax2 = (integer) rvalue;
	} else if (s_cmp(key, "LINEAR      ", (ftnlen)16, (ftnlen)12) == 0) {
	    if (s_cmp(key2, "CONSTRAINTS ", (ftnlen)16, (ftnlen)12) == 0) {
		lcpar1_1.nnclin = (integer) rvalue;
	    }
	    if (s_cmp(key2, "FEASIBILITY ", (ftnlen)16, (ftnlen)12) == 0) {
		lcpar2_1.tolfea = rvalue;
	    }
	    if (loc2 == 0) {
		if (*iprint > 0) {
		    io___199.ciunit = *iprint;
		    s_wsfe(&io___199);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
		if (*isumm > 0) {
		    io___200.ciunit = *isumm;
		    s_wsfe(&io___200);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
	    }
	} else if (s_cmp(key, "LINESEARCH  ", (ftnlen)16, (ftnlen)12) == 0) {
	    eta = rvalue;
	} else if (s_cmp(key, "LOWER       ", (ftnlen)16, (ftnlen)12) == 0) {
	    lcpar2_1.bndlow = rvalue;
	} else {
	    more = TRUE_;
	}
    }
    if (more) {
	more = FALSE_;
	if (s_cmp(key, "MAXIMUM     ", (ftnlen)16, (ftnlen)12) == 0) {
	    if (s_cmp(key2, "ACTIVE      ", (ftnlen)16, (ftnlen)12) == 0) {
		lcpar1_1.maxact = (integer) rvalue;
	    }
	    if (s_cmp(key2, "DEGREES     ", (ftnlen)16, (ftnlen)12) == 0) {
		lcpar1_1.maxnz = (integer) rvalue;
	    }
	    if (s_cmp(key2, "FREE        ", (ftnlen)16, (ftnlen)12) == 0) {
		lcpar1_1.mxfree = (integer) rvalue;
	    }
	    if (loc2 == 0) {
		if (*iprint > 0) {
		    io___202.ciunit = *iprint;
		    s_wsfe(&io___202);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
		if (*isumm > 0) {
		    io___203.ciunit = *isumm;
		    s_wsfe(&io___203);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
	    }
	} else if (s_cmp(key, "MINIMIZ     ", (ftnlen)16, (ftnlen)12) == 0) {
	    if (s_cmp(key3, "YES         ", (ftnlen)16, (ftnlen)12) == 0) {
		lcpar1_1.minsum = 1;
	    }
	    if (s_cmp(key3, "NO          ", (ftnlen)16, (ftnlen)12) == 0) {
		lcpar1_1.minsum = 0;
	    }
	    if (loc2 == 0) {
		if (*iprint > 0) {
		    io___204.ciunit = *iprint;
		    s_wsfe(&io___204);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
		if (*isumm > 0) {
		    io___205.ciunit = *isumm;
		    s_wsfe(&io___205);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
	    }
	} else if (s_cmp(key, "NO          ", (ftnlen)16, (ftnlen)12) == 0) {
	    ndls = 1;
	} else if (s_cmp(key, "OPTIMALITY  ", (ftnlen)16, (ftnlen)12) == 0) {
	    if (s_cmp(key2, "PHASE       ", (ftnlen)16, (ftnlen)12) == 0) {
		lcpar1_1.itmax2 = (integer) rvalue;
	    }
	    if (s_cmp(key2, "TOLERANCE   ", (ftnlen)16, (ftnlen)12) == 0) {
		lcpar2_1.tolopt = rvalue;
	    }
	    if (loc2 == 0) {
		if (*iprint > 0) {
		    io___206.ciunit = *iprint;
		    s_wsfe(&io___206);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
		if (*isumm > 0) {
		    io___207.ciunit = *isumm;
		    s_wsfe(&io___207);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
	    }
	} else if (s_cmp(key, "PROBLEM     ", (ftnlen)16, (ftnlen)12) == 0) {
	    if (s_cmp(key2, "NUMBER", (ftnlen)16, (ftnlen)6) == 0) {
		lcpar1_1.nprob = (integer) rvalue;
	    } else if (s_cmp(key2, "TYPE  ", (ftnlen)16, (ftnlen)6) == 0) {
/*              Recognize     Problem type = LP     etc. */
		oplook_(&c__10, type__, &c_true, key3, &loc3, (ftnlen)16, (
			ftnlen)16);
		if (s_cmp(key3, "FP", (ftnlen)16, (ftnlen)2) == 0) {
		    lcpar1_1.lprob = 1;
		}
		if (s_cmp(key3, "LP", (ftnlen)16, (ftnlen)2) == 0) {
		    lcpar1_1.lprob = 2;
		}
		if (s_cmp(key3, "QP1", (ftnlen)16, (ftnlen)3) == 0) {
		    lcpar1_1.lprob = 3;
		}
		if (s_cmp(key3, "QP2", (ftnlen)16, (ftnlen)3) == 0) {
		    lcpar1_1.lprob = 4;
		}
		if (s_cmp(key3, "QP3", (ftnlen)16, (ftnlen)3) == 0) {
		    lcpar1_1.lprob = 5;
		}
		if (s_cmp(key3, "QP4", (ftnlen)16, (ftnlen)3) == 0) {
		    lcpar1_1.lprob = 6;
		}
		if (s_cmp(key3, "LC ", (ftnlen)16, (ftnlen)3) == 0) {
		    lcpar1_1.lprob = 7;
		}
		if (loc3 == 0) {
		    if (*iprint > 0) {
			io___209.ciunit = *iprint;
			s_wsfe(&io___209);
			do_fio(&c__1, key3, (ftnlen)16);
			e_wsfe();
		    }
		    if (*isumm > 0) {
			io___210.ciunit = *isumm;
			s_wsfe(&io___210);
			do_fio(&c__1, key3, (ftnlen)16);
			e_wsfe();
		    }
		    lcpar1_1.lprob = 10;
		}
	    } else {
		if (*iprint > 0) {
		    io___211.ciunit = *iprint;
		    s_wsfe(&io___211);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
		if (*isumm > 0) {
		    io___212.ciunit = *isumm;
		    s_wsfe(&io___212);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
	    }
	} else {
	    more = TRUE_;
	}
    }
    if (more) {
	more = FALSE_;
	if (s_cmp(key, "PRINT       ", (ftnlen)16, (ftnlen)12) == 0) {
	    if (s_cmp(key2, "FILE        ", (ftnlen)16, (ftnlen)12) == 0) {
		*iprint = (integer) rvalue;
	    }
	    if (s_cmp(key2, "LEVEL       ", (ftnlen)16, (ftnlen)12) == 0) {
		*msglvl = (integer) rvalue;
	    }
	    if (loc2 == 0) {
		if (*iprint > 0) {
		    io___213.ciunit = *iprint;
		    s_wsfe(&io___213);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
		if (*isumm > 0) {
		    io___214.ciunit = *isumm;
		    s_wsfe(&io___214);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
	    }
	} else if (s_cmp(key, "RANK        ", (ftnlen)16, (ftnlen)12) == 0) {
	    lcpar2_1.tolrnk = rvalue;
	} else if (s_cmp(key, "RPRMLC      ", (ftnlen)16, (ftnlen)12) == 0) {
/*           Allow things like  RPRMLC 21 = 2  to set RPRMLC(21) = 2.0 */
	    ivalue = (integer) rvalue;
	    if (ivalue >= 1 && ivalue <= 30) {
		ici__1.icierr = 0;
		ici__1.iciend = 0;
		ici__1.icirnum = 1;
		ici__1.icirlen = 16;
		ici__1.iciunit = key3;
		ici__1.icifmt = "(bn, e16.0)";
		s_rsfi(&ici__1);
		do_fio(&c__1, (char *)&rprmlc[ivalue - 1], (ftnlen)sizeof(
			doublereal));
		e_rsfi();
	    } else {
		if (*iprint > 0) {
		    io___215.ciunit = *iprint;
		    s_wsfe(&io___215);
		    do_fio(&c__1, (char *)&ivalue, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
		if (*isumm > 0) {
		    io___216.ciunit = *isumm;
		    s_wsfe(&io___216);
		    do_fio(&c__1, (char *)&ivalue, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    }
	} else if (s_cmp(key, "START       ", (ftnlen)16, (ftnlen)12) == 0) {
	    if (s_cmp(key2, "OBJECTIVE   ", (ftnlen)16, (ftnlen)12) == 0) {
		jvrfy1 = (integer) rvalue;
	    }
	    if (loc2 == 0) {
		if (*iprint > 0) {
		    io___218.ciunit = *iprint;
		    s_wsfe(&io___218);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
		if (*isumm > 0) {
		    io___219.ciunit = *isumm;
		    s_wsfe(&io___219);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
	    }
	} else if (s_cmp(key, "STEP        ", (ftnlen)16, (ftnlen)12) == 0) {
	    dxlim = rvalue;
	} else if (s_cmp(key, "STOP        ", (ftnlen)16, (ftnlen)12) == 0) {
	    if (s_cmp(key2, "OBJECTIVE   ", (ftnlen)16, (ftnlen)12) == 0) {
		jvrfy2 = (integer) rvalue;
	    }
	    if (loc2 == 0) {
		if (*iprint > 0) {
		    io___222.ciunit = *iprint;
		    s_wsfe(&io___222);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
		if (*isumm > 0) {
		    io___223.ciunit = *isumm;
		    s_wsfe(&io___223);
		    do_fio(&c__1, key2, (ftnlen)16);
		    e_wsfe();
		}
	    }
	} else if (s_cmp(key, "SUMMARY     ", (ftnlen)16, (ftnlen)12) == 0) {
	    *isumm = (integer) rvalue;
	} else if (s_cmp(key, "UPPER       ", (ftnlen)16, (ftnlen)12) == 0) {
	    lcpar2_1.bndupp = rvalue;
	} else if (s_cmp(key, "VARIABLES   ", (ftnlen)16, (ftnlen)12) == 0) {
	    lcpar1_1.nn = (integer) rvalue;
	} else if (s_cmp(key, "VERIFY      ", (ftnlen)16, (ftnlen)12) == 0) {
	    if (s_cmp(key2, "OBJECTIVE   ", (ftnlen)16, (ftnlen)12) == 0) {
		lverfy = 1;
	    }
	    if (s_cmp(key2, "NO          ", (ftnlen)16, (ftnlen)12) == 0) {
		lverfy = -1;
	    }
	    if (s_cmp(key2, "YES         ", (ftnlen)16, (ftnlen)12) == 0) {
		lverfy = 3;
	    }
	    if (s_cmp(key2, "GRADIENTS   ", (ftnlen)16, (ftnlen)12) == 0) {
		lverfy = 3;
	    }
	    if (s_cmp(key2, "LEVEL       ", (ftnlen)16, (ftnlen)12) == 0) {
		lverfy = (integer) rvalue;
	    }
	    if (loc2 == 0) {
		lverfy = 3;
	    }
	} else if (s_cmp(key, "WARM        ", (ftnlen)16, (ftnlen)12) == 0) {
	    lcpar1_1.lcrash = 1;
	} else {
	    if (*iprint > 0) {
		io___225.ciunit = *iprint;
		s_wsfe(&io___225);
		do_fio(&c__1, key, (ftnlen)16);
		e_wsfe();
	    }
	    if (*isumm > 0) {
		io___226.ciunit = *isumm;
		s_wsfe(&io___226);
		do_fio(&c__1, key, (ftnlen)16);
		e_wsfe();
	    }
	}
    }
L900:
    return 0;
/*     end of lpkey */
} /* lpkey_ */

#undef msglvl
#undef rprmlc
#undef iprmlc


/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int lploc_(logical *cset, integer *n, integer *nclin, 
	integer *litotl, integer *lwtotl)
{
    integer lenq, minw, lwrk, lencq, lrlam, miniw, lenrt, ld, lq, lr, lt, 
	    lfeatu, lkactv, lanorm, lwtinf, lad, lcq, lgq, lkx;

/*     ================================================================== */
/*     lploc   allocates the addresses of the work arrays for lpcore. */

/*     Note that the arrays ( gq, cq ) lie in contiguous areas of */
/*     workspace. */

/*     Original version written  2-January-1987. */
/*     This version of  lploc  dated  18-Nov-1990. */
/*     (C) 1992--1997  Regents of the University of California and the */
/*                     Trustees of Stanford University */
/*     ================================================================== */
/*     ------------------------------------------------------------------ */
/*     Refer to the first free space in the work arrays. */
/*     ------------------------------------------------------------------ */
    miniw = *litotl + 1;
    minw = *lwtotl + 1;
/*     ------------------------------------------------------------------ */
/*     Integer workspace. */
/*     ------------------------------------------------------------------ */
    lkactv = miniw;
    lkx = lkactv + *n;
    miniw = lkx + *n;
/*     ------------------------------------------------------------------ */
/*     Real workspace. */
/*     Assign array lengths that depend upon the problem dimensions. */
/*     ------------------------------------------------------------------ */
    lenrt = sol3cm_1.ldt * sol3cm_1.ncolt;
    if (*nclin == 0) {
	lenq = 0;
    } else {
	lenq = sol3cm_1.ldq * sol3cm_1.ldq;
    }
    if (*cset) {
	lencq = *n;
    } else {
	lencq = 0;
    }
/*     ------------------------------------------------------------------ */
/*     We start with arrays that can be preloaded by smart users. */
/*     ------------------------------------------------------------------ */
    lfeatu = minw;
    minw = lfeatu + *nclin + *n;
/*     Next comes stuff used by  lpcore  and  qpcore. */
    lanorm = minw;
    lad = lanorm + *nclin;
    ld = lad + *nclin;
    lgq = ld + *n;
    lcq = lgq + *n;
    lrlam = lcq + lencq;
    lr = lrlam + *n;
    lt = lr;
    lq = lt + lenrt;
    lwtinf = lq + lenq;
    lwrk = lwtinf + *n + *nclin;
    minw = lwrk + *n + *nclin;
/*     Load the addresses in loclc. */
    sol1lc_1.loclc[0] = lkactv;
    sol1lc_1.loclc[1] = lkx;
    sol1lc_1.loclc[2] = lfeatu;
    sol1lc_1.loclc[3] = lanorm;
    sol1lc_1.loclc[4] = lad;
    sol1lc_1.loclc[6] = ld;
    sol1lc_1.loclc[7] = lgq;
    sol1lc_1.loclc[8] = lcq;
    sol1lc_1.loclc[9] = lrlam;
    sol1lc_1.loclc[10] = lr;
    sol1lc_1.loclc[11] = lt;
    sol1lc_1.loclc[12] = lq;
    sol1lc_1.loclc[13] = lwtinf;
    sol1lc_1.loclc[14] = lwrk;
    *litotl = miniw - 1;
    *lwtotl = minw - 1;
/*     end of lploc */
    return 0;
} /* lploc_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int lpnkey_(void)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int mcout_(integer *, integer *);
#define iprmlc ((integer *)&lcpar1_1 + 30)
#define rprmlc ((doublereal *)&lcpar2_1 + 30)

/*     ================================================================== */
/*     lpnkey  counts the number of consecutive calls of lpprm or lpprms. */

/*     Original version written  11-Sep-95, */
/*     This version of  lpnkey  dated  12-Sep-95. */
/*     (C) 1992--1997  Regents of the University of California and the */
/*                     Trustees of Stanford University */
/*     ================================================================== */
/*     +Include lcparm-Sep-95++++++++++++++++++++++++++++++++++++++++++++ */
/*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
    if (first) {
	sol2lc_1.ncalls = 0;
	first = FALSE_;
	sol2lc_1.newopt = TRUE_;
	sol2lc_1.listop = TRUE_;
	mcout_(&sol1cm_1.iprint, &sol1cm_1.isumm);
	for (i__ = 1; i__ <= 30; ++i__) {
	    iprmlc[i__ - 1] = -11111;
	    rprmlc[i__ - 1] = -11111.;
/* L10: */
	}
	first = FALSE_;
    }
    if (sol2lc_1.newopt) {
	sol2lc_1.ncalls = 1;
    } else {
	++sol2lc_1.ncalls;
    }
/*     end of lpnkey */
    return 0;
} /* lpnkey_ */

#undef rprmlc
#undef iprmlc


/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int lpprm_(char *string, ftnlen string_len)
{
    /* System generated locals */
    cilist ci__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_wsfe(cilist *), do_fio(
	    integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    extern /* Subroutine */ int lpkey_(integer *, integer *, logical *, char *
	    , char *, ftnlen, ftnlen);
    char buffer[72];
    extern /* Subroutine */ int lpnkey_(void);
    char key[16];

/*     ================================================================== */
/*     lpprm   loads the option supplied in  string  into the relevant */
/*     element of  iprmlc  or  rprmlc. */
/*     ================================================================== */
/*     ------------------------------------------------------------------ */
    s_copy(buffer, string, (ftnlen)72, string_len);
/*     If this is the first call of lpnkey, set newOpt and default values */
/*     of the optional parameters. The default is to list the options. */
/*     Increment ncalls, the number of calls of lpprms and lpprms for */
/*     this optimization. */
    lpnkey_();
/*     Call  lpkey  to decode the option and set the parameter value. */
/*     If required, print a heading at the start of a new run. */
/*     Note that the following call to lpkey may reset iPrint and iSumm. */
    lpkey_(&sol1cm_1.iprint, &sol1cm_1.isumm, &sol2lc_1.listop, buffer, key, (
	    ftnlen)72, (ftnlen)16);
    if (s_cmp(key, "LIST", (ftnlen)16, (ftnlen)4) == 0) {
	sol2lc_1.listop = TRUE_;
    }
    if (s_cmp(key, "NOLIST", (ftnlen)16, (ftnlen)6) == 0) {
	sol2lc_1.listop = FALSE_;
    }
    if (sol2lc_1.listop) {
	if (sol2lc_1.newopt) {
	    if (sol1cm_1.iprint > 0) {
		ci__1.cierr = 0;
		ci__1.ciunit = sol1cm_1.iprint;
		ci__1.cifmt = "(// a / a /)";
		s_wsfe(&ci__1);
		do_fio(&c__1, " Optional Parameters", (ftnlen)20);
		do_fio(&c__1, " -------------------", (ftnlen)20);
		e_wsfe();
	    }
	    sol2lc_1.newopt = FALSE_;
	}
	if (sol1cm_1.iprint > 0) {
	    ci__1.cierr = 0;
	    ci__1.ciunit = sol1cm_1.iprint;
	    ci__1.cifmt = "( 6x, a )";
	    s_wsfe(&ci__1);
	    do_fio(&c__1, buffer, (ftnlen)72);
	    e_wsfe();
	}
    }
/*     end of lpprm */
    return 0;
} /* lpprm_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int lpprmi_(char *string, integer *ivalue, ftnlen string_len)
{
    /* System generated locals */
    integer i__1;
    icilist ici__1;

    /* Builtin functions */
    integer s_wsfi(icilist *), do_fio(integer *, char *, ftnlen), e_wsfi(void)
	    , i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char buff72[72];
    extern /* Subroutine */ int lpprm_(char *, ftnlen);
    integer lenbuf;
    char key[16];

/*     ================================================================== */
/*     lpprmi decodes the option contained in  string // ivalue. */

/*     14 Sep 1995: first version. */
/*     ================================================================== */
    ici__1.icierr = 0;
    ici__1.icirnum = 1;
    ici__1.icirlen = 16;
    ici__1.iciunit = key;
    ici__1.icifmt = "(i16)";
    s_wsfi(&ici__1);
    do_fio(&c__1, (char *)&(*ivalue), (ftnlen)sizeof(integer));
    e_wsfi();
    lenbuf = i_len(string, string_len);
    s_copy(buff72, string, (ftnlen)72, string_len);
    i__1 = lenbuf;
    s_copy(buff72 + i__1, key, lenbuf + 16 - i__1, (ftnlen)16);
    lpprm_(buff72, (ftnlen)72);
/*     end of lpprmi */
    return 0;
} /* lpprmi_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int lpprmr_(char *string, doublereal *rvalue, ftnlen 
	string_len)
{
    /* System generated locals */
    integer i__1;
    icilist ici__1;

    /* Builtin functions */
    integer s_wsfi(icilist *), do_fio(integer *, char *, ftnlen), e_wsfi(void)
	    , i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char buff72[72];
    extern /* Subroutine */ int lpprm_(char *, ftnlen);
    integer lenbuf;
    char key[16];

/*     ================================================================== */
/*     lpprmr decodes the option contained in  string // rvalue. */

/*     14 Sep 1995: first version. */
/*     ================================================================== */
    ici__1.icierr = 0;
    ici__1.icirnum = 1;
    ici__1.icirlen = 16;
    ici__1.iciunit = key;
    ici__1.icifmt = "(1p, e16.8)";
    s_wsfi(&ici__1);
    do_fio(&c__1, (char *)&(*rvalue), (ftnlen)sizeof(doublereal));
    e_wsfi();
    lenbuf = i_len(string, string_len);
    s_copy(buff72, string, (ftnlen)72, string_len);
    i__1 = lenbuf;
    s_copy(buff72 + i__1, key, lenbuf + 16 - i__1, (ftnlen)16);
    lpprm_(buff72, (ftnlen)72);
/*     end of lpprmr */
    return 0;
} /* lpprmr_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int lpprms_(integer *ioptns, integer *inform__)
{
    extern /* Subroutine */ int lpkey_(integer *, integer *, logical *, char *
	    , char *, ftnlen, ftnlen), opfile_(integer *, integer *, integer *
	    , logical *, logical *, integer *, S_fp), lpnkey_(void);

/*     ================================================================== */
/*     lpprms  reads the options file from unit  iOptns  and loads the */
/*     options into the relevant elements of  iprmlc  and  rprmlc. */

/*     If  iOptns .lt. 0  or  iOptns .gt. 99  then no file is read, */
/*     otherwise the file associated with unit  iOptns  is read. */

/*     Output: */

/*         inform = 0  if a complete  options  file was found */
/*                     (starting with  begin  and ending with  end); */
/*                  1  if  iOptns .lt. 0  or  iOptns .gt. 99; */
/*                  2  if  begin  was found, but end-of-file */
/*                     occurred before  end  was found; */
/*                  3  if end-of-file occurred before  begin  or */
/*                     endrun  were found; */
/*                  4  if  endrun  was found before  begin. */
/*     ================================================================== */
/*     ------------------------------------------------------------------ */
/*     Update ncalls, the number of calls of lpprm and lpprms since the */
/*     start of this problem. */
/*     On the very first call, the default parameters are set. */
    lpnkey_();
    opfile_(ioptns, &sol1cm_1.iprint, &sol1cm_1.isumm, &sol2lc_1.listop, &
	    sol2lc_1.newopt, inform__, (S_fp)lpkey_);
/*     end of lpprms */
    return 0;
} /* lpprms_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int lpprnt_(char *prbtyp, logical *header, logical *rset, 
	integer *msglvl, integer *iter, integer *isdel, integer *jdel, 
	integer *jadd, integer *n, integer *nclin, integer *nactiv, integer *
	nfree, integer *nz, integer *nzr, integer *ldr, integer *ldt, integer 
	*istate, doublereal *alfa, doublereal *condrz, doublereal *condt, 
	doublereal *dzz, doublereal *gzrnrm, integer *numinf, doublereal *
	suminf, integer *notopt, doublereal *objlp, doublereal *trusml, 
	doublereal *ax, doublereal *r__, doublereal *t, doublereal *x, 
	doublereal *work, ftnlen prbtyp_len)
{
    /* Initialized data */

    static char lstate[2*6] = "  " "L " "U " "E " "F " "A ";

    /* Format strings */
    static char fmt_1000[] = "(///\002 \002,a2,\002 iteration\002,i5/\002 =="
	    "===============\002)";
    static char fmt_1200[] = "(//\002 Itn Jdel  Jadd     Step Ninf  Sinf/Obj"
	    "ective\002,\002 Norm gZ   Zr  Art  Bnd  Lin NOpt    Min Lm  Cond"
	    " T\002)";
    static char fmt_1700[] = "(i4,i5,a1,i5,a1,1p,e8.1,i5,e16.8,e8.1,2i5,2i5,"
	    "a15,e8.0)";
    static char fmt_1100[] = "(//\002 Itn Jdel  Jadd     Step Ninf  Sinf/Obj"
	    "ective\002,\002 Norm gZ   Zr  Art\002)";
    static char fmt_2000[] = "(/\002 Values and status of the \002,a2,\002 c"
	    "onstraints\002/\002 ---------------------------------------\002)";
    static char fmt_2100[] = "(/\002 Variables...\002/(1x,5(1p,e15.6,i5)))";
    static char fmt_2200[] = "(/\002 General linear constraints...\002/(1x,5"
	    "(1p,e15.6,i5)))";
    static char fmt_3000[] = "(/\002 Diagonals of \002,a2,\002 working set f"
	    "actor T\002/(1p,5e15.6))";
    static char fmt_3100[] = "(/\002 Diagonals of \002,a2,\002 triangle Rz  "
	    "      \002/(1p,5e15.6))";
    static char fmt_5000[] = "(///\002 -------------------------------------"
	    "--------------\002,\002-----------------------------------------"
	    "---\002)";

    /* System generated locals */
    integer r_dim1, r_offset, t_dim1, t_offset, i__1, i__2;
    icilist ici__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_wsfi(icilist *), e_wsfi(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char ladd[2];
    integer kadd, kdel;
    char ldel[2];
    integer j, k;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *);
    char lmchar[15];
    logical prthdr, newset;
    integer ndf;
    doublereal obj;
    integer itn;

    /* Fortran I/O blocks */
    static cilist io___259 = { 0, 0, 0, fmt_1000, 0 };
    static cilist io___270 = { 0, 0, 0, fmt_1200, 0 };
    static cilist io___271 = { 0, 0, 0, fmt_1700, 0 };
    static cilist io___272 = { 0, 0, 0, fmt_1100, 0 };
    static cilist io___273 = { 0, 0, 0, fmt_1700, 0 };
    static cilist io___274 = { 0, 0, 0, fmt_2000, 0 };
    static cilist io___275 = { 0, 0, 0, fmt_2100, 0 };
    static cilist io___277 = { 0, 0, 0, fmt_2200, 0 };
    static cilist io___279 = { 0, 0, 0, fmt_3000, 0 };
    static cilist io___280 = { 0, 0, 0, fmt_3100, 0 };
    static cilist io___281 = { 0, 0, 0, fmt_5000, 0 };


/*     ================================================================== */
/*     lpprnt  prints various levels of output for lpcore. */

/*           msg        cumulative result */
/*           ---        ----------------- */

/*       .le.  0        no output. */

/*       .eq.  1        nothing now (but full output later). */

/*       .eq.  5        one terse line of output. */

/*       .ge. 10        same as 5 (but full output later). */

/*       .ge. 20        constraint status,  x  and  ax. */

/*       .ge. 30        diagonals of  T  and  R. */


/*     Based on a version of lcprt, written by PEG, 16-February-1987. */
/*     This version of  lpprnt  dated  23-Dec-92. */
/*     (C) 1992--1997  Regents of the University of California and the */
/*                     Trustees of Stanford University */
/*     ================================================================== */
    /* Parameter adjustments */
    --work;
    --x;
    r_dim1 = *ldr;
    r_offset = r_dim1 + 1;
    r__ -= r_offset;
    t_dim1 = *ldt;
    t_offset = t_dim1 + 1;
    t -= t_offset;
    --istate;
    --ax;

    /* Function Body */
    if (*msglvl >= 15 && sol1cm_1.iprint > 0) {
	io___259.ciunit = sol1cm_1.iprint;
	s_wsfe(&io___259);
	do_fio(&c__1, prbtyp, (ftnlen)2);
	do_fio(&c__1, (char *)&(*iter), (ftnlen)sizeof(integer));
	e_wsfe();
    }
    if (*msglvl >= 5) {
/*        --------------------------------------------------------------- */
/*        Some printing required.  Set up information for the terse line. */
/*        --------------------------------------------------------------- */
	itn = *iter % 1000;
	ndf = *nzr % 10000;
	if (*jdel != 0) {
	    if (*notopt > 0) {
		ici__1.icierr = 0;
		ici__1.icirnum = 1;
		ici__1.icirlen = 15;
		ici__1.iciunit = lmchar;
		ici__1.icifmt = "( i5, 1p,e10.2 )";
		s_wsfi(&ici__1);
		do_fio(&c__1, (char *)&(*notopt), (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&(*trusml), (ftnlen)sizeof(doublereal));
		e_wsfi();
	    } else {
		ici__1.icierr = 0;
		ici__1.icirnum = 1;
		ici__1.icirlen = 15;
		ici__1.iciunit = lmchar;
		ici__1.icifmt = "( 5x, 1p,e10.2 )";
		s_wsfi(&ici__1);
		do_fio(&c__1, (char *)&(*trusml), (ftnlen)sizeof(doublereal));
		e_wsfi();
	    }
	    if (*jdel > 0) {
		kdel = *isdel;
	    } else if (*jdel < 0) {
		*jdel = *nz - *nzr + 1;
		kdel = 5;
	    }
	} else {
	    *jdel = 0;
	    kdel = 0;
	    s_copy(lmchar, "               ", (ftnlen)15, (ftnlen)15);
	}
	if (*jadd > 0) {
	    kadd = istate[*jadd];
	} else {
	    kadd = 0;
	}
	s_copy(ldel, lstate + (kdel << 1), (ftnlen)2, (ftnlen)2);
	s_copy(ladd, lstate + (kadd << 1), (ftnlen)2, (ftnlen)2);
	if (*numinf > 0) {
	    obj = *suminf;
	} else {
	    obj = *objlp;
	}
/*        --------------------------------------------------------------- */
/*        If necessary, print a header. */
/*        Print a single line of information. */
/*        --------------------------------------------------------------- */
	if (sol1cm_1.iprint > 0) {
/*           ------------------------------ */
/*           Terse line for the Print file. */
/*           ------------------------------ */
	    newset = sol1cm_1.lines1 >= 40;
	    prthdr = *msglvl >= 15 || *header || newset;
	    if (prthdr) {
		io___270.ciunit = sol1cm_1.iprint;
		s_wsfe(&io___270);
		e_wsfe();
		sol1cm_1.lines1 = 0;
	    }
	    io___271.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___271);
	    do_fio(&c__1, (char *)&itn, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&(*jdel), (ftnlen)sizeof(integer));
	    do_fio(&c__1, ldel, (ftnlen)2);
	    do_fio(&c__1, (char *)&(*jadd), (ftnlen)sizeof(integer));
	    do_fio(&c__1, ladd, (ftnlen)2);
	    do_fio(&c__1, (char *)&(*alfa), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&(*numinf), (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&obj, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&(*gzrnrm), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&ndf, (ftnlen)sizeof(integer));
	    i__1 = *nz - *nzr;
	    do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
	    i__2 = *n - *nfree;
	    do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&(*nactiv), (ftnlen)sizeof(integer));
	    do_fio(&c__1, lmchar, (ftnlen)15);
	    do_fio(&c__1, (char *)&(*condt), (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    ++sol1cm_1.lines1;
	}
	if (sol1cm_1.isumm > 0) {
/*           -------------------------------- */
/*           Terse line for the Summary file. */
/*           -------------------------------- */
	    newset = sol1cm_1.lines2 >= 5;
	    prthdr = *header || newset;
	    if (prthdr) {
		io___272.ciunit = sol1cm_1.isumm;
		s_wsfe(&io___272);
		e_wsfe();
		sol1cm_1.lines2 = 0;
	    }
	    io___273.ciunit = sol1cm_1.isumm;
	    s_wsfe(&io___273);
	    do_fio(&c__1, (char *)&itn, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&(*jdel), (ftnlen)sizeof(integer));
	    do_fio(&c__1, ldel, (ftnlen)2);
	    do_fio(&c__1, (char *)&(*jadd), (ftnlen)sizeof(integer));
	    do_fio(&c__1, ladd, (ftnlen)2);
	    do_fio(&c__1, (char *)&(*alfa), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&(*numinf), (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&obj, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&(*gzrnrm), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&ndf, (ftnlen)sizeof(integer));
	    i__1 = *nz - *nzr;
	    do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
	    e_wsfe();
	    ++sol1cm_1.lines2;
	}
	if (*msglvl >= 20 && sol1cm_1.iprint > 0) {
	    io___274.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___274);
	    do_fio(&c__1, prbtyp, (ftnlen)2);
	    e_wsfe();
	    io___275.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___275);
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		do_fio(&c__1, (char *)&x[j], (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&istate[j], (ftnlen)sizeof(integer));
	    }
	    e_wsfe();
	    if (*nclin > 0) {
		io___277.ciunit = sol1cm_1.iprint;
		s_wsfe(&io___277);
		i__1 = *nclin;
		for (k = 1; k <= i__1; ++k) {
		    do_fio(&c__1, (char *)&ax[k], (ftnlen)sizeof(doublereal));
		    do_fio(&c__1, (char *)&istate[*n + k], (ftnlen)sizeof(
			    integer));
		}
		e_wsfe();
	    }
	    if (*msglvl >= 30) {
/*              --------------------------------------------------------- */
/*              Print the diagonals of  T  and  R. */
/*              --------------------------------------------------------- */
		if (*nactiv > 0) {
		    i__1 = *ldt + 1;
		    dcopy_(nactiv, &t[(*nz + 1) * t_dim1 + 1], &i__1, &work[1]
			    , &c__1);
		    io___279.ciunit = sol1cm_1.iprint;
		    s_wsfe(&io___279);
		    do_fio(&c__1, prbtyp, (ftnlen)2);
		    i__1 = *nactiv;
		    for (j = 1; j <= i__1; ++j) {
			do_fio(&c__1, (char *)&work[j], (ftnlen)sizeof(
				doublereal));
		    }
		    e_wsfe();
		}
		if (*rset && *nzr > 0) {
		    io___280.ciunit = sol1cm_1.iprint;
		    s_wsfe(&io___280);
		    do_fio(&c__1, prbtyp, (ftnlen)2);
		    i__1 = *nzr;
		    for (j = 1; j <= i__1; ++j) {
			do_fio(&c__1, (char *)&r__[j + j * r_dim1], (ftnlen)
				sizeof(doublereal));
		    }
		    e_wsfe();
		}
	    }
	    io___281.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___281);
	    e_wsfe();
	}
    }
    *header = FALSE_;
    *jdel = 0;
    *jadd = 0;
    *alfa = 0.;
    return 0;
/*     end of lpprnt */
} /* lpprnt_ */


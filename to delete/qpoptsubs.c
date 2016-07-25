/* ../src/qpoptsubs.f -- translated by f2c (version 19980913).
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
    integer lqptyp, mhess;
} sol1qp_;

#define sol1qp_1 sol1qp_

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
static doublereal c_b44 = 1.;
static logical c_true = TRUE_;
static integer c__30 = 30;
static doublereal c_b132 = 0.;
static integer c__4 = 4;
static doublereal c_b198 = -1.;

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*                                                                      * */
/*    Q P O P T         Version 1.0-10(2)       November 4, 2001        * */
/*                                                                      * */
/*    Philip E. Gill    Walter  Murray          Michael A. Saunders     * */
/*    UC San Diego      Stanford University     Stanford University     * */
/*                                                                      * */
/* ----------------------------------------------------------------------* */
/*     (C) 1992--1997  Regents of the University of California          * */
/*                     and the Trustees of Stanford University          * */
/*                                                                      * */
/*     This software is NOT in the public domain. Its use is governed   * */
/*     by a license agreement with either Stanford University or the    * */
/*     University of California.  It is a breach of copyright to make   * */
/*     copies except as authorized by the license agreement.            * */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*     File  qpoptsubs.f */

/*     qpopt    qpcolr   qpcore   qpcrsh   qpdflt   qpgetd   qpHess */
/*     qploc    qpprm    qpprmi   qpprmr   qpprms   qpprnt */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int qpopt(integer* n) { return 0; }

/* Subroutine */ int qpopt1_(integer *n, integer *nclin, integer *lda, integer 
	*ldh, doublereal *a, doublereal *bl, doublereal *bu, doublereal *cvec,
	 doublereal *h__, S_fp qphess, integer *istate, doublereal *x, 
	integer *inform__, integer *iter, doublereal *obj, doublereal *ax, 
	doublereal *clamda, integer *iw, integer *leniw, doublereal *w, 
	integer *lenw)
{
    /* Initialized data */

    static char title[40] = "QPOPT  ---  Version 1.0-10(2)  Nov  2001";

    /* Format strings */
    static char fmt_1000[] = "(\002 Itn\002,i6,\002 -- Feasible point found"
	    ".\002)";
    static char fmt_2001[] = "(/\002 Exit QPOPT - Feasible point found.    "
	    " \002)";
    static char fmt_2002[] = "(/\002 Exit QPOPT - Optimal \002,a2,\002 solut"
	    "ion.\002)";
    static char fmt_2010[] = "(/\002 Exit QPOPT - Iterations terminated at a"
	    " dead-point\002,\002 (check the optimality conditions).     \002)"
	    ;
    static char fmt_2015[] = "(\002            - Artificial constraints in w"
	    "orking set = \002,i4)";
    static char fmt_2011[] = "(/\002 Exit QPOPT - Optimal solution is not un"
	    "ique.\002)";
    static char fmt_2020[] = "(/\002 Exit QPOPT - \002,a2,\002 solution is u"
	    "nbounded.\002)";
    static char fmt_2030[] = "(/\002 Exit QPOPT - No feasible point for the "
	    "linear\002,\002 constraints.\002)";
    static char fmt_2035[] = "(/\002 Exit QPOPT - Cannot satisfy the constra"
	    "ints to the\002,\002 accuracy requested.\002)";
    static char fmt_2040[] = "(/\002 Exit QPOPT - Too many iterations.\002)";
    static char fmt_2050[] = "(/\002 Exit QPOPT - Reduced Hessian exceeds as"
	    "signed\002,\002 dimension.   maxnZ = \002,i4/\002 Problem abando"
	    "ned.\002)";
    static char fmt_2060[] = "(/\002 Exit QPOPT - \002,i10,\002 errors found"
	    " in the input\002,\002 parameters.  Problem abandoned.\002)";
    static char fmt_2070[] = "(\002 Exit QPOPT - Problem type not recogniz"
	    "ed.\002/\002 Problem abandoned.\002)";
    static char fmt_3000[] = "(/\002 Final \002,a2,\002 objective value ="
	    "\002,g16.7)";
    static char fmt_3010[] = "(/\002 Sum of infeasibilities =\002,g16.7)";
    static char fmt_3011[] = "(/\002 Minimum sum of infeasibilities =\002,g1"
	    "6.7)";
    static char fmt_3015[] = "(/\002 Maximum row error =\002,g16.7)";
    static char fmt_3020[] = "(/\002 Final sum of infeasibilities =\002,g16."
	    "7)";
    static char fmt_2000[] = "(/\002 Exit from \002,a2,\002 problem after"
	    " \002,i4,\002 iterations.\002,\002  Inform =\002,i3)";

    /* System generated locals */
    integer a_dim1, a_offset, h_dim1, h_offset, i__1;
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
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
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
    logical found;
    extern /* Subroutine */ int qploc_(logical *, integer *, integer *, 
	    integer *, integer *), dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *);
    integer itmax, nviol;
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *, 
	    integer *, doublereal *, integer *);
    doublereal hsize;
    extern /* Subroutine */ int icopy_(integer *, integer *, integer *, 
	    integer *, integer *);
    char start[4];
    doublereal rteps;
    logical unitq;
    doublereal xnorm;
    integer ld, lr, lq;
    logical halted;
    integer lt;
    extern /* Subroutine */ int cmdgen_(char *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, ftnlen);
    doublereal feamax, feamin;
    integer it, nz;
    extern /* Subroutine */ int mchpar_(void);
    doublereal epsmch;
    integer minact, lfeatu;
#define iprmlc ((integer *)&lcpar1_1 + 30)
    extern /* Subroutine */ int qpdflt_(integer *, integer *, char *, ftnlen);
    doublereal condmx;
    integer minfxd;
#define rprmlc ((doublereal *)&lcpar2_1 + 30)
    integer lkactv, lanorm;
#define msglvl ((integer *)&lcpar1_1 + 41)
    integer nctotl, litotl;
    logical vertex;
    integer lwtinf;
    extern /* Subroutine */ int cminit_(integer *, integer *, char *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, logical *, char *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, ftnlen, ftnlen);
    logical rowerr;
    char prbtyp[2];
    integer nerror;
    extern /* Subroutine */ int qpprnt_();
    integer lwtotl, nmoved, numinf, ianrmj;
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
	    doublereal *, doublereal *);
    integer itnlim;
    extern /* Subroutine */ int cmsetx_(logical *, logical *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, doublereal 
	    *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    doublereal errmax;
    extern /* Subroutine */ int lpcore_(char *, char *, logical *, logical *, 
	    char *, logical *, logical *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, U_fp, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, ftnlen, 
	    ftnlen, ftnlen);
    integer jthcol;
    extern /* Subroutine */ int qpcrsh_(logical *, S_fp, integer *, integer *,
	     integer *, integer *, integer *, integer *, integer *, integer *,
	     integer *, integer *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *, integer *, doublereal *, integer *), 
	    qpcore_(char *, char *, logical *, logical *, char *, logical *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, S_fp, U_fp, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, integer *, doublereal *, 
	    integer *, ftnlen, ftnlen, ftnlen), cmwrap_(logical *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *), cmprnt_(integer *, integer *, 
	    integer *, integer *, doublereal *, logical *, char *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, ftnlen);
    integer lcq, ldr, lgq;
    char msg[6];
    integer ngq;
    logical hot;
    integer lhx, lkx, nzr;

    /* Fortran I/O blocks */
    static cilist io___66 = { 0, 0, 0, fmt_1000, 0 };
    static cilist io___67 = { 0, 0, 0, fmt_1000, 0 };
    static cilist io___73 = { 0, 0, 0, fmt_2001, 0 };
    static cilist io___74 = { 0, 0, 0, fmt_2002, 0 };
    static cilist io___75 = { 0, 0, 0, fmt_2010, 0 };
    static cilist io___76 = { 0, 0, 0, fmt_2015, 0 };
    static cilist io___77 = { 0, 0, 0, fmt_2011, 0 };
    static cilist io___78 = { 0, 0, 0, fmt_2020, 0 };
    static cilist io___79 = { 0, 0, 0, fmt_2030, 0 };
    static cilist io___80 = { 0, 0, 0, fmt_2035, 0 };
    static cilist io___81 = { 0, 0, 0, fmt_2040, 0 };
    static cilist io___82 = { 0, 0, 0, fmt_2050, 0 };
    static cilist io___83 = { 0, 0, 0, fmt_2060, 0 };
    static cilist io___84 = { 0, 0, 0, fmt_2070, 0 };
    static cilist io___85 = { 0, 0, 0, fmt_3000, 0 };
    static cilist io___86 = { 0, 0, 0, fmt_3010, 0 };
    static cilist io___87 = { 0, 0, 0, fmt_3011, 0 };
    static cilist io___88 = { 0, 0, 0, fmt_3015, 0 };
    static cilist io___89 = { 0, 0, 0, fmt_3020, 0 };
    static cilist io___90 = { 0, 0, 0, fmt_2000, 0 };
    static cilist io___91 = { 0, 0, 0, fmt_2000, 0 };


/*     ================================================================== */
/*     QPOPT  solves problems of the form */

/*              minimize               f(x) */
/*                 x */
/*                                    (  x ) */
/*              subject to    bl  .le.(    ).ge.  bu, */
/*                                    ( Ax ) */

/*     where  '  denotes the transpose of a column vector,  x  denotes */
/*     the n-vector of parameters and  F(x) is one of the following... */

/*     FP            =              none    (find a feasible point) */
/*     LP            =    c'x */
/*     QP1           =          1/2 x'Hx     H n x n symmetric */
/*     QP2 (default) =    c'x + 1/2 x'Hx     H n x n symmetric */
/*     QP3           =          1/2 x'G'Gx   G m x n upper trapezoidal */
/*     QP4           =    c'x + 1/2 x'G'Gx   G m x n upper trapezoidal */

/*     Both  H and G  are stored in the two-dimensional array  H  of */
/*     row dimension  ldH.  H  can be entered explicitly as the matrix */
/*     H,  or implicitly via a user-supplied version of the */
/*     subroutine qpHess.  If  ldH = 0,  H is not touched. */

/*     The vector  c  is entered in the one-dimensional array  cvec. */

/*     nclin  is the number of general linear constraints (rows of  A). */
/*     (nclin may be zero.) */

/*     The first  n  components of  bl  and   bu  are lower and upper */
/*     bounds on the variables.  The next  nclin  components are */
/*     lower and upper bounds on the general linear constraints. */

/*     The matrix  A  of coefficients in the general linear constraints */
/*     is entered as the two-dimensional array  A  (of dimension */
/*     ldA by n).  If nclin = 0, a is not referenced. */

/*     The vector  x  must contain an initial estimate of the solution, */
/*     and will contain the computed solution on output. */

/*     For more information, see: */
/*     User's guide  for QPOPT (Version 1.0), by */
/*     P. E. Gill, W. Murray and M. A. Saunders. */

/*     Version 1.0-6   Jun 30, 1991. (Nag Mk 16 version). */
/*     Version 1.0-7   Mar 21, 1993. Summary file added. */
/*     Version 1.0-8   Apr 10, 1994. Sum of infeas. added as an option. */
/*     Version 1.0-9   Jul 15, 1994. Debug output eliminated. */
/*     Version 1.0-10  Sep 15, 1995. New document. */

/*     This version of  QPOPT  dated 04 November 2001. */
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
    h_dim1 = *ldh;
    h_offset = h_dim1 + 1;
    h__ -= h_offset;
    --cvec;
    --ax;
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
    *iter = 0;
    sol4lc_1.header = TRUE_;
    sol4lc_1.prnt = TRUE_;
/*     Set the default values of the parameters. */
    qpdflt_(n, nclin, title, (ftnlen)40);
/* -->  condmx = max( one/epspt5, hundrd ) */
/* -->  condmx = max( one/epspt3, hundrd ) */
/* Computing MAX */
    d__1 = 1. / sol4cm_1.epspt5;
    condmx = max(d__1,100.);
    sol1qp_1.lqptyp = lcpar1_1.lprob;
    sol1qp_1.mhess = lcpar1_1.mm;
    nctotl = *n + *nclin;
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*     Set all parameters determined by the problem type. */
/*        key  lqptyp        objective */
/*        ---  ------        --------- */
/*        FP      1              none    (find a feasible point) */
/*        LP      2    c'x */
/*        QP1     3          1/2 x'Ax     A n x n symmetric */
/*        QP2     4    c'x + 1/2 x'Ax     A n x n symmetric */
/*        QP3     5          1/2 x'A'Ax   A m x n upper trapezoidal */
/*        QP4     6    c'x + 1/2 x'A'Ax   A m x n upper trapezoidal */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
    if (sol1qp_1.lqptyp == 1) {
	s_copy(prbtyp, "FP", (ftnlen)2, (ftnlen)2);
	cset = FALSE_;
    } else if (sol1qp_1.lqptyp == 2) {
	s_copy(prbtyp, "LP", (ftnlen)2, (ftnlen)2);
	cset = TRUE_;
    } else if (sol1qp_1.lqptyp >= 3 && sol1qp_1.lqptyp <= 6) {
	s_copy(prbtyp, "QP", (ftnlen)2, (ftnlen)2);
	cset = TRUE_;
	if (sol1qp_1.lqptyp == 3 || sol1qp_1.lqptyp == 5) {
	    cset = FALSE_;
	}
    } else {
	s_copy(prbtyp, "illegal", (ftnlen)2, (ftnlen)7);
	s_copy(msg, "noprob", (ftnlen)6, (ftnlen)6);
	goto L800;
    }
/*     Assign the dimensions of arrays in the parameter list of qpcore. */
/*     Economies of storage are possible if the minimum number of active */
/*     constraints and the minimum number of fixed variables are known in */
/*     advance.  The expert user should alter minact and minfxd */
/*     accordingly. */
/*     If a linear program is being solved and the matrix of general */
/*     constraints has fewer rows than columns, i.e.,  nclin .lt. n,  a */
/*     non-zero value is known for minfxd.  In this case, vertex must */
/*     be set  .true.. */
    vertex = s_cmp(prbtyp, "QP", (ftnlen)2, (ftnlen)2) != 0 && *nclin < *n;
    minfxd = *n - lcpar1_1.mxfree;
    minact = lcpar1_1.mxfree - lcpar1_1.maxnz;
    sol3cm_1.ldt = max(lcpar1_1.maxnz,lcpar1_1.maxact);
    sol3cm_1.ncolt = lcpar1_1.mxfree;
    ldr = sol3cm_1.ldt;
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
/*                  The first four components of  iw  contain details */
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
    qploc_(&cset, n, nclin, &litotl, &lwtotl);
    lkactv = sol1lc_1.loclc[0];
    lkx = sol1lc_1.loclc[1];
    lfeatu = sol1lc_1.loclc[2];
    lanorm = sol1lc_1.loclc[3];
    lhx = sol1lc_1.loclc[5];
    ld = sol1lc_1.loclc[6];
    lgq = sol1lc_1.loclc[7];
    lcq = sol1lc_1.loclc[8];
    lrlam = sol1lc_1.loclc[9];
    lr = sol1lc_1.loclc[10];
    lt = sol1lc_1.loclc[11];
    lq = sol1lc_1.loclc[12];
    lwtinf = sol1lc_1.loclc[13];
    lwrk = sol1lc_1.loclc[14];
/*     Check input parameters and storage limits. */
    cminit_(&nerror, msglvl, start, leniw, lenw, &litotl, &lwtotl, n, nclin, &
	    ncnln, &istate[1], &named, names, &lcpar2_1.bigbnd, &bl[1], &bu[1]
	    , &clamda[1], &x[1], (ftnlen)4, (ftnlen)16);
    if (nerror > 0) {
	iw[1] = litotl;
	iw[2] = lwtotl;
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
    if (s_cmp(prbtyp, "LP", (ftnlen)2, (ftnlen)2) == 0) {
	itmax = max(lcpar1_1.itmax1,lcpar1_1.itmax2);
	itnlim = itmax;
    } else {
	itmax = lcpar1_1.itmax1;
	itnlim = lcpar1_1.itmax1 + lcpar1_1.itmax2;
    }
    jinf = 0;
/* +    When minimizing the sum of infeasibilities, */
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
	    errmax, &xnorm, &a[a_offset], &ax[1], &bl[1], &bu[1], &clamda[1], 
	    &w[lt], &x[1], &w[lq], &w[ld], &w[lwrk]);
    if (rowerr) {
	s_copy(msg, "rowerr", (ftnlen)6, (ftnlen)6);
	numinf = 1;
	goto L800;
    }
    lpcore_(prbtyp, msg, &cset, &named, names, &rset, &unitq, iter, &itmax, &
	    jinf, &nviol, n, nclin, lda, &nactiv, &nfree, &nzr, &nz, &istate[
	    1], &iw[lkactv], &iw[lkx], (U_fp)qpprnt_, obj, &numinf, &xnorm, &
	    a[a_offset], &ax[1], &bl[1], &bu[1], &cvec[1], &clamda[1], &w[
	    lfeatu], &x[1], &iw[1], &w[1], (ftnlen)2, (ftnlen)6, (ftnlen)16);
    if (s_cmp(prbtyp, "QP", (ftnlen)2, (ftnlen)2) == 0 && s_cmp(msg, "feasbl",
	     (ftnlen)6, (ftnlen)6) == 0) {
	if (*msglvl > 0) {
	    if (sol1cm_1.iprint > 0) {
		io___66.ciunit = sol1cm_1.iprint;
		s_wsfe(&io___66);
		do_fio(&c__1, (char *)&(*iter), (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	    if (sol1cm_1.isumm > 0) {
		io___67.ciunit = sol1cm_1.isumm;
		s_wsfe(&io___67);
		do_fio(&c__1, (char *)&(*iter), (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
/* Computing MIN */
	i__1 = *iter + lcpar1_1.itmax2;
	itmax = min(i__1,itnlim);
/*           ------------------------------------------------------------ */
/*           Compute the first QP objective and transformed gradient. */
/*           ------------------------------------------------------------ */
	if (cset) {
	    *obj = ddot_(n, &cvec[1], &c__1, &x[1], &c__1);
	} else {
	    *obj = 0.;
	}
	jthcol = 0;
	(*qphess)(n, ldh, &jthcol, &h__[h_offset], &x[1], &w[lhx], &iw[1], 
		leniw, &w[1], lenw);
	*obj += ddot_(n, &w[lhx], &c__1, &x[1], &c__1) * .5;
	dcopy_(n, &w[lhx], &c__1, &w[lgq], &c__1);
	cmqmul_(&c__6, n, &nz, &nfree, &sol3cm_1.ldq, &unitq, &iw[lkx], &w[
		lgq], &w[lq], &w[lwrk]);
	if (cset) {
	    daxpy_(n, &c_b44, &w[lcq], &c__1, &w[lgq], &c__1);
	}
/*           ------------------------------------------------------------ */
/*           Find the Cholesky factor R of an initial reduced Hessian. */
/*           The magnitudes of the diagonals of  R  are nonincreasing. */
/*           ------------------------------------------------------------ */
	if (cset) {
	    ngq = 2;
	} else {
	    ngq = 1;
	}
	hsize = 1.;
	qpcrsh_(&unitq, (S_fp)qphess, &lcpar1_1.maxnz, n, &ngq, &nzr, &nz, &
		nfree, &sol3cm_1.ldq, ldh, &ldr, &iw[lkx], &hsize, &
		lcpar2_1.tolrnk, &w[lgq], &h__[h_offset], &w[lr], &w[lq], &w[
		lwrk], &w[lrlam], &iw[1], leniw, &w[1], lenw);
	qpcore_(prbtyp, msg, &cset, &named, names, &unitq, iter, &itmax, &
		nviol, n, nclin, lda, ldh, &nactiv, &nfree, &nzr, &nz, &
		istate[1], &iw[lkactv], &iw[lkx], (S_fp)qphess, (U_fp)qpprnt_,
		 obj, &xnorm, &hsize, &a[a_offset], &ax[1], &bl[1], &bu[1], &
		cvec[1], &clamda[1], &w[lfeatu], &h__[h_offset], &x[1], &iw[1]
		, leniw, &w[1], lenw, (ftnlen)2, (ftnlen)6, (ftnlen)16);
    }
    found = s_cmp(msg, "optiml", (ftnlen)6, (ftnlen)6) == 0 || s_cmp(msg, 
	    "feasbl", (ftnlen)6, (ftnlen)6) == 0 || s_cmp(msg, "deadpt", (
	    ftnlen)6, (ftnlen)6) == 0 || s_cmp(msg, "weak  ", (ftnlen)6, (
	    ftnlen)6) == 0 || s_cmp(msg, "unbndd", (ftnlen)6, (ftnlen)6) == 0 
	    || s_cmp(msg, "infeas", (ftnlen)6, (ftnlen)6) == 0;
    halted = s_cmp(msg, "itnlim", (ftnlen)6, (ftnlen)6) == 0 || s_cmp(msg, 
	    "Rz2big", (ftnlen)6, (ftnlen)6) == 0;
    if (found) {
	cmdgen_("Optimal", msglvl, n, nclin, &nmoved, iter, &numinf, &istate[
		1], &bl[1], &bu[1], &clamda[1], &w[lfeatu], &x[1], (ftnlen)7);
    }
    done = found && nviol == 0 && nmoved == 0;
/*     until      done  .or.  halted */
    if (! (done || halted)) {
	goto L300;
    }
/*     ================================================================== */
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
    if (s_cmp(msg, "feasbl", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 0;
	if (sol4lc_1.prnt) {
	    io___73.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___73);
	    e_wsfe();
	}
    } else if (s_cmp(msg, "optiml", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 0;
	if (sol4lc_1.prnt) {
	    io___74.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___74);
	    do_fio(&c__1, prbtyp, (ftnlen)2);
	    e_wsfe();
	}
    } else if (s_cmp(msg, "deadpt", (ftnlen)6, (ftnlen)6) == 0 || s_cmp(msg, 
	    "weak  ", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 1;
	if (sol4lc_1.prnt) {
	    if (s_cmp(prbtyp, "QP", (ftnlen)2, (ftnlen)2) == 0) {
		io___75.ciunit = sol1cm_1.iprint;
		s_wsfe(&io___75);
		e_wsfe();
		if (nz > nzr) {
		    io___76.ciunit = sol1cm_1.iprint;
		    s_wsfe(&io___76);
		    i__1 = nz - nzr;
		    do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    } else {
		io___77.ciunit = sol1cm_1.iprint;
		s_wsfe(&io___77);
		e_wsfe();
	    }
	}
    } else if (s_cmp(msg, "unbndd", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 2;
	if (sol4lc_1.prnt) {
	    io___78.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___78);
	    do_fio(&c__1, prbtyp, (ftnlen)2);
	    e_wsfe();
	}
    } else if (s_cmp(msg, "infeas", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 3;
	if (sol4lc_1.prnt) {
	    io___79.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___79);
	    e_wsfe();
	}
    } else if (s_cmp(msg, "rowerr", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 3;
	if (sol4lc_1.prnt) {
	    io___80.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___80);
	    e_wsfe();
	}
    } else if (s_cmp(msg, "itnlim", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 4;
	if (sol4lc_1.prnt) {
	    io___81.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___81);
	    e_wsfe();
	}
    } else if (s_cmp(msg, "Rz2big", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 5;
	if (sol4lc_1.prnt) {
	    io___82.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___82);
	    do_fio(&c__1, (char *)&lcpar1_1.maxnz, (ftnlen)sizeof(integer));
	    e_wsfe();
	}
    } else if (s_cmp(msg, "errors", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 6;
	if (sol4lc_1.prnt) {
	    io___83.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___83);
	    do_fio(&c__1, (char *)&nerror, (ftnlen)sizeof(integer));
	    e_wsfe();
	}
    } else if (s_cmp(msg, "noprob", (ftnlen)6, (ftnlen)6) == 0) {
	*inform__ = 7;
	if (sol4lc_1.prnt) {
	    io___84.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___84);
	    e_wsfe();
	}
    }
    if (sol4lc_1.prnt) {
	if (*inform__ < 5) {
	    if (numinf == 0) {
		if (s_cmp(prbtyp, "FP", (ftnlen)2, (ftnlen)2) != 0) {
		    io___85.ciunit = sol1cm_1.iprint;
		    s_wsfe(&io___85);
		    do_fio(&c__1, prbtyp, (ftnlen)2);
		    do_fio(&c__1, (char *)&(*obj), (ftnlen)sizeof(doublereal))
			    ;
		    e_wsfe();
		}
	    } else if (*inform__ == 3) {
		if (s_cmp(msg, "infeas", (ftnlen)6, (ftnlen)6) == 0) {
		    if (lcpar1_1.minsum == 0) {
			io___86.ciunit = sol1cm_1.iprint;
			s_wsfe(&io___86);
			do_fio(&c__1, (char *)&(*obj), (ftnlen)sizeof(
				doublereal));
			e_wsfe();
		    } else {
			io___87.ciunit = sol1cm_1.iprint;
			s_wsfe(&io___87);
			do_fio(&c__1, (char *)&(*obj), (ftnlen)sizeof(
				doublereal));
			e_wsfe();
		    }
		} else if (s_cmp(msg, "rowerr", (ftnlen)6, (ftnlen)6) == 0) {
		    io___88.ciunit = sol1cm_1.iprint;
		    s_wsfe(&io___88);
		    do_fio(&c__1, (char *)&errmax, (ftnlen)sizeof(doublereal))
			    ;
		    e_wsfe();
		}
	    } else {
		io___89.ciunit = sol1cm_1.iprint;
		s_wsfe(&io___89);
		do_fio(&c__1, (char *)&(*obj), (ftnlen)sizeof(doublereal));
		e_wsfe();
	    }
	}
    }
    if (*inform__ < 6) {
	if (*msglvl > 0) {
	    if (sol1cm_1.iprint > 0) {
		io___90.ciunit = sol1cm_1.iprint;
		s_wsfe(&io___90);
		do_fio(&c__1, prbtyp, (ftnlen)2);
		do_fio(&c__1, (char *)&(*iter), (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&(*inform__), (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	    if (sol1cm_1.isumm > 0) {
		io___91.ciunit = sol1cm_1.isumm;
		s_wsfe(&io___91);
		do_fio(&c__1, prbtyp, (ftnlen)2);
		do_fio(&c__1, (char *)&(*iter), (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&(*inform__), (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
    }
    icopy_(&c__30, lcpar1_1.ipsvlc, &c__1, iprmlc, &c__1);
    dcopy_(&c__30, lcpar2_1.rpsvlc, &c__1, rprmlc, &c__1);
    return 0;
/*     end of qpopt */
} /* qpopt_ */

#undef msglvl
#undef rprmlc
#undef iprmlc


/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int qpcolr_(logical *singlr, logical *posdef, logical *
	renewr, logical *unitq, integer *n, integer *nzr, integer *nfree, 
	integer *ldq, integer *ldh, integer *ldr, integer *kx, doublereal *
	hsize, doublereal *dzz, doublereal *tolrnk, S_fp qphess, doublereal *
	h__, doublereal *r__, doublereal *q, doublereal *hz, doublereal *wrk, 
	integer *iw, integer *leniw, doublereal *w, integer *lenw)
{
    /* System generated locals */
    integer h_dim1, h_offset, r_dim1, r_offset, q_dim1, q_offset, i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal zthz;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    integer j, k;
    extern /* Subroutine */ int dload_(integer *, doublereal *, doublereal *, 
	    integer *), dcond_(integer *, doublereal *, integer *, doublereal 
	    *, doublereal *), dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *), dtrsv_(char *, char *, char *, integer *
	    , doublereal *, integer *, doublereal *, integer *, ftnlen, 
	    ftnlen, ftnlen);
    integer jthcol;
    doublereal rdsmin;
    extern /* Subroutine */ int cmqmul_(integer *, integer *, integer *, 
	    integer *, integer *, logical *, integer *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal drzmin, drzmax, dzznew, rznorm, rzz;
    integer nzr1;

/*     ================================================================== */
/*     QPCOLR  is used to compute the last column of the (nZr x nZr) */
/*     triangular factor Rz such that */
/*                    Hz  =  (Rz)'D(Rz), */
/*     where Hz is the reduced Hessian Z'HZ, and D is a diagonal */
/*     matrix.  If  Hz  is positive definite, Rz is the Cholesky factor */
/*     of  Hz  and  D  is the identity matrix;  otherwise, D(nZr) is */
/*     negative or small and the last diagonal of Rz is one. */

/*     The element D(nZr) is stored in Dzz.  Dzz is equal to one if */
/*     posdef is true. */

/*     Original f66 version written by PEG,   March-1982. */
/*     This version of  qpcolr  dated 16-Jan-1995. */
/*     (C) 1992--1997  Regents of the University of California and the */
/*                     Trustees of Stanford University */
/*     ================================================================== */
    /* Parameter adjustments */
    --wrk;
    --hz;
    --kx;
    q_dim1 = *ldq;
    q_offset = q_dim1 + 1;
    q -= q_offset;
    h_dim1 = *ldh;
    h_offset = h_dim1 + 1;
    h__ -= h_offset;
    r_dim1 = *ldr;
    r_offset = r_dim1 + 1;
    r__ -= r_offset;
    --iw;
    --w;

    /* Function Body */
    if (*nzr == 0) {
	*posdef = TRUE_;
	*renewr = FALSE_;
	*singlr = FALSE_;
	*dzz = 1.;
    } else {
	if (*renewr) {
/*           ------------------------------------------------------------ */
/*           Compute the first nZr-1 elements of the last column of Rz */
/*           and Dzznew, the square of the last diagonal element. */
/*           ------------------------------------------------------------ */
	    dload_(n, &c_b132, &wrk[1], &c__1);
	    if (*unitq) {
/*              Only bounds are in the working set.  The nZr-th column of */
/*              Z is just a column of the identity matrix. */
		jthcol = kx[*nzr];
		wrk[jthcol] = 1.;
	    } else {
/*              Expand the new column of  Z  into an n-vector. */
		i__1 = *nfree;
		for (k = 1; k <= i__1; ++k) {
		    j = kx[k];
		    wrk[j] = q[k + *nzr * q_dim1];
/* L110: */
		}
		jthcol = 0;
	    }
/*           Compute the nZr-th column of Z'HZ. */
	    (*qphess)(n, ldh, &jthcol, &h__[h_offset], &wrk[1], &hz[1], &iw[1]
		    , leniw, &w[1], lenw);
	    cmqmul_(&c__4, n, nzr, nfree, ldq, unitq, &kx[1], &hz[1], &q[
		    q_offset], &wrk[1]);
	    dcopy_(nzr, &hz[1], &c__1, &r__[*nzr * r_dim1 + 1], &c__1);
	    nzr1 = *nzr - 1;
	    zthz = r__[*nzr + *nzr * r_dim1];
	    dzznew = zthz;
	    if (nzr1 > 0) {
		dtrsv_("U", "T", "N", &nzr1, &r__[r_offset], ldr, &r__[*nzr * 
			r_dim1 + 1], &c__1, (ftnlen)1, (ftnlen)1, (ftnlen)1);
		rznorm = dnrm2_(&nzr1, &r__[*nzr * r_dim1 + 1], &c__1);
		dzznew = zthz - rznorm * rznorm;
	    }
	    r__[*nzr + *nzr * r_dim1] = 1.;
	    *dzz = dzznew;
/*           Update the estimate of the norm of the Hessian. */
/* Computing MAX */
	    d__1 = *hsize, d__2 = abs(zthz);
	    *hsize = max(d__1,d__2);
	}
/* Computing 2nd power */
	d__1 = r__[*nzr + *nzr * r_dim1];
	dzznew = *dzz * (d__1 * d__1);
/*        --------------------------------------------------------------- */
/*        Attempt to compute Rzz, the square root of  Dzznew.  The last */
/*        diagonal of Rz.  The variables posdef and singlr are set here. */
/*        They are used to indicate if the new Z'HZ is positive definite */
/*        or singular.  If the required diagonal modification is large */
/*        the last row and column of Rz are marked for recomputation next */
/*        iteration. */
/*        --------------------------------------------------------------- */
/*        Rdsmin is the square of the smallest allowable diagonal element */
/*        for a positive-definite Cholesky factor.  Note that the test */
/*        for positive definiteness is unavoidably scale dependent. */
	if (*nzr == 1) {
	    rdsmin = *tolrnk * *hsize;
	} else {
	    i__1 = *ldr + 1;
	    dcond_(nzr, &r__[r_offset], &i__1, &drzmax, &drzmin);
	    rdsmin = *tolrnk * drzmax * drzmax;
	}
	*posdef = dzznew > rdsmin;
	if (*posdef) {
	    *dzz = 1.;
	    rzz = sqrt(dzznew);
	    *renewr = FALSE_;
	    *singlr = FALSE_;
	} else {
	    *dzz = dzznew;
	    rzz = 1.;
	    *singlr = dzznew >= -rdsmin;
	    *renewr = dzznew < *hsize * -10.;
	}
	r__[*nzr + *nzr * r_dim1] = rzz;
    }
/*     end of qpcolr */
    return 0;
} /* qpcolr_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int qpcore_(char *prbtyp, char *msg, logical *cset, logical *
	named, char *names, logical *unitq, integer *iter, integer *itmax, 
	integer *nviol, integer *n, integer *nclin, integer *lda, integer *
	ldh, integer *nactiv, integer *nfree, integer *nzr, integer *nz, 
	integer *istate, integer *kactiv, integer *kx, S_fp qphess, S_fp 
	qpprnt, doublereal *objqp, doublereal *xnorm, doublereal *hsize, 
	doublereal *a, doublereal *ax, doublereal *bl, doublereal *bu, 
	doublereal *cvec, doublereal *featol, doublereal *featlu, doublereal *
	h__, doublereal *x, integer *iw, integer *leniw, doublereal *w, 
	integer *lenw, ftnlen prbtyp_len, ftnlen msg_len, ftnlen names_len)
{
    /* Format strings */
    static char fmt_2100[] = "(\002 XXX  Iterative refinement.  The maximum "
	    "violation is \002,1p,e14.2,\002 in constraint\002,i5)";

    /* System generated locals */
    integer a_dim1, a_offset, h_dim1, h_offset, i__1;
    doublereal d__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_wsfe(cilist *), do_fio(
	    integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    integer iadd, kdel, jinf;
    extern doublereal ddiv_(doublereal *, doublereal *, logical *), ddot_(
	    integer *, doublereal *, integer *, doublereal *, integer *);
    integer ifix, jmax;
    logical move, rset;
    doublereal gzdz;
    integer lwrk;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    doublereal alfap;
    extern /* Subroutine */ int dcond_(integer *, doublereal *, integer *, 
	    doublereal *, doublereal *);
    logical onbnd;
    integer irefn;
    doublereal flmax;
    integer lrlam;
    doublereal condt;
    logical uncon;
    doublereal dinky, tollm;
    extern /* Subroutine */ int rzdel_(logical *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    doublereal dnorm;
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *, 
	    integer *, doublereal *, integer *);
    integer jtiny;
    extern /* Subroutine */ int rzadd_(logical *, logical *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), dcopy_(
	    integer *, doublereal *, integer *, doublereal *, integer *), 
	    cmmul1_(char *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    doublereal *, integer *, integer *, doublereal *, doublereal *, 
	    integer *, integer *, doublereal *, integer *, integer *, 
	    doublereal *, doublereal *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, ftnlen), cmmul2_(integer *, integer *, integer *, 
	    integer *, doublereal *, integer *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *);
    integer ld;
    doublereal bigalf;
    integer it;
    logical deadpt, delreg;
    integer lr, lt, lq;
    logical posdef;
#define iprmlc ((integer *)&lcpar1_1 + 30)
    logical hitcon, overfl;
#define rprmlc ((doublereal *)&lcpar2_1 + 30)
    logical giveup;
#define msglvl ((integer *)&lcpar1_1 + 41)
    logical hitlow, minmzr, firstv, renewr, singlr, statpt, unbndd, unitgz;
    integer lanorm, lwtinf, nctotl, numinf;
    doublereal suminf, condmx;
    integer notopt;
    doublereal gznorm, gzrnrm, gfnorm, objsiz, wssize, drzmax, drzmin, condrz,
	     zerolm, smllst, biggst, tinyst, trusml;
    integer jsmlst, ksmlst;
    doublereal trubig;
    integer jbigst, kbigst, jdsave, issave, lad;
    extern /* Subroutine */ int qpcolr_(logical *, logical *, logical *, 
	    logical *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, doublereal *, doublereal *, doublereal *, 
	    S_fp, doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *, integer *, doublereal *, integer *), 
	    qpgetd_(logical *, logical *, logical *, logical *, logical *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), cmchzr_(
	    logical *, integer *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, logical *, logical *, logical *, 
	    logical *, doublereal *, doublereal *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    doublereal alfhit, objchg;
    integer inform__;
    extern /* Subroutine */ int cmfeas_(integer *, integer *, integer *, 
	    doublereal *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    doublereal errmax;
    extern /* Subroutine */ int cmdgen_(char *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, ftnlen);
    integer nmoved, jthcol, lcq, ldr, lgq, ngq;
    extern /* Subroutine */ int cmqmul_(integer *, integer *, integer *, 
	    integer *, integer *, logical *, integer *, doublereal *, 
	    doublereal *, doublereal *);
    integer lhx;
    doublereal dzz;

    /* Fortran I/O blocks */
    static cilist io___184 = { 0, 0, 0, fmt_2100, 0 };
    static cilist io___185 = { 0, 0, 0, fmt_2100, 0 };


/*     ================================================================== */
/*     qpcore  is a subroutine for general quadratic programming. */
/*     On entry, it is assumed that an initial working set of */
/*     linear constraints and bounds is available. */
/*     The arrays  istate, kactiv and kx  will have been set accordingly */
/*     and the arrays  T  and  Q  will contain the TQ factorization of */
/*     the matrix whose rows are the gradients of the active linear */
/*     constraints with the columns corresponding to the active bounds */
/*     removed.  The TQ factorization of the resulting (nactiv by nfree) */
/*     matrix is  A(free)*Q = (0 T),  where Q is (nfree by nfree) and T */
/*     is upper-triangular. */

/*     Over a cycle of iterations, the feasibility tolerance featol */
/*     increases slightly (from tolx0 to tolx1 in steps of tolinc). */
/*     this ensures that all steps taken will be positive. */

/*     After idegen consecutive iterations, variables within featol of */
/*     their bounds are set exactly on their bounds and iterative */
/*     refinement is used to satisfy the constraints in the working set. */
/*     featol is then reduced to tolx0 for the next cycle of iterations. */

/*     Values of istate(j) for the linear constraints....... */

/*     Istate(j) */
/*     --------- */
/*          0    constraint j is not in the working set. */
/*          1    constraint j is in the working set at its lower bound. */
/*          2    constraint j is in the working set at its upper bound. */
/*          3    constraint j is in the working set as an equality. */

/*     Constraint j may be violated by as much as featol(j). */

/*     This version of  qpcore  dated  16-Jan-95. */
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
    h_dim1 = *ldh;
    h_offset = h_dim1 + 1;
    h__ -= h_offset;
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
    ldr = sol3cm_1.ldt;
    it = 1;
    lanorm = sol1lc_1.loclc[3];
    lad = sol1lc_1.loclc[4];
    lhx = sol1lc_1.loclc[5];
    ld = sol1lc_1.loclc[6];
    lgq = sol1lc_1.loclc[7];
    lcq = sol1lc_1.loclc[8];
    lrlam = sol1lc_1.loclc[9];
    lr = sol1lc_1.loclc[10];
    lt = sol1lc_1.loclc[11];
    lq = sol1lc_1.loclc[12];
    lwtinf = sol1lc_1.loclc[13];
    lwrk = sol1lc_1.loclc[14];
/*     Initialize. */
    irefn = 0;
    jinf = 0;
    nctotl = *n + *nclin;
    *nviol = 0;
    numinf = 0;
    suminf = 0.;
    condmx = flmax;
    delreg = FALSE_;
    firstv = FALSE_;
    posdef = TRUE_;
    renewr = FALSE_;
    rset = TRUE_;
    singlr = FALSE_;
    uncon = FALSE_;
    unitgz = FALSE_;
    notopt = 0;
    dzz = 1.;
    s_copy(msg, "      ", (ftnlen)6, (ftnlen)6);
/* *    ======================Start of main loop========================== */
/* +    do while (msg .eq. empty) */
L100:
    if (s_cmp(msg, "      ", (ftnlen)6, (ftnlen)6) == 0) {
	if (*nz > 0) {
	    gznorm = dnrm2_(nz, &w[lgq], &c__1);
	} else {
	    gznorm = 0.;
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
	objsiz = abs(*objqp) + 1.;
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
	if (uncon) {
	    unitgz = gzrnrm <= dinky;
	}
/*        If the reduced gradient (Zr)'g is small and Hz is positive */
/*        definite,  x is a minimizer on the working set. */
/*        A maximum number of unconstrained steps is imposed to */
/*        allow for  dinky  being too large because of bad scaling. */
	statpt = gzrnrm <= dinky;
	giveup = irefn > 1;
	minmzr = statpt && posdef;
	deadpt = statpt && singlr;
/*        --------------------------------------------------------------- */
/*        Print the details of this iteration. */
/*        --------------------------------------------------------------- */
/*        Define small quantities that reflect the size of x, R and */
/*        the constraints in the working set. */
	if (sol4lc_1.prnt) {
	    if (*nzr > 0) {
		i__1 = ldr + 1;
		dcond_(nzr, &w[lr], &i__1, &drzmax, &drzmin);
		condrz = ddiv_(&drzmax, &drzmin, &overfl);
	    } else {
		condrz = 1.;
	    }
	    if (*nactiv > 0) {
		condt = ddiv_(&sol5cm_1.dtmax, &sol5cm_1.dtmin, &overfl);
	    } else {
		condt = 1.;
	    }
	    (*qpprnt)(prbtyp, &sol4lc_1.header, &rset, msglvl, iter, &
		    sol4lc_1.isdel, &sol4lc_1.jdel, &sol4lc_1.jadd, n, nclin, 
		    nactiv, nfree, nz, nzr, &ldr, &sol3cm_1.ldt, &istate[1], &
		    sol4lc_1.alfa, &condrz, &condt, &dzz, &gzrnrm, &numinf, &
		    suminf, &notopt, objqp, &sol4lc_1.trulam, &ax[1], &w[lr], 
		    &w[lt], &x[1], &w[lwrk], (ftnlen)2);
	}
	if (minmzr || giveup) {
/*           ============================================================ */
/*           The point  x  is a constrained stationary point. */
/*           Compute Lagrange multipliers. */
/*           ============================================================ */
/*           Define what we mean by ``non-optimal'' multipliers. */
	    notopt = 0;
	    sol4lc_1.jdel = 0;
	    zerolm = tollm;
	    smllst = tollm;
	    biggst = tollm + 1.;
	    tinyst = tollm;
	    cmmul1_(prbtyp, msglvl, n, lda, &sol3cm_1.ldt, nactiv, nfree, nz, 
		    &istate[1], &kactiv[1], &kx[1], &zerolm, &notopt, &numinf,
		     &trusml, &smllst, &jsmlst, &ksmlst, &tinyst, &jtiny, &
		    jinf, &trubig, &biggst, &jbigst, &kbigst, &a[a_offset], &
		    w[lanorm], &w[lgq], &w[lrlam], &w[lt], &w[lwtinf], (
		    ftnlen)2);
	    if (*nzr < *nz) {
		cmmul2_(msglvl, n, nzr, nz, &zerolm, &notopt, &numinf, &
			trusml, &smllst, &jsmlst, &tinyst, &jtiny, &w[lgq]);
	    }
	    if (notopt == 0 && posdef) {
		s_copy(msg, "optiml", (ftnlen)6, (ftnlen)6);
		goto L100;
	    }
/*           ------------------------------------------------------------ */
/*           Delete one of three types of constraint */
/*           (1) regular           jsmlst > 0   istate(jsmlst) = 1, 2 */
/*           (2) temporary bound   jsmlst > 0,  istate(jsmlst) = 4 */
/*           (3) artificial        jsmlst < 0 */
/*           ------------------------------------------------------------ */
	    sol4lc_1.trulam = trusml;
	    sol4lc_1.jdel = jsmlst;
	    delreg = FALSE_;
	    if (*nzr + 1 > lcpar1_1.maxnz && sol4lc_1.jdel != 0) {
		s_copy(msg, "Rz2big", (ftnlen)6, (ftnlen)6);
		goto L100;
	    }
	    if (sol4lc_1.jdel > 0) {
/*              Regular constraint or temporary bound. */
/*              delreg  says that a regular constraint was deleted. */
/*              jdsave, issave are only defined if  delreg  is true. */
		kdel = ksmlst;
		sol4lc_1.isdel = istate[sol4lc_1.jdel];
		istate[sol4lc_1.jdel] = 0;
		delreg = sol4lc_1.isdel != 4;
		if (delreg) {
		    jdsave = sol4lc_1.jdel;
		    issave = sol4lc_1.isdel;
		}
	    }
/*           Update the factorizations. */
	    rzdel_(unitq, &it, n, nactiv, nfree, &ngq, nz, nzr, lda, &
		    sol3cm_1.ldq, &sol3cm_1.ldt, &sol4lc_1.jdel, &kdel, &
		    kactiv[1], &kx[1], &a[a_offset], &w[lt], &w[lgq], &w[lq], 
		    &w[lwrk], &w[ld], &w[lrlam]);
	    renewr = TRUE_;
	    qpcolr_(&singlr, &posdef, &renewr, unitq, n, nzr, nfree, &
		    sol3cm_1.ldq, ldh, &ldr, &kx[1], hsize, &dzz, &
		    lcpar2_1.tolrnk, (S_fp)qphess, &h__[h_offset], &w[lr], &w[
		    lq], &w[lwrk], &w[ld], &iw[1], leniw, &w[1], lenw);
	    irefn = 0;
	    sol4lc_1.prnt = FALSE_;
	    uncon = FALSE_;
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
	    qpgetd_(&delreg, &posdef, &statpt, &unitgz, unitq, n, nclin, 
		    nfree, lda, &sol3cm_1.ldq, &ldr, nzr, &issave, &jdsave, &
		    kx[1], &dnorm, &gzdz, &a[a_offset], &w[lad], &w[ld], &w[
		    lgq], &w[lr], &w[lq], &w[lwrk]);
/*           ------------------------------------------------------------ */
/*           Find the constraint we bump into along  d. */
/*           Update  x  and  Ax  if the step  alfa  is nonZero. */
/*           ------------------------------------------------------------ */
/*           cmchzr initializes  alfhit  to bigalf. If it is still */
/*           that value on exit,  it is regarded as infinite. */
	    bigalf = ddiv_(&lcpar2_1.bigdx, &dnorm, &overfl);
	    cmchzr_(&firstv, n, nclin, &istate[1], &bigalf, &lcpar2_1.bigbnd, 
		    &dnorm, &hitlow, &move, &onbnd, &unbndd, &alfhit, &alfap, 
		    &sol4lc_1.jadd, &w[lanorm], &w[lad], &ax[1], &bl[1], &bu[
		    1], &featol[1], &featlu[1], &w[ld], &x[1]);
/*           ------------------------------------------------------------ */
/*           If Hz is positive definite,  alfa = 1.0  will be the step */
/*           to the minimizer of the quadratic on the current working */
/*           set.  If the unit step does not violate the nearest */
/*           constraint by more than featol,  the constraint is not */
/*           added to the working set. */
/*           ------------------------------------------------------------ */
	    uncon = alfap > 1. && posdef;
	    hitcon = ! uncon;
	    if (hitcon) {
		sol4lc_1.alfa = alfhit;
		irefn = 0;
	    } else {
		++irefn;
		sol4lc_1.jadd = 0;
		sol4lc_1.alfa = 1.;
	    }
	    if (hitcon && unbndd) {
		s_copy(msg, "unbndd", (ftnlen)6, (ftnlen)6);
		goto L100;
	    }
/*           Predict the change in the QP objective function. */
	    if (posdef) {
		objchg = sol4lc_1.alfa * gzdz * (1. - sol4lc_1.alfa * .5);
	    } else {
/* Computing 2nd power */
		d__1 = sol4lc_1.alfa;
		objchg = sol4lc_1.alfa * gzdz + d__1 * d__1 * .5 * dzz;
	    }
/*           Check for a dead point or unbounded solution. */
	    if (objchg >= -sol4cm_1.epspt9 * objsiz && deadpt) {
		s_copy(msg, "deadpt", (ftnlen)6, (ftnlen)6);
		goto L100;
	    }
	    if (objchg >= sol4cm_1.epspt9 * objsiz) {
		s_copy(msg, "resetx", (ftnlen)6, (ftnlen)6);
		goto L100;
	    }
	    daxpy_(n, &sol4lc_1.alfa, &w[ld], &c__1, &x[1], &c__1);
	    if (*nclin > 0) {
		daxpy_(nclin, &sol4lc_1.alfa, &w[lad], &c__1, &ax[1], &c__1);
	    }
	    *xnorm = dnrm2_(n, &x[1], &c__1);
	    if (hitcon) {
/*              --------------------------------------------------------- */
/*              Add a constraint to the working set. */
/*              Update the TQ factors of the working set. */
/*              Use  d  as temporary work space. */
/*              --------------------------------------------------------- */
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
		    if (hitlow) {
			x[sol4lc_1.jadd] = bl[sol4lc_1.jadd];
		    } else {
			x[sol4lc_1.jadd] = bu[sol4lc_1.jadd];
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
		rzadd_(unitq, &rset, &inform__, &ifix, &iadd, &sol4lc_1.jadd, 
			&it, nactiv, nz, nfree, nzr, &ngq, n, lda, &
			sol3cm_1.ldq, &ldr, &sol3cm_1.ldt, &kx[1], &condmx, &
			dzz, &a[a_offset], &w[lr], &w[lt], &w[lgq], &w[lq], &
			w[lwrk], &w[lrlam], &w[ld]);
		--(*nzr);
		--(*nz);
		if (sol4lc_1.jadd <= *n) {
/*                 A simple bound has been added. */
		    --(*nfree);
		} else {
/*                 A general constraint has been added. */
		    ++(*nactiv);
		    kactiv[*nactiv] = iadd;
		}
/*              --------------------------------------------------------- */
/*              Check if  Hz  has become positive definite. */
/*              Recompute the last column of Rz if unacceptable */
/*              growth has occurred. */
/*              -------------------------------------------------------- */
		if (! posdef) {
		    qpcolr_(&singlr, &posdef, &renewr, unitq, n, nzr, nfree, &
			    sol3cm_1.ldq, ldh, &ldr, &kx[1], hsize, &dzz, &
			    lcpar2_1.tolrnk, (S_fp)qphess, &h__[h_offset], &w[
			    lr], &w[lq], &w[lwrk], &w[ld], &iw[1], leniw, &w[
			    1], lenw);
		}
	    }
/*           Increment featol. */
	    daxpy_(&nctotl, &sol3lc_1.tolinc, &featlu[1], &c__1, &featol[1], &
		    c__1);
	    if (*iter % lcpar1_1.kchk == 0) {
/*              --------------------------------------------------------- */
/*              Check the feasibility of constraints with non-negative */
/*              istate  values.  If violations have occurred,  force */
/*              iterative refinement and a switch to phase 1. */
/*              --------------------------------------------------------- */
		cmfeas_(n, nclin, &istate[1], &lcpar2_1.bigbnd, nviol, &jmax, 
			&errmax, &ax[1], &bl[1], &bu[1], &featol[1], &x[1]);
		if (*nviol > 0) {
		    if (*msglvl > 0) {
			if (sol1cm_1.iprint > 0) {
			    io___184.ciunit = sol1cm_1.iprint;
			    s_wsfe(&io___184);
			    do_fio(&c__1, (char *)&errmax, (ftnlen)sizeof(
				    doublereal));
			    do_fio(&c__1, (char *)&jmax, (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			}
			if (sol1cm_1.isumm > 0) {
			    io___185.ciunit = sol1cm_1.isumm;
			    s_wsfe(&io___185);
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
		cmdgen_("End of cycle", msglvl, n, nclin, &nmoved, iter, &
			numinf, &istate[1], &bl[1], &bu[1], &featol[1], &
			featlu[1], &x[1], (ftnlen)12);
		*nviol += nmoved;
	    }
	    if (*nviol > 0) {
		s_copy(msg, "resetx", (ftnlen)6, (ftnlen)6);
		goto L100;
	    }
/*           ------------------------------------------------------------ */
/*           Compute the QP objective and transformed gradient. */
/*           ------------------------------------------------------------ */
	    if (*cset) {
		*objqp = ddot_(n, &cvec[1], &c__1, &x[1], &c__1);
	    } else {
		*objqp = 0.;
	    }
	    jthcol = 0;
	    (*qphess)(n, ldh, &jthcol, &h__[h_offset], &x[1], &w[lhx], &iw[1],
		     leniw, &w[1], lenw);
	    *objqp += ddot_(n, &w[lhx], &c__1, &x[1], &c__1) * .5;
	    dcopy_(n, &w[lhx], &c__1, &w[lgq], &c__1);
	    cmqmul_(&c__6, n, nz, nfree, &sol3cm_1.ldq, unitq, &kx[1], &w[lgq]
		    , &w[lq], &w[lwrk]);
	    if (*cset) {
		daxpy_(n, &c_b44, &w[lcq], &c__1, &w[lgq], &c__1);
	    }
	}
	goto L100;
/* +    end while */
    }
/*     ======================end of main loop============================ */

    return 0;
/*     end of qpcore */
} /* qpcore_ */

#undef msglvl
#undef rprmlc
#undef iprmlc


/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int qpcrsh_(logical *unitq, S_fp qphess, integer *maxnz, 
	integer *n, integer *ngq, integer *nzr, integer *nz, integer *nfree, 
	integer *ldq, integer *ldh, integer *ldr, integer *kx, doublereal *
	hsize, doublereal *tolrnk, doublereal *gq, doublereal *h__, 
	doublereal *r__, doublereal *q, doublereal *hz, doublereal *wrk, 
	integer *iw, integer *leniw, doublereal *w, integer *lenw)
{
    /* System generated locals */
    integer gq_dim1, gq_offset, h_dim1, h_offset, r_dim1, r_offset, q_dim1, 
	    q_offset, i__1, i__2, i__3;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal dmin__, dmax__;
    integer kmax;
    extern /* Subroutine */ int dsyr_(char *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, ftnlen);
    doublereal d__;
    integer i__, j, k;
    extern /* Subroutine */ int dload_(integer *, doublereal *, doublereal *, 
	    integer *), dscal_(integer *, doublereal *, doublereal *, integer 
	    *), dcopy_(integer *, doublereal *, integer *, doublereal *, 
	    integer *), dswap_(integer *, doublereal *, integer *, doublereal 
	    *, integer *);
    extern integer idamax_(integer *, doublereal *, integer *);
    integer jthcol;
    extern /* Subroutine */ int cmqmul_(integer *, integer *, integer *, 
	    integer *, integer *, logical *, integer *, doublereal *, 
	    doublereal *, doublereal *);
    integer mnz;

/*     ================================================================== */
/*     qpcrsh  computes the Cholesky factor Rz of the reduced Hessian */
/*     Z'HZ,  given the (nfree x nZ) matrix Z.  If the reduced Hessian is */
/*     indefinite, the Cholesky factor of the (nZr x nZr) matrix  Hz  is */
/*     returned, where Hz is formed from  H  and  nZr  columns of Z. */
/*     Column interchanges are used in an attempt to maximize nZr. */
/*     These are applied to  Z  and the rows of the matrix  gq. */

/*     This version of qpcrsh dated 16-Jan-1995. */
/*     (C) 1992--1997  Regents of the University of California and the */
/*                     Trustees of Stanford University */
/*     ================================================================== */
    /* Parameter adjustments */
    --wrk;
    --hz;
    gq_dim1 = *n;
    gq_offset = gq_dim1 + 1;
    gq -= gq_offset;
    --kx;
    q_dim1 = *ldq;
    q_offset = q_dim1 + 1;
    q -= q_offset;
    h_dim1 = *ldh;
    h_offset = h_dim1 + 1;
    h__ -= h_offset;
    r_dim1 = *ldr;
    r_offset = r_dim1 + 1;
    r__ -= r_offset;
    --iw;
    --w;

    /* Function Body */
    *nzr = 0;
    if (*nz == 0) {
	return 0;
    }
    mnz = min(*nz,*maxnz);
/*     ------------------------------------------------------------------ */
/*     Compute  Z'HZ  and store the upper-triangular symmetric part in */
/*     the first  mnZ  columns of R. */
/*     ------------------------------------------------------------------ */
    i__1 = mnz;
    for (k = 1; k <= i__1; ++k) {
	dload_(n, &c_b132, &wrk[1], &c__1);
	if (*unitq) {
/*           Only bounds are in the working set.  The k-th column of Z */
/*           is just a column of the identity matrix. */
	    jthcol = kx[k];
	    wrk[jthcol] = 1.;
	} else {
/*           Expand the column of Z into an n-vector. */
	    i__2 = *nfree;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		j = kx[i__];
		wrk[j] = q[i__ + k * q_dim1];
/* L120: */
	    }
	    jthcol = 0;
	}
/*        Set  R(*,k)  =  top of  H*(column of Z). */
	(*qphess)(n, ldh, &jthcol, &h__[h_offset], &wrk[1], &hz[1], &iw[1], 
		leniw, &w[1], lenw);
	cmqmul_(&c__4, n, nz, nfree, ldq, unitq, &kx[1], &hz[1], &q[q_offset],
		 &wrk[1]);
	dcopy_(&mnz, &hz[1], &c__1, &r__[k * r_dim1 + 1], &c__1);
/*        Update an estimate of the size of the reduced Hessian. */
/* Computing MAX */
	d__2 = *hsize, d__3 = (d__1 = r__[k + k * r_dim1], abs(d__1));
	*hsize = max(d__2,d__3);
/* L200: */
    }
/*     ------------------------------------------------------------------ */
/*     Form the Cholesky factorization R'R = Z'HZ as far as possible, */
/*     using symmetric interchanges. */
/*     ------------------------------------------------------------------ */
    dmin__ = *tolrnk * *hsize;
    i__1 = mnz;
    for (j = 1; j <= i__1; ++j) {
/*        Find the maximum diagonal of the Schur complement. */
	i__2 = mnz - j + 1;
	i__3 = *ldr + 1;
	kmax = j - 1 + idamax_(&i__2, &r__[j + j * r_dim1], &i__3);
	dmax__ = r__[kmax + kmax * r_dim1];
/*        See if the diagonal is big enough. */
	if (dmax__ <= dmin__) {
	    return 0;
	}
/*        Perform a symmetric interchange if necessary. */
	if (kmax != j) {
	    if (*unitq) {
		k = kx[kmax];
		kx[kmax] = kx[j];
		kx[j] = k;
	    } else {
		dswap_(nfree, &q[kmax * q_dim1 + 1], &c__1, &q[j * q_dim1 + 1]
			, &c__1);
	    }
	    if (*ngq > 0) {
		dswap_(ngq, &gq[kmax + gq_dim1], n, &gq[j + gq_dim1], n);
	    }
	    i__2 = kmax - j;
	    dswap_(&i__2, &r__[j + 1 + kmax * r_dim1], &c__1, &r__[j + (j + 1)
		     * r_dim1], ldr);
	    dswap_(&j, &r__[j * r_dim1 + 1], &c__1, &r__[kmax * r_dim1 + 1], &
		    c__1);
	    i__2 = mnz - kmax + 1;
	    dswap_(&i__2, &r__[kmax + kmax * r_dim1], ldr, &r__[j + kmax * 
		    r_dim1], ldr);
	}
/*        Set the diagonal element of R. */
	d__ = sqrt(dmax__);
	r__[j + j * r_dim1] = d__;
	++(*nzr);
	if (j < mnz) {
/*           Set the super-diagonal elements of this row of R and update */
/*           the elements of the Schur complement. */
	    i__2 = mnz - j;
	    d__1 = 1. / d__;
	    dscal_(&i__2, &d__1, &r__[j + (j + 1) * r_dim1], ldr);
	    i__2 = mnz - j;
	    dsyr_("U", &i__2, &c_b198, &r__[j + (j + 1) * r_dim1], ldr, &r__[
		    j + 1 + (j + 1) * r_dim1], ldr, (ftnlen)1);
	}
/* L400: */
    }
/*     end of qpcrsh */
    return 0;
} /* qpcrsh_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int qpdflt_(integer *n, integer *nclin, char *title, ftnlen 
	title_len)
{
    /* Initialized data */

    static char noyes[3*2] = " No" "Yes";
    static char icrsh[4*3] = "Cold" "Warm" "Hot ";
    static char qptype[7*10] = "     FP" "     LP" "    QP1" "    QP2" "    "
	    "QP3" "    QP4" "       " "       " "       " "illegal";

    /* Format strings */
    static char fmt_2000[] = "(//\002 Parameters\002/\002 ----------\002)";
    static char fmt_2100[] = "(/\002 Problem type...........\002,3x,a7/\002 "
	    "Linear constraints.....\002,i10,2x,1x,a4,\002 start............"
	    ".\002,12x,\002 Min. Sum of Infeas.....\002,7x,a3/\002 Variables."
	    ".............\002,i10,2x,\002 Infinite bound size....\002,1p,e10"
	    ".2,2x,\002 Feasibility tolerance..\002,1p,e10.2/\002 Hessian row"
	    "s...........\002,i10,2x,\002 Infinite step size.....\002,1p,e10."
	    "2,2x,\002 Optimality tolerance...\002,e10.2/\002 Check frequency"
	    "........\002,i10,2x,\002 Expand frequency.......\002,i10,2x,\002"
	    " Crash tolerance........\002,e10.2/\002 Max degrees of freedom"
	    ".\002,i10,2x,\002 Max active constraints.\002,i10,2x,\002 Rank t"
	    "olerance.........\002,e10.2/\002 Max free variables.....\002,i10)"
	    ;
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
    static cilist io___209 = { 0, 0, 0, fmt_2000, 0 };
    static cilist io___210 = { 0, 0, 0, fmt_2100, 0 };
    static cilist io___211 = { 0, 0, 0, fmt_2200, 0 };


/*     ================================================================== */
/*     qpdflt loads the default values of parameters not set by the user. */

/*     30 Dec 1986: first version. */
/*     04 Nov 2001: Current version of  qpdflt. */
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
	lcpar1_1.lprob = 4;
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
    if (lcpar1_1.mm < 0 || lcpar1_1.mm > *n) {
	lcpar1_1.mm = *n;
    }
    if (lcpar1_1.lprob <= 2) {
	lcpar1_1.mm = 0;
    }
    if (lcpar1_1.maxnz < 0 && lcpar1_1.mm >= 0) {
	lcpar1_1.maxnz = lcpar1_1.mm;
    }
    if (lcpar1_1.maxnz == 0) {
	lcpar1_1.maxnz = 1;
    }
    if (lcpar1_1.maxnz < 1 || lcpar1_1.maxnz > *n) {
	lcpar1_1.maxnz = *n;
    }
    if (lcpar1_1.mxfree < 1 || lcpar1_1.mxfree > *n) {
	lcpar1_1.mxfree = *n;
    }
    if (lcpar1_1.mxfree < lcpar1_1.maxnz) {
	lcpar1_1.mxfree = lcpar1_1.maxnz;
    }
    if (lcpar1_1.minsum < 0) {
	lcpar1_1.minsum = 0;
    }
    if (*nclin < *n && lcpar1_1.lprob <= 2) {
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
    if (lcpar2_1.tolrnk <= 0.) {
	lcpar2_1.tolrnk = epsmch * 100.;
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
	    io___209.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___209);
	    e_wsfe();
	    io___210.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___210);
	    do_fio(&c__1, qptype + (lcpar1_1.lprob - 1) * 7, (ftnlen)7);
	    do_fio(&c__1, (char *)&(*nclin), (ftnlen)sizeof(integer));
	    do_fio(&c__1, icrsh + (lcpar1_1.lcrash << 2), (ftnlen)4);
	    do_fio(&c__1, noyes + lcpar1_1.minsum * 3, (ftnlen)3);
	    do_fio(&c__1, (char *)&(*n), (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&lcpar2_1.bigbnd, (ftnlen)sizeof(doublereal)
		    );
	    do_fio(&c__1, (char *)&lcpar2_1.tolfea, (ftnlen)sizeof(doublereal)
		    );
	    do_fio(&c__1, (char *)&lcpar1_1.mm, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&lcpar2_1.bigdx, (ftnlen)sizeof(doublereal))
		    ;
	    do_fio(&c__1, (char *)&lcpar2_1.tolopt, (ftnlen)sizeof(doublereal)
		    );
	    do_fio(&c__1, (char *)&lcpar1_1.kchk, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&sol3lc_1.kdegen, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&lcpar2_1.tolact, (ftnlen)sizeof(doublereal)
		    );
	    do_fio(&c__1, (char *)&lcpar1_1.maxnz, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&lcpar1_1.maxact, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&lcpar2_1.tolrnk, (ftnlen)sizeof(doublereal)
		    );
	    do_fio(&c__1, (char *)&lcpar1_1.mxfree, (ftnlen)sizeof(integer));
	    e_wsfe();
	    io___211.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___211);
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
/*     end of qpdflt */
} /* qpdflt_ */

#undef msglvl
#undef rprmlc
#undef iprmlc


/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int qpgetd_(logical *delreg, logical *posdef, logical *
	statpt, logical *unitgz, logical *unitq, integer *n, integer *nclin, 
	integer *nfree, integer *lda, integer *ldq, integer *ldr, integer *
	nzr, integer *issave, integer *jdsave, integer *kx, doublereal *dnorm,
	 doublereal *gzdz, doublereal *a, doublereal *ad, doublereal *d__, 
	doublereal *gq, doublereal *r__, doublereal *q, doublereal *v)
{
    /* System generated locals */
    integer a_dim1, a_offset, r_dim1, r_offset, q_dim1, q_offset, i__1;

    /* Local variables */
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *), dnrm2_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int dload_(integer *, doublereal *, doublereal *, 
	    integer *), dscal_(integer *, doublereal *, doublereal *, integer 
	    *), dgemv_(char *, integer *, integer *, doublereal *, doublereal 
	    *, integer *, doublereal *, integer *, doublereal *, doublereal *,
	     integer *, ftnlen), dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *), dtrsv_(char *, char *, char *, integer *
	    , doublereal *, integer *, doublereal *, integer *, ftnlen, 
	    ftnlen, ftnlen);
    logical dellow;
    extern /* Subroutine */ int cmqmul_(integer *, integer *, integer *, 
	    integer *, integer *, logical *, integer *, doublereal *, 
	    doublereal *, doublereal *);
    logical revers;
    doublereal atd;

/*     ================================================================== */
/*     qpgetd  computes the following quantities for  qpcore. */
/*     (1) The search direction d (and its 2-norm).  The vector d is */
/*         defined as  Z*(dz), where  (dz)  is defined as follows. */
/*         If Hz is positive definite, (dz) is the solution of the */
/*         (nZr x nZr)  triangular system  (Rz)'(Rz)*(dz) = - (gz). */
/*         Otherwise  (dz) is the solution of the triangular system */
/*         (Rz)(dz) =  gamma ez,  where ez is the nZr-th unit vector and */
/*         gamma = -sgn(gq(nZr)). */
/*     (2) The vector Ad,  where A is the matrix of linear constraints. */

/*     Original version written 31-December-1986. */
/*     This version of  qpgetd  dated 21-Dec-1990. */
/*     (C) 1992--1997  Regents of the University of California and the */
/*                     Trustees of Stanford University */
/*     ================================================================== */
    /* Parameter adjustments */
    --v;
    --gq;
    --d__;
    --kx;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    q_dim1 = *ldq;
    q_offset = q_dim1 + 1;
    q -= q_offset;
    r_dim1 = *ldr;
    r_offset = r_dim1 + 1;
    r__ -= r_offset;
    --ad;

    /* Function Body */
    if (*posdef) {
	if (*unitgz) {
	    if (*nzr > 1) {
		i__1 = *nzr - 1;
		dload_(&i__1, &c_b132, &v[1], &c__1);
	    }
	    v[*nzr] = -gq[*nzr] / r__[*nzr + *nzr * r_dim1];
	} else {
	    dcopy_(nzr, &gq[1], &c__1, &v[1], &c__1);
	    dscal_(nzr, &c_b198, &v[1], &c__1);
	    dtrsv_("U", "T", "N", nzr, &r__[r_offset], ldr, &v[1], &c__1, (
		    ftnlen)1, (ftnlen)1, (ftnlen)1);
	}
    } else {
	if (*nzr > 1) {
	    i__1 = *nzr - 1;
	    dload_(&i__1, &c_b132, &v[1], &c__1);
	}
	if (gq[*nzr] > 0.) {
	    v[*nzr] = -1.;
	} else {
	    v[*nzr] = 1.;
	}
    }
/*     Solve  (Rz)*(dz) =  v. */
    dcopy_(nzr, &v[1], &c__1, &d__[1], &c__1);
    dtrsv_("U", "n", "n", nzr, &r__[r_offset], ldr, &d__[1], &c__1, (ftnlen)1,
	     (ftnlen)1, (ftnlen)1);
/*     Compute  d = Zr*(dz)  and its norm.  Find  gz'dz */
    *dnorm = dnrm2_(nzr, &d__[1], &c__1);
    *gzdz = ddot_(nzr, &d__[1], &c__1, &gq[1], &c__1);
    cmqmul_(&c__1, n, nzr, nfree, ldq, unitq, &kx[1], &d__[1], &q[q_offset], &
	    v[1]);
/*     Compute  Ad. */
    if (*nclin > 0) {
	dgemv_("No transpose", nclin, n, &c_b44, &a[a_offset], lda, &d__[1], &
		c__1, &c_b132, &ad[1], &c__1, (ftnlen)12);
    }
    if (*delreg && (*gzdz > 0. || *statpt)) {
/*        --------------------------------------------------------------- */
/*        The reduced-gradient norm is small enough that we need to worry */
/*        about the sign of d.  Make  d  point away from the last deleted */
/*        constraint. */
/*        --------------------------------------------------------------- */
/*        Jdsave  is the index of the last deleted regular constraint. */
	if (*jdsave <= *n) {
	    atd = d__[*jdsave];
	} else {
	    atd = ad[*jdsave - *n];
	}
	dellow = *issave == 1;
	if (dellow) {
	    revers = atd < 0.;
	} else {
	    revers = atd > 0.;
	}
	if (revers) {
	    dscal_(n, &c_b198, &d__[1], &c__1);
	    if (*nclin > 0) {
		dscal_(nclin, &c_b198, &ad[1], &c__1);
	    }
	    *gzdz = -(*gzdz);
	}
    }
/*     end of qpgetd */
    return 0;
} /* qpgetd_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int qphess_(integer *n, integer *ldh, integer *jthcol, 
	doublereal *h__, doublereal *x, doublereal *hx, integer *iw, integer *
	leniw, doublereal *w, integer *lenw)
{
    /* System generated locals */
    integer h_dim1, h_offset, i__1;

    /* Local variables */
    extern /* Subroutine */ int dload_(integer *, doublereal *, doublereal *, 
	    integer *), dgemv_(char *, integer *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, integer *, ftnlen), dcopy_(integer *, doublereal *, 
	    integer *, doublereal *, integer *);
    integer lenrx;
    extern /* Subroutine */ int dtrmv_(char *, char *, char *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, ftnlen, ftnlen, 
	    ftnlen), dsymv_(char *, integer *, doublereal *, doublereal *, 
	    integer *, doublereal *, integer *, doublereal *, doublereal *, 
	    integer *, ftnlen);
    integer jp1, num;

/*     ================================================================== */
/*     qpHess  is used to compute the product Hx, where H is the QP */
/*     Hessian matrix stored in H and x is an n-vector. */

/*     If lqptyp is 3 ('QP1') or 4 ('QP2'), the upper-half of the */
/*     two-dimensional array  H  contains the elements of the */
/*     symmetric Hessian matrix H. */
/*     The value of  mHess  defines the dimension of the mHess x mHess */
/*     leading principal minor of H. */
/*     The value of mHess is input as the option 'Hessian Rows' with */
/*     default value n.  The sub-diagonal elements of H are not */
/*     referenced. */

/*     If lqptyp is 5 ('QP3') or 6 ('QP4'), the Hessian is of the form */
/*     H = R'R, where  R  is an mHess x n upper-trapezoidal matrix.  The */
/*     factor R is stored in the first mHess rows of the upper half of */
/*     the two-dimensional array H.  The sub-diagonal elements of  R  are */
/*     not referenced. */

/*     Original F66 Version written    March-1982. */
/*     This version of qpHess dated 16-Jan-1995. */
/*     (C) 1992--1997  Regents of the University of California and the */
/*                     Trustees of Stanford University */
/*     ================================================================== */
    /* Parameter adjustments */
    --hx;
    --x;
    h_dim1 = *ldh;
    h_offset = h_dim1 + 1;
    h__ -= h_offset;
    --iw;
    --w;

    /* Function Body */
    if (sol1qp_1.lqptyp == 3 || sol1qp_1.lqptyp == 4) {
/*        Problem type QP1 and QP2. */
	if (*jthcol > 0) {
/*           ------------------------------------------------------------ */
/*           Special case -- extract one column of H. */
/*           ------------------------------------------------------------ */
	    if (*jthcol > sol1qp_1.mhess) {
		dload_(&sol1qp_1.mhess, &c_b132, &hx[1], &c__1);
	    } else {
		dcopy_(jthcol, &h__[*jthcol * h_dim1 + 1], &c__1, &hx[1], &
			c__1);
		num = sol1qp_1.mhess - *jthcol;
		jp1 = *jthcol + 1;
		if (num > 0) {
		    dcopy_(&num, &h__[*jthcol + jp1 * h_dim1], ldh, &hx[jp1], 
			    &c__1);
		}
	    }
	} else {
/*           ------------------------------------------------------------ */
/*           Normal case. */
/*           ------------------------------------------------------------ */
	    dsymv_("upper-triangular", &sol1qp_1.mhess, &c_b44, &h__[h_offset]
		    , ldh, &x[1], &c__1, &c_b132, &hx[1], &c__1, (ftnlen)16);
	}
	if (*n > sol1qp_1.mhess) {
	    i__1 = *n - sol1qp_1.mhess;
	    dload_(&i__1, &c_b132, &hx[sol1qp_1.mhess + 1], &c__1);
	}
    } else if (sol1qp_1.lqptyp == 5 || sol1qp_1.lqptyp == 6) {
/*        Problem type qp3 and qp4. */
	if (sol1qp_1.mhess == 0) {
	    dload_(n, &c_b132, &hx[1], &c__1);
	} else if (*jthcol > 0) {
/*           ------------------------------------------------------------ */
/*           Special case -- extract one column of H. */
/*           ------------------------------------------------------------ */
	    lenrx = min(*jthcol,sol1qp_1.mhess);
	    dcopy_(&lenrx, &h__[*jthcol * h_dim1 + 1], &c__1, &hx[1], &c__1);
	} else {
/*           ------------------------------------------------------------ */
/*           Normal case. */
/*           ------------------------------------------------------------ */
	    dcopy_(&sol1qp_1.mhess, &x[1], &c__1, &hx[1], &c__1);
	    dtrmv_("U", "N", "N", &sol1qp_1.mhess, &h__[h_offset], ldh, &hx[1]
		    , &c__1, (ftnlen)1, (ftnlen)1, (ftnlen)1);
	    if (*n > sol1qp_1.mhess) {
		i__1 = *n - sol1qp_1.mhess;
		dgemv_("N", &sol1qp_1.mhess, &i__1, &c_b44, &h__[(
			sol1qp_1.mhess + 1) * h_dim1 + 1], ldh, &x[
			sol1qp_1.mhess + 1], &c__1, &c_b44, &hx[1], &c__1, (
			ftnlen)1);
	    }
	    lenrx = sol1qp_1.mhess;
	}
	if (*n > lenrx) {
	    i__1 = *n - lenrx;
	    dgemv_("T", &lenrx, &i__1, &c_b44, &h__[(lenrx + 1) * h_dim1 + 1],
		     ldh, &hx[1], &c__1, &c_b132, &hx[lenrx + 1], &c__1, (
		    ftnlen)1);
	}
	dtrmv_("U", "T", "N", &lenrx, &h__[h_offset], ldh, &hx[1], &c__1, (
		ftnlen)1, (ftnlen)1, (ftnlen)1);
    }
/*     end of qpHess */
    return 0;
} /* qphess_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int qploc_(logical *cset, integer *n, integer *nclin, 
	integer *litotl, integer *lwtotl)
{
    integer lenq, minw, lwrk, lencq, lrlam, miniw, lenrt, ld, lq, lr, lt, 
	    lfeatu, lkactv, lanorm, lwtinf, lad, lcq, lgq, lhx, lkx;

/*     ================================================================== */
/*     qploc   allocates the addresses of the work arrays for qpcore. */

/*     Note that the arrays ( gq, cq ) lie in contiguous areas of */
/*     workspace. */

/*     Original version written  2-January-1987. */
/*     This version of qploc dated  18-Nov-1990. */
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
    lhx = lad + *nclin;
    ld = lhx + *n;
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
    sol1lc_1.loclc[5] = lhx;
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
/*     end of qploc */
    return 0;
} /* qploc_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int qpprm_(char *string, ftnlen string_len)
{
    extern /* Subroutine */ int lpprm_(char *, ftnlen);

/*     ================================================================== */
/*     qpprm   loads the option supplied in  string  into the relevant */
/*     element of  iprmlc  or  rprmlc. */
/*     ================================================================== */
    lpprm_(string, string_len);
    return 0;
} /* qpprm_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int qpprmi_(char *string, integer *ivalue, ftnlen string_len)
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
/*     qpprmi decodes the option contained in  string // ivalue. */

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
/*     end of qpprmi */
    return 0;
} /* qpprmi_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int qpprmr_(char *string, doublereal *rvalue, ftnlen 
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
/*     qpprmr decodes the option contained in  string // rvalue. */

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
/*     end of qpprmr */
    return 0;
} /* qpprmr_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int qpprms_(integer *ioptns, integer *inform__)
{
    extern /* Subroutine */ int lpprms_(integer *, integer *);

/*     ================================================================== */
/*     qpprms  just calls lpprms, which reads the options file from unit */
/*     ioptns  and loads the options into the relevant elements of */
/*     iprmlc  and  rprmlc. */
/*     ================================================================== */
    lpprms_(ioptns, inform__);
/*     end of qpprms */
    return 0;
} /* qpprms_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int qpprnt_(char *prbtyp, logical *header, logical *rset, 
	integer *msglvl, integer *iter, integer *isdel, integer *jdel, 
	integer *jadd, integer *n, integer *nclin, integer *nactiv, integer *
	nfree, integer *nz, integer *nzr, integer *ldr, integer *ldt, integer 
	*istate, doublereal *alfa, doublereal *condrz, doublereal *condt, 
	doublereal *dzz, doublereal *gzrnrm, integer *numinf, doublereal *
	suminf, integer *notopt, doublereal *objqp, doublereal *trusml, 
	doublereal *ax, doublereal *r__, doublereal *t, doublereal *x, 
	doublereal *work, ftnlen prbtyp_len)
{
    /* Initialized data */

    static char lstate[2*6] = "  " "L " "U " "E " "F " "A ";

    /* Format strings */
    static char fmt_1000[] = "(///\002 \002,a2,\002 iteration\002,i5/\002 =="
	    "===============\002)";
    static char fmt_1300[] = "(//\002 Itn Jdel  Jadd     Step Ninf  Sinf/Obj"
	    "ective\002,\002 Norm gZ   Zr  Art  Bnd  Lin NOpt    Min Lm  Cond"
	    " T\002,\002  Cond Rz     Rzz\002)";
    static char fmt_1200[] = "(//\002 Itn Jdel  Jadd     Step Ninf  Sinf/Obj"
	    "ective\002,\002 Norm gZ   Zr  Art  Bnd  Lin NOpt    Min Lm  Cond"
	    " T\002)";
    static char fmt_1700[] = "(i4,i5,a1,i5,a1,1p,e8.1,i5,e16.8,e8.1,2i5,2i5,"
	    "a15,e8.0,2a9)";
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
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char ladd[2];
    integer kadd, kdel;
    char ldel[2], ldzz[9];
    integer j, k;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *);
    char lmchar[15], lcondr[9];
    logical prthdr, newset;
    integer ndf;
    doublereal obj;
    integer itn;

    /* Fortran I/O blocks */
    static cilist io___245 = { 0, 0, 0, fmt_1000, 0 };
    static cilist io___258 = { 0, 0, 0, fmt_1300, 0 };
    static cilist io___259 = { 0, 0, 0, fmt_1200, 0 };
    static cilist io___260 = { 0, 0, 0, fmt_1700, 0 };
    static cilist io___261 = { 0, 0, 0, fmt_1100, 0 };
    static cilist io___262 = { 0, 0, 0, fmt_1700, 0 };
    static cilist io___263 = { 0, 0, 0, fmt_2000, 0 };
    static cilist io___264 = { 0, 0, 0, fmt_2100, 0 };
    static cilist io___266 = { 0, 0, 0, fmt_2200, 0 };
    static cilist io___268 = { 0, 0, 0, fmt_3000, 0 };
    static cilist io___269 = { 0, 0, 0, fmt_3100, 0 };
    static cilist io___270 = { 0, 0, 0, fmt_5000, 0 };


/*     ================================================================== */
/*     qpprnt prints various levels of output for qpcore. */

/*           msg        cumulative result */
/*           ---        ----------------- */

/*       .le.  0        no output. */

/*       .eq.  1        nothing now (but full output later). */

/*       .eq.  5        one terse line of output. */

/*       .ge. 10        same as 5 (but full output later). */

/*       .ge. 20        constraint status,  x  and  Ax. */

/*       .ge. 30        diagonals of  T  and  R. */


/*     Original version of qpprnt written by PEG, 31-October-1984. */
/*     This version of  qpprnt  dated  23-Dec-92. */
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
	io___245.ciunit = sol1cm_1.iprint;
	s_wsfe(&io___245);
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
	s_copy(ldzz, "         ", (ftnlen)9, (ftnlen)9);
	s_copy(lcondr, "         ", (ftnlen)9, (ftnlen)9);
	if (*rset && *nzr > 0) {
	    ici__1.icierr = 0;
	    ici__1.icirnum = 1;
	    ici__1.icirlen = 9;
	    ici__1.iciunit = lcondr;
	    ici__1.icifmt = "( 1p,e9.1 )";
	    s_wsfi(&ici__1);
	    do_fio(&c__1, (char *)&(*condrz), (ftnlen)sizeof(doublereal));
	    e_wsfi();
	    if (*dzz != 1.) {
		ici__1.icierr = 0;
		ici__1.icirnum = 1;
		ici__1.icirlen = 9;
		ici__1.iciunit = ldzz;
		ici__1.icifmt = "( 1p,e9.1 )";
		s_wsfi(&ici__1);
		do_fio(&c__1, (char *)&(*dzz), (ftnlen)sizeof(doublereal));
		e_wsfi();
	    }
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
	    obj = *objqp;
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
		if (s_cmp(prbtyp, "QP", (ftnlen)2, (ftnlen)2) == 0) {
		    io___258.ciunit = sol1cm_1.iprint;
		    s_wsfe(&io___258);
		    e_wsfe();
		} else {
		    io___259.ciunit = sol1cm_1.iprint;
		    s_wsfe(&io___259);
		    e_wsfe();
		}
		sol1cm_1.lines1 = 0;
	    }
	    io___260.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___260);
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
	    do_fio(&c__1, lcondr, (ftnlen)9);
	    do_fio(&c__1, ldzz, (ftnlen)9);
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
		io___261.ciunit = sol1cm_1.isumm;
		s_wsfe(&io___261);
		e_wsfe();
		sol1cm_1.lines2 = 0;
	    }
	    io___262.ciunit = sol1cm_1.isumm;
	    s_wsfe(&io___262);
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
	    io___263.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___263);
	    do_fio(&c__1, prbtyp, (ftnlen)2);
	    e_wsfe();
	    io___264.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___264);
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		do_fio(&c__1, (char *)&x[j], (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&istate[j], (ftnlen)sizeof(integer));
	    }
	    e_wsfe();
	    if (*nclin > 0) {
		io___266.ciunit = sol1cm_1.iprint;
		s_wsfe(&io___266);
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
		    io___268.ciunit = sol1cm_1.iprint;
		    s_wsfe(&io___268);
		    do_fio(&c__1, prbtyp, (ftnlen)2);
		    i__1 = *nactiv;
		    for (j = 1; j <= i__1; ++j) {
			do_fio(&c__1, (char *)&work[j], (ftnlen)sizeof(
				doublereal));
		    }
		    e_wsfe();
		}
		if (*rset && *nzr > 0) {
		    io___269.ciunit = sol1cm_1.iprint;
		    s_wsfe(&io___269);
		    do_fio(&c__1, prbtyp, (ftnlen)2);
		    i__1 = *nzr;
		    for (j = 1; j <= i__1; ++j) {
			do_fio(&c__1, (char *)&r__[j + j * r_dim1], (ftnlen)
				sizeof(doublereal));
		    }
		    e_wsfe();
		}
	    }
	    io___270.ciunit = sol1cm_1.iprint;
	    s_wsfe(&io___270);
	    e_wsfe();
	}
    }
    *header = FALSE_;
    *jdel = 0;
    *jadd = 0;
    *alfa = 0.;
    return 0;
/*     end of qpprnt */
} /* qpprnt_ */


/* ../src/rzsubs.f -- translated by f2c (version 19980913).
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
    doublereal epspt3, epspt5, epspt8, epspt9;
} sol4cm_;

#define sol4cm_1 sol4cm_

extern struct {
    doublereal asize, dtmax, dtmin;
} sol5cm_;

#define sol5cm_1 sol5cm_

extern struct {
    doublereal wmach[15];
} solmch_;

#define solmch_1 solmch_

/* Table of constant values */

static integer c__1 = 1;
static integer c__8 = 8;
static doublereal c_b9 = 0.;
static doublereal c_b10 = 1.;
static doublereal c_b28 = -1.;

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*     File  rzsubs.f */

/*     rzadd    rzadds   rzdel */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int rzadd_(logical *unitq, logical *rset, integer *inform__, 
	integer *ifix, integer *iadd, integer *jadd, integer *it, integer *
	nactiv, integer *nz, integer *nfree, integer *nzr, integer *ngq, 
	integer *n, integer *lda, integer *ldq, integer *ldr, integer *ldt, 
	integer *kx, doublereal *condmx, doublereal *dzz, doublereal *a, 
	doublereal *r__, doublereal *t, doublereal *gqm, doublereal *q, 
	doublereal *w, doublereal *c__, doublereal *s)
{
    /* Format strings */
    static char fmt_2000[] = "(/\002 XXX  Serious ill-conditioning in the wo"
	    "rking set\002,\002 after adding constraint \002,i5/\002 XXX  Ove"
	    "rflow may occur in subsequent iterations.\002//)";

    /* System generated locals */
    integer a_dim1, a_offset, r_dim1, r_offset, t_dim1, t_offset, gqm_dim1, 
	    gqm_offset, q_dim1, q_offset, i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    doublereal cond;
    extern doublereal ddiv_(doublereal *, doublereal *, logical *);
    integer npiv, nsup;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    integer i__, j, k;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *), f06fqf_(char *, char *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, doublereal *, ftnlen, 
	    ftnlen), dcond_(integer *, doublereal *, integer *, doublereal *, 
	    doublereal *), f06qhf_(char *, integer *, integer *, doublereal *,
	     doublereal *, doublereal *, integer *, ftnlen), f06qkf_(char *, 
	    char *, integer *, doublereal *, integer *, doublereal *, integer 
	    *, ftnlen, ftnlen), f06qnf_(char *, integer *, integer *, integer 
	    *, doublereal *, doublereal *, integer *, ftnlen), f06qrf_(char *,
	     integer *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *, ftnlen);
    logical bound;
    integer nanew;
    extern /* Subroutine */ int f06qvf_(char *, integer *, integer *, integer 
	    *, doublereal *, doublereal *, doublereal *, integer *, ftnlen), 
	    f06qxf_(char *, char *, char *, integer *, integer *, integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, integer *, 
	    ftnlen, ftnlen, ftnlen), dcopy_(integer *, doublereal *, integer *
	    , doublereal *, integer *);
    doublereal dtnew, condbd;
    integer jt;
    logical overfl;
    extern /* Subroutine */ int cmqmul_(integer *, integer *, integer *, 
	    integer *, integer *, logical *, integer *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal tdtmin, tdtmax;

    /* Fortran I/O blocks */
    static cilist io___15 = { 0, 0, 0, fmt_2000, 0 };


/*     ================================================================== */
/*     rzadd   updates the matrices  Z, Y, T, R  and  D  associated with */
/*     factorizations */

/*              A(free) * Q(free)  = (  0 T ) */
/*                        Q(free)  = (  Z Y ) */
/*                      R' *D * R  =   Hz */

/*     a) The matrices  R  and  T  are upper triangular. */
/*     b) The arrays  T  and  R  may be the same array. */
/*     c) The  nactiv x nactiv  upper-triangular matrix  T  is stored */
/*        with its (1,1) element in position  (iT,jT) of the */
/*        array  T.   The integer  jT  is always  nZ+1.  During regular */
/*        changes to the working set,  iT = 1;  when several constraints */
/*        are added simultaneously,  it  points to the first row of the */
/*        existing  T. */
/*     d) The matrix  R  is stored in the first  nZr x nZr  rows */
/*        and columns of the  nfree x nfree  leading principal submatrix */
/*        of the array  R. */
/*     e) If  Rset  is  false,   R  is not touched. */

/*     There are three separate cases to consider (although each case */
/*     shares code with another)... */

/*     (1) A free variable becomes fixed on one of its bounds when there */
/*         are already some general constraints in the working set. */

/*     (2) A free variable becomes fixed on one of its bounds when there */
/*         are only bound constraints in the working set. */

/*     (3) A general constraint (corresponding to row  iadd  of  A) is */
/*         added to the working set. */

/*     In cases (1) and (2), we assume that  kx(ifix) = jadd. */
/*     In all cases,  jadd  is the index of the constraint being added. */

/*     If there are no general constraints in the working set,  the */
/*     matrix  Q = (Z Y)  is the identity and will not be touched. */

/*     If  ngq .gt. 0,  the column transformations are applied to the */
/*     columns of the  (ngq x n)  matrix  gqm'. */

/*     Original version of  rzadd  written by PEG,  31-October-1984. */
/*     This version of  rzadd  dated  28-Aug-1991. */
/*     ================================================================== */
/*     If the condition estimator of the updated T is greater than */
/*     condbd,  a warning message is printed. */
    /* Parameter adjustments */
    --s;
    --c__;
    --w;
    gqm_dim1 = *n;
    gqm_offset = gqm_dim1 + 1;
    gqm -= gqm_offset;
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
    t_dim1 = *ldt;
    t_offset = t_dim1 + 1;
    t -= t_offset;

    /* Function Body */
    condbd = 1. / sol4cm_1.epspt9;
    overfl = FALSE_;
    bound = *jadd <= *n;
    jt = *nz + 1;
    if (bound) {
/*        =============================================================== */
/*        A simple bound has entered the working set.  iadd is not used. */
/*        =============================================================== */
	nanew = *nactiv;
	if (*unitq) {
/*           Q is not stored, but  kx  defines an ordering of the columns */
/*           of the identity matrix that implicitly define Q. */
/*           Define the sequence of pairwise interchanges P that moves */
/*           the newly-fixed variable to position  nfree. */
/*           Reorder  kx  accordingly. */
	    i__1 = *nfree - 1;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (i__ >= *ifix) {
		    w[i__] = (doublereal) (i__ + 1);
		    kx[i__] = kx[i__ + 1];
		} else {
		    w[i__] = (doublereal) i__;
		}
/* L100: */
	    }
	} else {
/*           ------------------------------------------------------------ */
/*           Q  is stored explicitly. */
/*           ------------------------------------------------------------ */
/*           Set  w = the  (ifix)-th  row of  Q. */
/*           Move the  (nfree)-th  row of  Q  to position ifix. */
	    dcopy_(nfree, &q[*ifix + q_dim1], ldq, &w[1], &c__1);
	    if (*ifix < *nfree) {
		dcopy_(nfree, &q[*nfree + q_dim1], ldq, &q[*ifix + q_dim1], 
			ldq);
		kx[*ifix] = kx[*nfree];
	    }
	}
	kx[*nfree] = *jadd;
    } else {
/*        =============================================================== */
/*        A general constraint has entered the working set. */
/*        ifix is not used. */
/*        =============================================================== */
	nanew = *nactiv + 1;
/*        Transform the incoming row of A by Q'. */
	dcopy_(n, &a[*iadd + a_dim1], lda, &w[1], &c__1);
	cmqmul_(&c__8, n, nz, nfree, ldq, unitq, &kx[1], &w[1], &q[q_offset], 
		&c__[1]);
/*        Check that the incoming row is not dependent upon those */
/*        already in the working set. */
	dtnew = dnrm2_(nz, &w[1], &c__1);
	if (*nactiv == 0) {
/*           This is the only general constraint in the working set. */
	    cond = ddiv_(&sol5cm_1.asize, &dtnew, &overfl);
	    tdtmax = dtnew;
	    tdtmin = dtnew;
	} else {
/*           There are already some general constraints in the working */
/*           set.  Update the estimate of the condition number. */
	    tdtmax = max(dtnew,sol5cm_1.dtmax);
	    tdtmin = min(dtnew,sol5cm_1.dtmin);
	    cond = ddiv_(&tdtmax, &tdtmin, &overfl);
	}
	if (cond > *condmx || overfl) {
	    goto L900;
	}
	if (*unitq) {
/*           First general constraint added.  Set  Q = I. */
	    f06qhf_("general", nfree, nfree, &c_b9, &c_b10, &q[q_offset], ldq,
		     (ftnlen)7);
	    *unitq = FALSE_;
	    *it = 0;
	}
    }
    if (bound) {
	npiv = *nfree;
    } else {
	npiv = *nz;
    }
    if (*unitq) {
/*        --------------------------------------------------------------- */
/*        The orthogonal matrix  Q  is not stored explicitly. */
/*        Apply  P, the sequence of pairwise interchanges that moves the */
/*        newly-fixed variable to position  nfree. */
/*        --------------------------------------------------------------- */
	if (*ngq > 0) {
	    i__1 = *nfree - 1;
	    f06qkf_("left", "transpose", &i__1, &w[1], ngq, &gqm[gqm_offset], 
		    n, (ftnlen)4, (ftnlen)9);
	}
	if (*rset) {
/*           Apply the pairwise interchanges to  Rz. */
/*           The subdiagonal elements generated by this process are */
/*           stored in  s(ifix), s(2), ..., s(nZr-1). */
	    nsup = *nzr - *ifix;
	    f06qnf_("right", nzr, ifix, nzr, &s[1], &r__[r_offset], ldr, (
		    ftnlen)5);
	}
    } else {
/*        --------------------------------------------------------------- */
/*        The matrix  Q  is stored explicitly. */
/*        Define a sweep of plane rotations P such that */
/*                           Pw = beta*e(npiv). */
/*        The rotations are applied in the planes (1, 2), (2, 3), ..., */
/*        (npiv-1, npiv).  The rotations must be applied to Q, gqm', R */
/*        and T. */
/*        --------------------------------------------------------------- */
	i__1 = npiv - 1;
	f06fqf_("varble", "forwrds", &i__1, &w[npiv], &w[1], &c__1, &c__[1], &
		s[1], (ftnlen)6, (ftnlen)7);
	if (*ngq > 0) {
	    f06qxf_("left ", "variable", "forwards", &npiv, ngq, &c__1, &npiv,
		     &c__[1], &s[1], &gqm[gqm_offset], n, (ftnlen)5, (ftnlen)
		    8, (ftnlen)8);
	}
	f06qxf_("right", "variable", "forwards", nfree, nfree, &c__1, &npiv, &
		c__[1], &s[1], &q[q_offset], ldq, (ftnlen)5, (ftnlen)8, (
		ftnlen)8);
	if (*rset) {
/*           Apply the rotations to the triangular part of R. */
/*           The subdiagonal elements generated by this process are */
/*           stored in  s(1),  s(2), ..., s(nZr-1). */
	    nsup = *nzr - 1;
	    f06qvf_("right", nzr, &c__1, nzr, &c__[1], &s[1], &r__[r_offset], 
		    ldr, (ftnlen)5);
	}
    }
    if (*rset) {
/*        --------------------------------------------------------------- */
/*        Eliminate the  nsup  subdiagonal elements of  R  stored in */
/*        s(nZr-nsup), ..., s(nZr-1)  with a left-hand sweep of rotations */
/*        in planes (nZr-nsup, nZr-nsup+1), ..., (nZr-1, nZr). */
/*        --------------------------------------------------------------- */
	i__1 = *nzr - nsup;
	f06qrf_("left ", nzr, &i__1, nzr, &c__[1], &s[1], &r__[r_offset], ldr,
		 (ftnlen)5);
	if (nsup > 0 && *dzz != 1.) {
/* Computing 2nd power */
	    d__1 = c__[*nzr - 1];
/* Computing 2nd power */
	    d__2 = s[*nzr - 1];
	    *dzz = d__1 * d__1 + *dzz * (d__2 * d__2);
	}
    }
    if (! (*unitq)) {
	if (bound) {
/*           ------------------------------------------------------------ */
/*           Bound constraint added.   The rotations affect columns */
/*           nZ+1  thru  nfree  of  gqm'  and  T. */
/*           ------------------------------------------------------------ */
/*           The last row and column of  Q  has been transformed to plus */
/*           or minus the unit vector  e(nfree).  We can reconstitute the */
/*           column of gqm' corresponding to the new fixed variable. */
	    if (w[*nfree] < 0.) {
		if (*ngq > 0) {
		    dscal_(ngq, &c_b28, &gqm[*nfree + gqm_dim1], n);
		}
	    }
	    if (*nactiv > 0) {
		t[*it + (jt - 1) * t_dim1] = s[jt - 1] * t[*it + jt * t_dim1];
		t[*it + jt * t_dim1] = c__[jt - 1] * t[*it + jt * t_dim1];
		if (*nactiv > 1) {
		    f06qvf_("right", nactiv, &c__1, nactiv, &c__[jt], &s[jt], 
			    &t[*it + jt * t_dim1], ldt, (ftnlen)5);
		    i__1 = *nactiv - 1;
		    i__2 = *ldt + 1;
		    dcopy_(&i__1, &s[jt], &c__1, &t[*it + 1 + jt * t_dim1], &
			    i__2);
		}
		--jt;
		i__1 = *ldt + 1;
		dcond_(nactiv, &t[*it + jt * t_dim1], &i__1, &tdtmax, &tdtmin)
			;
		cond = ddiv_(&tdtmax, &tdtmin, &overfl);
	    }
	} else {
/*           ------------------------------------------------------------ */
/*           General constraint added.  Install  w  at the front of  T. */
/*           If there is no room,  shift all the rows down one position. */
/*           ------------------------------------------------------------ */
	    --(*it);
	    if (*it <= 0) {
		*it = 1;
		i__1 = *nactiv;
		for (k = 1; k <= i__1; ++k) {
		    j = jt + k - 1;
		    for (i__ = k; i__ >= 1; --i__) {
			t[i__ + 1 + j * t_dim1] = t[i__ + j * t_dim1];
/* L200: */
		    }
/* L210: */
		}
	    }
	    --jt;
	    dcopy_(&nanew, &w[jt], &c__1, &t[*it + jt * t_dim1], ldt);
	}
    }
/*     ================================================================== */
/*     Prepare to exit.  Check the magnitude of the condition estimator. */
/*     ================================================================== */
L900:
    if (nanew > 0) {
	if (cond < *condmx && ! overfl) {
/*           The factorization has been successfully updated. */
	    *inform__ = 0;
	    sol5cm_1.dtmax = tdtmax;
	    sol5cm_1.dtmin = tdtmin;
	    if (cond >= condbd && sol1cm_1.iprint > 0) {
		io___15.ciunit = sol1cm_1.iprint;
		s_wsfe(&io___15);
		do_fio(&c__1, (char *)&(*jadd), (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	} else {
/*           The proposed working set appears to be linearly dependent. */
	    *inform__ = 1;
	}
    }
    return 0;
/*     end of rzadd */
} /* rzadd_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int rzadds_(logical *unitq, logical *vertex, integer *k1, 
	integer *k2, integer *it, integer *nactiv, integer *nartif, integer *
	nz, integer *nfree, integer *nrejtd, integer *ngq, integer *n, 
	integer *ldq, integer *lda, integer *ldt, integer *istate, integer *
	kactiv, integer *kx, doublereal *condmx, doublereal *a, doublereal *t,
	 doublereal *gqm, doublereal *q, doublereal *w, doublereal *c__, 
	doublereal *s)
{
    /* System generated locals */
    integer a_dim1, a_offset, t_dim1, t_offset, gqm_dim1, gqm_offset, q_dim1, 
	    q_offset, i__1, i__2;

    /* Local variables */
    integer iadd, jadd;
    extern /* Subroutine */ int dger_(integer *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    doublereal cond;
    extern doublereal ddiv_(doublereal *, doublereal *, logical *);
    integer ifix;
    logical rset;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    integer i__, j, k, l;
    extern /* Subroutine */ int dcond_(integer *, doublereal *, integer *, 
	    doublereal *, doublereal *);
    doublereal delta;
    extern /* Subroutine */ int f06qhf_(char *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, ftnlen), 
	    dgrfg_(integer *, doublereal *, doublereal *, integer *, 
	    doublereal *, doublereal *);
    integer nzadd;
    extern /* Subroutine */ int dgemv_(char *, integer *, integer *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, integer *, ftnlen), rzadd_(logical *, 
	    logical *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, doublereal 
	    *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *);
    doublereal dtnew;
    integer iswap;
    doublereal rtmax, rnorm;
    integer jt;
    doublereal cndmax;
    integer iartif, inform__;
    logical overfl;
    extern /* Subroutine */ int cmqmul_(integer *, integer *, integer *, 
	    integer *, integer *, logical *, integer *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal tdtmin, tdtmax, rowmax, dzz;

/*     ================================================================== */
/*     rzadds  includes general constraints  k1  thru  k2  as new rows of */
/*     the  TQ  factorization: */
/*              A(free) * Q(free)  = (  0 T ) */
/*                        Q(free)  = (  Z Y ) */

/*     a) The  nactiv x nactiv  upper-triangular matrix  T  is stored */
/*        with its (1,1) element in position  (iT,jT)  of the array  T. */

/*     Original version of  rzadds  written by PEG,  October-31-1984. */
/*     This version of  rzadds  dated  5-Jul-1991. */
/*     ================================================================== */
    /* Parameter adjustments */
    --s;
    --c__;
    --w;
    gqm_dim1 = *n;
    gqm_offset = gqm_dim1 + 1;
    gqm -= gqm_offset;
    --kx;
    --kactiv;
    q_dim1 = *ldq;
    q_offset = q_dim1 + 1;
    q -= q_offset;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    t_dim1 = *ldt;
    t_offset = t_dim1 + 1;
    t -= t_offset;
    --istate;

    /* Function Body */
    rtmax = solmch_1.wmach[7];
    jt = *nz + 1;
/*     Estimate the condition number of the constraints already */
/*     factorized. */
    if (*nactiv == 0) {
	sol5cm_1.dtmax = 0.;
	sol5cm_1.dtmin = 1.;
	if (*unitq) {
/*           First general constraint added.  Set  Q = I. */
	    f06qhf_("general", nfree, nfree, &c_b9, &c_b10, &q[q_offset], ldq,
		     (ftnlen)7);
	    *unitq = FALSE_;
	}
    } else {
	i__1 = *ldt + 1;
	dcond_(nactiv, &t[*it + jt * t_dim1], &i__1, &sol5cm_1.dtmax, &
		sol5cm_1.dtmin);
    }
    i__1 = *k2;
    for (k = *k1; k <= i__1; ++k) {
	iadd = kactiv[k];
	jadd = *n + iadd;
	if (*nactiv < *nfree) {
	    overfl = FALSE_;
/*           Transform the incoming row of  A  by  Q'. */
	    dcopy_(n, &a[iadd + a_dim1], lda, &w[1], &c__1);
	    cmqmul_(&c__8, n, nz, nfree, ldq, unitq, &kx[1], &w[1], &q[
		    q_offset], &s[1]);
/*           Check that the incoming row is not dependent upon those */
/*           already in the working set. */
	    dtnew = dnrm2_(nz, &w[1], &c__1);
	    if (*nactiv == 0) {
/*              This is the first general constraint in the working set. */
		cond = ddiv_(&sol5cm_1.asize, &dtnew, &overfl);
		tdtmax = dtnew;
		tdtmin = dtnew;
	    } else {
/*              There are already some general constraints in the working */
/*              set. Update the estimate of the condition number. */
		tdtmax = max(dtnew,sol5cm_1.dtmax);
		tdtmin = min(dtnew,sol5cm_1.dtmin);
		cond = ddiv_(&tdtmax, &tdtmin, &overfl);
	    }
	    if (cond >= *condmx || overfl) {
/*              --------------------------------------------------------- */
/*              This constraint appears to be dependent on those already */
/*              in the working set.  Skip it. */
/*              --------------------------------------------------------- */
		istate[jadd] = 0;
		kactiv[k] = -kactiv[k];
	    } else {
		if (*nz > 1) {
/*                 ------------------------------------------------------ */
/*                 Use a single column transformation to reduce the first */
/*                 nZ-1  elements of  w  to zero. */
/*                 ------------------------------------------------------ */
/*                 Apply the Householder reflection  I  -  w w'. */
/*                 The reflection is applied to  Z  and gqm so that */
/*                    y  =    Z  * w,   Z    =  Z    -  y w'  and */
/*                    y  =  gqm' * w,   gqm  =  gqm  -  w y', */
/*                 where  w = wrk1 (from Householder), */
/*                 and    y = wrk2 (workspace). */

/*                 Note that delta  has to be stored after the reflection */
/*                 is used. */
		    delta = w[*nz];
		    i__2 = *nz - 1;
		    dgrfg_(&i__2, &delta, &w[1], &c__1, &c_b9, &w[*nz]);
		    if (w[*nz] > 0.) {
			dgemv_("n", nfree, nz, &c_b10, &q[q_offset], ldq, &w[
				1], &c__1, &c_b9, &s[1], &c__1, (ftnlen)1);
			dger_(nfree, nz, &c_b28, &s[1], &c__1, &w[1], &c__1, &
				q[q_offset], ldq);
			if (*ngq > 0) {
			    dgemv_("t", nz, ngq, &c_b10, &gqm[gqm_offset], n, 
				    &w[1], &c__1, &c_b9, &s[1], &c__1, (
				    ftnlen)1);
			    dger_(nz, ngq, &c_b28, &w[1], &c__1, &s[1], &c__1,
				     &gqm[gqm_offset], n);
			}
		    }
		    w[*nz] = delta;
		}
		--(*it);
		--jt;
		++(*nactiv);
		--(*nz);
		dcopy_(nactiv, &w[jt], &c__1, &t[*it + jt * t_dim1], ldt);
		sol5cm_1.dtmax = tdtmax;
		sol5cm_1.dtmin = tdtmin;
	    }
	}
/* L600: */
    }
    if (*nactiv < *k2) {
/*        Some of the constraints were classed as dependent and not */
/*        included in the factorization.  Re-order the part of  kactiv */
/*        that holds the indices of the general constraints in the */
/*        working set.  Move accepted indices to the front and shift */
/*        rejected indices (with negative values) to the end. */
	l = *k1 - 1;
	i__1 = *k2;
	for (k = *k1; k <= i__1; ++k) {
	    i__ = kactiv[k];
	    if (i__ >= 0) {
		++l;
		if (l != k) {
		    iswap = kactiv[l];
		    kactiv[l] = i__;
		    kactiv[k] = iswap;
		}
	    }
/* L700: */
	}
/*        If a vertex is required,  add some temporary bounds. */
/*        We must accept the resulting condition number of the working */
/*        set. */
	if (*vertex) {
	    rset = FALSE_;
	    cndmax = rtmax;
	    dzz = 1.;
	    nzadd = *nz;
	    i__1 = nzadd;
	    for (iartif = 1; iartif <= i__1; ++iartif) {
		if (*unitq) {
		    ifix = *nfree;
		    jadd = kx[ifix];
		} else {
		    rowmax = 0.;
		    i__2 = *nfree;
		    for (i__ = 1; i__ <= i__2; ++i__) {
			rnorm = dnrm2_(nz, &q[i__ + q_dim1], ldq);
			if (rowmax < rnorm) {
			    rowmax = rnorm;
			    ifix = i__;
			}
/* L710: */
		    }
		    jadd = kx[ifix];
		    rzadd_(unitq, &rset, &inform__, &ifix, &iadd, &jadd, it, 
			    nactiv, nz, nfree, nz, ngq, n, lda, ldq, ldt, ldt,
			     &kx[1], &cndmax, &dzz, &a[a_offset], &t[t_offset]
			    , &t[t_offset], &gqm[gqm_offset], &q[q_offset], &
			    w[1], &c__[1], &s[1]);
		}
		--(*nfree);
		--(*nz);
		++(*nartif);
		istate[jadd] = 4;
/* L720: */
	    }
	}
	if (*it > 1) {
/*           ------------------------------------------------------------ */
/*           If some dependent constraints were rejected,  move the */
/*           matrix T  to the top of the array  T. */
/*           ------------------------------------------------------------ */
	    i__1 = *nactiv;
	    for (k = 1; k <= i__1; ++k) {
		j = *nz + k;
		i__2 = k;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    t[i__ + j * t_dim1] = t[*it + i__ - 1 + j * t_dim1];
/* L800: */
		}
/* L810: */
	    }
	}
    }
    *nrejtd = *k2 - *nactiv;
/*     end of rzadds */
    return 0;
} /* rzadds_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int rzdel_(logical *unitq, integer *it, integer *n, integer *
	nactiv, integer *nfree, integer *ngq, integer *nz, integer *nzr, 
	integer *lda, integer *ldq, integer *ldt, integer *jdel, integer *
	kdel, integer *kactiv, integer *kx, doublereal *a, doublereal *t, 
	doublereal *gqm, doublereal *q, doublereal *work, doublereal *c__, 
	doublereal *s)
{
    /* System generated locals */
    integer a_dim1, a_offset, t_dim1, t_offset, gqm_dim1, gqm_offset, q_dim1, 
	    q_offset, i__1, i__2;

    /* Local variables */
    integer jart, npiv, nsup;
    extern /* Subroutine */ int f06baf_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    integer i__, j, k, l;
    extern /* Subroutine */ int dload_(integer *, doublereal *, doublereal *, 
	    integer *), dcond_(integer *, doublereal *, integer *, doublereal 
	    *, doublereal *);
    integer itdel;
    extern /* Subroutine */ int f06qrf_(char *, integer *, integer *, integer 
	    *, doublereal *, doublereal *, doublereal *, integer *, ftnlen), 
	    f06qxf_(char *, char *, char *, integer *, integer *, integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, integer *, 
	    ftnlen, ftnlen, ftnlen), dswap_(integer *, doublereal *, integer *
	    , doublereal *, integer *), dcopy_(integer *, doublereal *, 
	    integer *, doublereal *, integer *);
    doublereal cs;
    integer ir, jt;
    doublereal sn;
    extern integer idamax_(integer *, doublereal *, integer *);
    integer nzr1;

/*     ================================================================== */
/*     RZDEL   updates the matrices  Z, Y  and  T  associated with the */
/*             factorizations */

/*              A(free) * Q(free)  = (  0 T ) */
/*                        Q(free)  = (  Z Y ) */

/*     when a regular, temporary or artificial constraint is deleted */
/*     from the working set. */

/*     The  nactiv x nactiv  upper-triangular matrix  T  is stored */
/*     with its (1,1) element in position  (iT,jT)  of the array  T. */

/*     Original version of  rzdel  written by PEG,  31-October-1984. */
/*     This version of  rzdel  dated 14-Sep-1992. */
/*     ================================================================== */
    /* Parameter adjustments */
    --s;
    --c__;
    --work;
    gqm_dim1 = *n;
    gqm_offset = gqm_dim1 + 1;
    gqm -= gqm_offset;
    --kx;
    --kactiv;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    q_dim1 = *ldq;
    q_offset = q_dim1 + 1;
    q -= q_offset;
    t_dim1 = *ldt;
    t_offset = t_dim1 + 1;
    t -= t_offset;

    /* Function Body */
    jt = *nz + 1;
    if (*jdel > 0) {
/*        Regular constraint or temporary bound deleted. */
	if (*jdel <= *n) {
/*           Case 1.  A simple bound has been deleted. */
/*           =======  Columns  nfree+1  and  ir  of gqm' must be swapped. */
	    ir = *nz + *kdel;
	    itdel = *nactiv + 1;
	    ++(*nfree);
	    if (*nfree < ir) {
		kx[ir] = kx[*nfree];
		kx[*nfree] = *jdel;
		dswap_(ngq, &gqm[*nfree + gqm_dim1], n, &gqm[ir + gqm_dim1], 
			n);
	    }
	    if (! (*unitq)) {
/*              Copy the incoming column of  A(free)  into the end of T. */
		i__1 = *nactiv;
		for (k = 1; k <= i__1; ++k) {
		    i__ = kactiv[k];
		    t[*nactiv - k + 1 + *nfree * t_dim1] = a[i__ + *jdel * 
			    a_dim1];
/* L130: */
		}
/*              Expand  Q  by adding a unit row and column. */
		if (*nfree > 1) {
		    i__1 = *nfree - 1;
		    dload_(&i__1, &c_b9, &q[*nfree + q_dim1], ldq);
		    i__1 = *nfree - 1;
		    dload_(&i__1, &c_b9, &q[*nfree * q_dim1 + 1], &c__1);
		}
		q[*nfree + *nfree * q_dim1] = 1.;
	    }
	} else {
/*           Case 2.  A general constraint has been deleted. */
/*           ======= */
/*           Delete row  iTdel  of  T  and move up the ones below it. */
/*           T  becomes lower Hessenberg. */
	    itdel = *kdel;
	    i__1 = *nactiv;
	    for (k = itdel; k <= i__1; ++k) {
		j = jt + k - 1;
		i__2 = k - 1;
		for (l = itdel; l <= i__2; ++l) {
		    i__ = *it + l - 1;
		    t[i__ + j * t_dim1] = t[i__ + 1 + j * t_dim1];
/* L200: */
		}
/* L210: */
	    }
	    i__1 = *nactiv - 1;
	    for (i__ = *nactiv - itdel + 1; i__ <= i__1; ++i__) {
		kactiv[i__] = kactiv[i__ + 1];
/* L220: */
	    }
	    --(*nactiv);
	}
	++(*nz);
	if (*nactiv == 0) {
	    sol5cm_1.dtmax = 1.;
	    sol5cm_1.dtmin = 1.;
	} else {
/*           ------------------------------------------------------------ */
/*           Restore the nactiv x (nactiv+1) upper-Hessenberg matrix  T */
/*           to upper-triangular form.  The  nsup  super-diagonal */
/*           elements are removed by a backward sweep of rotations. */
/*           The rotation for the  (1,1)-th  element of  T  is generated */
/*           separately. */
/*           ------------------------------------------------------------ */
	    nsup = itdel - 1;
	    if (nsup > 0) {
		npiv = jt + itdel - 1;
		if (nsup > 1) {
		    i__1 = nsup - 1;
		    i__2 = *ldt + 1;
		    dcopy_(&i__1, &t[*it + 1 + (jt + 1) * t_dim1], &i__2, &s[
			    jt + 1], &c__1);
		    f06qrf_("right", nactiv, &c__1, &nsup, &c__[jt + 1], &s[
			    jt + 1], &t[*it + (jt + 1) * t_dim1], ldt, (
			    ftnlen)5);
		}
		f06baf_(&t[*it + (jt + 1) * t_dim1], &t[*it + jt * t_dim1], &
			cs, &sn);
		t[*it + jt * t_dim1] = 0.;
		s[jt] = -sn;
		c__[jt] = cs;
		f06qxf_("right", "variable", "backwards", nfree, nfree, nz, &
			npiv, &c__[1], &s[1], &q[q_offset], ldq, (ftnlen)5, (
			ftnlen)8, (ftnlen)9);
		f06qxf_("left ", "variable", "backwards", &npiv, ngq, nz, &
			npiv, &c__[1], &s[1], &gqm[gqm_offset], n, (ftnlen)5, 
			(ftnlen)8, (ftnlen)9);
	    }
	    ++jt;
	    i__1 = *ldt + 1;
	    dcond_(nactiv, &t[*it + jt * t_dim1], &i__1, &sol5cm_1.dtmax, &
		    sol5cm_1.dtmin);
	}
    }
    nzr1 = *nzr + 1;
    if (*nz > *nzr) {
	if (*jdel > 0) {
	    i__1 = *nz - nzr1 + 1;
	    jart = nzr1 - 1 + idamax_(&i__1, &gqm[nzr1 + gqm_dim1], &c__1);
	} else {
	    jart = -(*jdel);
	}
	if (jart > nzr1) {
/*           Swap columns  nZr1  and  jArt  of  Q  and  gqm. */
	    if (*unitq) {
		k = kx[nzr1];
		kx[nzr1] = kx[jart];
		kx[jart] = k;
	    } else {
		dswap_(nfree, &q[nzr1 * q_dim1 + 1], &c__1, &q[jart * q_dim1 
			+ 1], &c__1);
	    }
	    dswap_(ngq, &gqm[nzr1 + gqm_dim1], n, &gqm[jart + gqm_dim1], n);
	}
    }
    *nzr = nzr1;
/*     end of rzdel */
    return 0;
} /* rzdel_ */


/* ../src/cmsubs.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

extern struct {
    integer iprint, isumm, lines1, lines2;
} sol1cm_;

#define sol1cm_1 sol1cm_

/* Table of constant values */

static integer c__1 = 1;
static doublereal c_b40 = 0.;
static doublereal c_b47 = 1.;

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*     File  cmsubs.f */

/*     cmprnt   cmqmul   cmr1md   cmrswp */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int cmprnt_(integer *msglvl, integer *n, integer *nclin, 
	integer *nctotl, doublereal *bigbnd, logical *named, char *names, 
	integer *istate, doublereal *bl, doublereal *bu, doublereal *clamda, 
	doublereal *featol, doublereal *r__, ftnlen names_len)
{
    /* Initialized data */

    static char lstate[2*7] = "--" "++" "FR" "LL" "UL" "EQ" "TF";

    /* Format strings */
    static char fmt_1000[] = "(//1x,a15,2x,\002State\002,6x,\002Value\002,"
	    "7x,\002Lower bound\002,5x,\002Upper bound\002,3x,\002Lagr multip"
	    "lier\002,4x,\002   Slack\002/)";
    static char fmt_2000[] = "(1x,a8,i6,3x,a1,1x,a2,4g16.7,g16.4)";

    /* System generated locals */
    integer i__1;
    cilist ci__1;
    icilist ici__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfi(icilist *), e_wsfi(void);

    /* Local variables */
    char name__[8], line[102];
    doublereal wlam;
    integer j;
    char state[2];
    integer nplin;
    doublereal b1, b2, rj;
    integer is, number;
    char key[1];
    doublereal slk, tol, slk1, slk2;

    /* Fortran I/O blocks */
    static cilist io___2 = { 0, 0, 0, fmt_1000, 0 };
    static cilist io___11 = { 0, 0, 0, fmt_1000, 0 };
    static cilist io___12 = { 0, 0, 0, fmt_1000, 0 };


/*     ================================================================== */
/*     cmprnt  prints r(x) (x,  A*x and c(x)), the bounds, the */
/*     multipliers, and the slacks (distance to the nearer bound). */

/*     Original Fortran 77 version written  October 1984. */
/*     This version of  cmprnt dated  11-May-95. */
/*     ================================================================== */
    /* Parameter adjustments */
    --r__;
    --featol;
    --clamda;
    --bu;
    --bl;
    --istate;
    names -= 16;

    /* Function Body */
    if (sol1cm_1.iprint == 0 || *msglvl < 10 && *msglvl != 1) {
	return 0;
    }
    io___2.ciunit = sol1cm_1.iprint;
    s_wsfe(&io___2);
    do_fio(&c__1, "Variable       ", (ftnlen)15);
    e_wsfe();
    s_copy(name__, "variable", (ftnlen)8, (ftnlen)8);
    nplin = *n + *nclin;
    i__1 = *nctotl;
    for (j = 1; j <= i__1; ++j) {
	b1 = bl[j];
	b2 = bu[j];
	wlam = clamda[j];
	rj = r__[j];
	if (j <= *n) {
	    number = j;
	} else if (j <= nplin) {
	    number = j - *n;
	    if (number == 1) {
		io___11.ciunit = sol1cm_1.iprint;
		s_wsfe(&io___11);
		do_fio(&c__1, "Linear constrnt", (ftnlen)15);
		e_wsfe();
		s_copy(name__, "lincon  ", (ftnlen)8, (ftnlen)8);
	    }
	} else {
	    number = j - nplin;
	    if (number == 1) {
		io___12.ciunit = sol1cm_1.iprint;
		s_wsfe(&io___12);
		do_fio(&c__1, "Nonlin constrnt", (ftnlen)15);
		e_wsfe();
		s_copy(name__, "nlncon  ", (ftnlen)8, (ftnlen)8);
	    }
	}
/*        Print a line for the jth variable or constraint. */
/*        ------------------------------------------------ */
	is = istate[j];
	s_copy(state, lstate + (is + 2 << 1), (ftnlen)2, (ftnlen)2);
	tol = featol[j];
	slk1 = rj - b1;
	slk2 = b2 - rj;
	if (abs(slk1) < abs(slk2)) {
	    slk = slk1;
	    if (b1 <= -(*bigbnd)) {
		slk = slk2;
	    }
	} else {
	    slk = slk2;
	    if (b2 >= *bigbnd) {
		slk = slk1;
	    }
	}
/*        Flag infeasibilities, primal and dual degeneracies, */
/*        and active QP constraints that are loose in NP. */

	*(unsigned char *)key = ' ';
	if (slk1 < -tol || slk2 < -tol) {
	    *(unsigned char *)key = 'I';
	}
	if (is == 0 && abs(slk) <= tol) {
	    *(unsigned char *)key = 'D';
	}
	if (is >= 1 && abs(wlam) <= tol) {
	    *(unsigned char *)key = 'A';
	}
	ici__1.icierr = 0;
	ici__1.icirnum = 1;
	ici__1.icirlen = 102;
	ici__1.iciunit = line;
	ici__1.icifmt = fmt_2000;
	s_wsfi(&ici__1);
	do_fio(&c__1, name__, (ftnlen)8);
	do_fio(&c__1, (char *)&number, (ftnlen)sizeof(integer));
	do_fio(&c__1, key, (ftnlen)1);
	do_fio(&c__1, state, (ftnlen)2);
	do_fio(&c__1, (char *)&rj, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&b1, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&b2, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&wlam, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&slk, (ftnlen)sizeof(doublereal));
	e_wsfi();
/*        Reset special cases: */
/*           Infinite bounds */
/*           Zero bounds */
/*           Lagrange multipliers for inactive constraints */
/*           Lagrange multipliers for infinite bounds */
/*           Infinite slacks */
/*           Zero slacks */
	if (*named) {
	    s_copy(line + 1, names + (j << 4), (ftnlen)16, (ftnlen)16);
	}
	if (b1 <= -(*bigbnd)) {
	    s_copy(line + 38, "      None      ", (ftnlen)16, (ftnlen)16);
	}
	if (b2 >= *bigbnd) {
	    s_copy(line + 54, "      None      ", (ftnlen)16, (ftnlen)16);
	}
	if (b1 == 0.) {
	    s_copy(line + 38, "        .       ", (ftnlen)16, (ftnlen)16);
	}
	if (b2 == 0.) {
	    s_copy(line + 54, "        .       ", (ftnlen)16, (ftnlen)16);
	}
	if (is == 0 || wlam == 0.) {
	    s_copy(line + 70, "        .       ", (ftnlen)16, (ftnlen)16);
	}
	if (b1 <= -(*bigbnd) && b2 >= *bigbnd) {
	    s_copy(line + 70, "                ", (ftnlen)16, (ftnlen)16);
	    s_copy(line + 86, "                ", (ftnlen)16, (ftnlen)16);
	}
	if (slk == 0.) {
	    s_copy(line + 86, "        .       ", (ftnlen)16, (ftnlen)16);
	}
	ci__1.cierr = 0;
	ci__1.ciunit = sol1cm_1.iprint;
	ci__1.cifmt = "(a)";
	s_wsfe(&ci__1);
	do_fio(&c__1, line, (ftnlen)102);
	e_wsfe();
/* L500: */
    }
    return 0;
/*     end of cmprnt */
} /* cmprnt_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int cmqmul_(integer *mode, integer *n, integer *nz, integer *
	nfree, integer *ldq, logical *unitq, integer *kx, doublereal *v, 
	doublereal *q, doublereal *w)
{
    /* System generated locals */
    integer q_dim1, q_offset, i__1;

    /* Local variables */
    integer lenv, j, k, l;
    extern /* Subroutine */ int dload_(integer *, doublereal *, doublereal *, 
	    integer *), dgemv_(char *, integer *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, integer *, ftnlen), dcopy_(integer *, doublereal *, 
	    integer *, doublereal *, integer *);
    integer j1, j2, nfixed;

/*     ================================================================== */
/*     cmqmul  transforms the vector  v  in various ways using the */
/*     matrix  Q = ( Z  Y )  defined by the input parameters. */

/*        Mode               Result */
/*        ----               ------ */

/*          1                v = Z v */
/*          2                v = Y v */
/*          3                v = Q v */

/*     On input,  v  is assumed to be ordered as  ( v(free)  v(fixed) ). */
/*     On output, v  is a full n-vector. */


/*          4                v = Z'v */
/*          5                v = Y'v */
/*          6                v = Q'v */

/*     On input,  v  is a full n-vector. */
/*     On output, v  is ordered as  ( v(free)  v(fixed) ). */

/*          7                v = Y'v */
/*          8                v = Q'v */

/*     On input,  v  is a full n-vector. */
/*     On output, v  is as in modes 5 and 6 except that v(fixed) is not */
/*     set. */

/*     Modes  1, 4, 7 and 8  do not involve  v(fixed). */
/*     Original F66 version  April 1983. */
/*     Fortran 77 version written  9-February-1985. */
/*     Level 2 BLAS added 10-June-1986. */
/*     This version of cmqmul dated 14-Sep-92. */
/*     ================================================================== */
    /* Parameter adjustments */
    --w;
    --v;
    --kx;
    q_dim1 = *ldq;
    q_offset = q_dim1 + 1;
    q -= q_offset;

    /* Function Body */
    nfixed = *n - *nfree;
    j1 = 1;
    j2 = *nfree;
    if (*mode == 1 || *mode == 4) {
	j2 = *nz;
    }
    if (*mode == 2 || *mode == 5 || *mode == 7) {
	j1 = *nz + 1;
    }
    lenv = j2 - j1 + 1;
    if (*mode <= 3) {
/*        =============================================================== */
/*        Mode = 1, 2  or  3. */
/*        =============================================================== */
	if (*nfree > 0) {
	    dload_(nfree, &c_b40, &w[1], &c__1);
	}
/*        Copy  v(fixed)  into the end of wrk. */
	if (*mode >= 2 && nfixed > 0) {
	    dcopy_(&nfixed, &v[*nfree + 1], &c__1, &w[*nfree + 1], &c__1);
	}
/*        Set  w  =  relevant part of  Qv. */
	if (lenv > 0) {
	    if (*unitq) {
		dcopy_(&lenv, &v[j1], &c__1, &w[j1], &c__1);
	    } else {
		i__1 = j2 - j1 + 1;
		dgemv_("n", nfree, &i__1, &c_b47, &q[j1 * q_dim1 + 1], ldq, &
			v[j1], &c__1, &c_b47, &w[1], &c__1, (ftnlen)1);
	    }
	}
/*        Expand  w  into  v  as a full n-vector. */
	dload_(n, &c_b40, &v[1], &c__1);
	i__1 = *nfree;
	for (k = 1; k <= i__1; ++k) {
	    j = kx[k];
	    v[j] = w[k];
/* L220: */
	}
/*        Copy  w(fixed)  into the appropriate parts of  v. */
	if (*mode > 1) {
	    i__1 = nfixed;
	    for (l = 1; l <= i__1; ++l) {
		j = kx[*nfree + l];
		v[j] = w[*nfree + l];
/* L320: */
	    }
	}
    } else {
/*        =============================================================== */
/*        Mode = 4, 5, 6, 7  or  8. */
/*        =============================================================== */
/*        Put the fixed components of  v  into the end of w. */
	if (*mode == 5 || *mode == 6) {
	    i__1 = nfixed;
	    for (l = 1; l <= i__1; ++l) {
		j = kx[*nfree + l];
		w[*nfree + l] = v[j];
/* L420: */
	    }
	}
/*        Put the free  components of  v  into the beginning of  w. */
	if (*nfree > 0) {
	    i__1 = *nfree;
	    for (k = 1; k <= i__1; ++k) {
		j = kx[k];
		w[k] = v[j];
/* L520: */
	    }
/*           Set  v  =  relevant part of  Q' * w. */
	    if (lenv > 0) {
		if (*unitq) {
		    dcopy_(&lenv, &w[j1], &c__1, &v[j1], &c__1);
		} else {
		    i__1 = j2 - j1 + 1;
		    dgemv_("T", nfree, &i__1, &c_b47, &q[j1 * q_dim1 + 1], 
			    ldq, &w[1], &c__1, &c_b40, &v[j1], &c__1, (ftnlen)
			    1);
		}
	    }
	}
/*        Copy the fixed components of  w  into the end of v. */
	if (nfixed > 0 && (*mode == 5 || *mode == 6)) {
	    dcopy_(&nfixed, &w[*nfree + 1], &c__1, &v[*nfree + 1], &c__1);
	}
    }
/*     end of cmqmul */
    return 0;
} /* cmqmul_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int cmr1md_(integer *n, integer *nu, integer *nrank, integer 
	*ldr, integer *lenv, integer *lenw, doublereal *r__, doublereal *u, 
	doublereal *v, doublereal *w, doublereal *c__, doublereal *s)
{
    /* System generated locals */
    integer r_dim1, r_offset, u_dim1, u_offset, i__1, i__2;

    /* Local variables */
    integer j;
    extern /* Subroutine */ int f06fqf_(char *, char *, integer *, doublereal 
	    *, doublereal *, integer *, doublereal *, doublereal *, ftnlen, 
	    ftnlen), f06qsf_(char *, integer *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, ftnlen), 
	    f06qxf_(char *, char *, char *, integer *, integer *, integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, integer *, 
	    ftnlen, ftnlen, ftnlen), f06qwf_(char *, integer *, integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, integer *, 
	    ftnlen), daxpy_(integer *, doublereal *, doublereal *, integer *, 
	    doublereal *, integer *);

/*     ================================================================== */
/*     cmr1md  modifies the  nrank*n  upper-triangular matrix  R  so that */
/*     Q*(R + v*w')  is upper triangular,  where  Q  is orthogonal, */
/*     v  and  w  are vectors, and the modified  R  overwrites the old. */
/*     Q  is the product of two sweeps of plane rotations (not stored). */
/*     If required,  the rotations are applied to the nu columns of */
/*     the matrix  U. */

/*     The matrix v*w' is an (lenv) by (lenw) matrix. */
/*     The vector v is overwritten. */

/*     Systems Optimization Laboratory, Stanford University. */
/*     Original version   October  1984. */
/*     Level-2 matrix routines added 22-Apr-1988. */
/*     This version of  cmr1md  dated 22-Apr-1988. */
/*     ================================================================== */
    /* Parameter adjustments */
    --s;
    --c__;
    --w;
    --v;
    u_dim1 = *n;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    r_dim1 = *ldr;
    r_offset = r_dim1 + 1;
    r__ -= r_offset;

    /* Function Body */
    j = min(*lenv,*nrank);
    if (*nrank > 0) {
/*        --------------------------------------------------------------- */
/*        Reduce  v to beta*e( j )  using a backward sweep of rotations */
/*        in planes (j-1, j), (j-2, j), ..., (1, j). */
/*        --------------------------------------------------------------- */
	i__1 = j - 1;
	f06fqf_("Fixed", "Backwards", &i__1, &v[j], &v[1], &c__1, &c__[1], &s[
		1], (ftnlen)5, (ftnlen)9);
/*        --------------------------------------------------------------- */
/*        Apply the sequence of rotations to U. */
/*        --------------------------------------------------------------- */
	if (*nu > 0) {
	    f06qxf_("Left", "Bottom", "Backwards", &j, nu, &c__1, &j, &c__[1],
		     &s[1], &u[u_offset], n, (ftnlen)4, (ftnlen)6, (ftnlen)9);
	}
/*        --------------------------------------------------------------- */
/*        Apply the sequence of rotations to R. This generates a spike in */
/*        the j-th row of R, which is stored in s. */
/*        --------------------------------------------------------------- */
	f06qwf_("Left", n, &c__1, &j, &c__[1], &s[1], &r__[r_offset], ldr, (
		ftnlen)4);
/*        --------------------------------------------------------------- */
/*        Form  beta*e(j)*w' + R.  This a spiked matrix, with a row */
/*        spike in row j. */
/*        --------------------------------------------------------------- */
/* Computing MIN */
	i__2 = j - 1;
	i__1 = min(i__2,*lenw);
	daxpy_(&i__1, &v[j], &w[1], &c__1, &s[1], &c__1);
	i__1 = *lenw - j + 1;
	daxpy_(&i__1, &v[j], &w[j], &c__1, &r__[j + j * r_dim1], ldr);
/*        --------------------------------------------------------------- */
/*        Eliminate the spike using a forward sweep of rotations in */
/*        planes (1, j), (2, j), ..., (j-1, j). */
/*        --------------------------------------------------------------- */
	f06qsf_("Left", n, &c__1, &j, &c__[1], &s[1], &r__[r_offset], ldr, (
		ftnlen)4);
/*        --------------------------------------------------------------- */
/*        Apply the rotations to U. */
/*        --------------------------------------------------------------- */
	if (*nu > 0) {
	    f06qxf_("Left", "Bottom", "Forwards", &j, nu, &c__1, &j, &c__[1], 
		    &s[1], &u[u_offset], n, (ftnlen)4, (ftnlen)6, (ftnlen)8);
	}
    }
/*     end of cmr1md */
    return 0;
} /* cmr1md_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int cmrswp_(integer *n, integer *nu, integer *nrank, integer 
	*ldr, integer *i__, integer *j, doublereal *r__, doublereal *u, 
	doublereal *c__, doublereal *s)
{
    /* System generated locals */
    integer r_dim1, r_offset, u_dim1, u_offset, i__1;

    /* Local variables */
    extern /* Subroutine */ int dload_(integer *, doublereal *, doublereal *, 
	    integer *), f06fqf_(char *, char *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, doublereal *, ftnlen, 
	    ftnlen), f06qsf_(char *, integer *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, ftnlen);
    integer lenrj;
    extern /* Subroutine */ int f06qxf_(char *, char *, char *, integer *, 
	    integer *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *, ftnlen, ftnlen, ftnlen), f06qwf_(char *, 
	    integer *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *, ftnlen), dswap_(integer *, doublereal *, 
	    integer *, doublereal *, integer *);

/*     ================================================================== */
/*     CMRSWP  interchanges the  i-th  and  j-th  (i .lt. j)  columns of */
/*     an  nrank x n  upper-trapezoidal matrix  R   and restores the */
/*     resulting matrix to upper-trapezoidal form using two sweeps of */
/*     plane rotations applied on the left.  R is overwritten. */

/*     If nU .gt. 0,  the rotations are applied to the  nU  columns of */
/*     the matrix  U. */

/*     Systems Optimization Laboratory, Stanford University. */
/*     Original version written 31-October-1984. */
/*     Level-2 matrix routines added 13-May-1988. */
/*     This version of  cmrswp  dated  26-Aug-1991. */
/*     ================================================================== */
/*     Swap the elements of the i-th and j-th columns of R on, or above, */
/*     the main diagonal. */
    /* Parameter adjustments */
    --s;
    --c__;
    u_dim1 = *n;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    r_dim1 = *ldr;
    r_offset = r_dim1 + 1;
    r__ -= r_offset;

    /* Function Body */
    i__1 = min(*i__,*nrank);
    dswap_(&i__1, &r__[*i__ * r_dim1 + 1], &c__1, &r__[*j * r_dim1 + 1], &
	    c__1);
    lenrj = min(*j,*nrank);
    if (lenrj > *i__) {
/*        --------------------------------------------------------------- */
/*        Reduce elements  R(i+1,j), ..., R(lenRj,j)  to  beta*e(lenRj) */
/*        using a backward sweep in planes */
/*        (lenRj-1,lenRj), (lenRj-2,lenRj), ..., (i+1,lenRj). */
/*        If required, apply the sequence of rotations to U. */
/*        --------------------------------------------------------------- */
	i__1 = lenrj - *i__ - 1;
	f06fqf_("Fixed", "Backwards", &i__1, &r__[lenrj + *j * r_dim1], &r__[*
		i__ + 1 + *j * r_dim1], &c__1, &c__[*i__ + 1], &s[*i__ + 1], (
		ftnlen)5, (ftnlen)9);
	if (*nu > 0) {
	    i__1 = *i__ + 1;
	    f06qxf_("Left", "Bottom", "Backwards", n, nu, &i__1, &lenrj, &c__[
		    1], &s[1], &u[u_offset], n, (ftnlen)4, (ftnlen)6, (ftnlen)
		    9);
	}
/*        Put zeros into the j-th column of R in positions corresponding */
/*        to the sub-diagonals of the i-th column. */
	s[*i__] = r__[lenrj + *j * r_dim1];
	i__1 = lenrj - *i__;
	dload_(&i__1, &c_b40, &r__[*i__ + 1 + *j * r_dim1], &c__1);
/*        Apply the sequence of rotations to R.  This generates a spike */
/*        in the (lenRj)-th row of R, which is stored in s. */
	i__1 = *i__ + 1;
	f06qwf_("Left", n, &i__1, &lenrj, &c__[1], &s[1], &r__[r_offset], ldr,
		 (ftnlen)4);
/*        Eliminate the spike using a forward sweep in planes */
/*        (i,lenRj), (i+1,lenRj), ..., (lenRj-1,lenRj). */
/*        If necessary, apply the sequence of rotations to U. */
	f06qsf_("Left", n, i__, &lenrj, &c__[1], &s[1], &r__[r_offset], ldr, (
		ftnlen)4);
	if (*nu > 0) {
	    f06qxf_("Left", "Bottom", "Forwards", &lenrj, nu, i__, &lenrj, &
		    c__[1], &s[1], &u[u_offset], n, (ftnlen)4, (ftnlen)6, (
		    ftnlen)8);
	}
    }
/*     end of cmrswp */
    return 0;
} /* cmrswp_ */


/* ../src/f06subs.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

extern struct {
    doublereal wmach[15];
} solmch_;

#define solmch_1 solmch_

/* Table of constant values */

static integer c__1 = 1;
static doublereal c_b9 = 1.;

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*     File  f06.f */
/*     A subset of the NAG F06 Chapter with some modifications. */
/*     The routines perform the same function as the NAG F06 routines. */

/*                         Level 0  F06  Scalar routines */
/*                         -------  ---- --------------- */
/*     f06aaz+         f06baf/drot3g+  f06bcf/dcsg+    ddiv+ */
/*     f06bmf/dnorm+ */

/*                         Level 1  F06  Vector routines */
/*                         -------  ---  --------------- */
/*     iload/f06dbf    dload/f06fbf    dddiv           ddscl/f06fcf */
/*     icopy/f06dff    dssq/f06fjf+    dcond/f06flf */
/*     idrank/f06klf+  f06fqf/dsrotg   dgrfg/f06frf+ */

/*                         Level 2  F06  Matrix routines */
/*                         -------  ---  --------------- */
/*     f06qff          f06qgf          f06qhf */
/*     f06qkf          f06qnf          f06qrf          f06qsf */
/*     f06qtf          f06qvf          f06qwf          f06qxf */
/*     f06qzf */

/*    +Differs from the Nag F06 version. */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int f06aaz_(char *srname, integer *info, ftnlen srname_len)
{
    /* Format strings */
    static char fmt_99999[] = "(\002 ** On entry to \002,a13,\002 parameter "
	    "number \002,i2,\002 had an illegal value\002)";

    /* System generated locals */
    icilist ici__1;

    /* Builtin functions */
    integer s_wsfi(icilist *), do_fio(integer *, char *, ftnlen), e_wsfi(void)
	    ;

    /* Local variables */
    char rec[80*1];

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     Modified by PEG 9/25/88. */
/*     .. Scalar Arguments .. */
/*     .. */

/*  Purpose */
/*  ======= */

/*  F06AAZ  is an error handler for the Level 2 BLAS routines. */

/*  It is called by the Level 2 BLAS routines if an input parameter is */
/*  invalid. */

/*  Parameters */
/*  ========== */

/*  SRNAME - CHARACTER*13. */
/*           On entry, SRNAME specifies the name of the routine which */
/*           called F06AAZ. */

/*  INFO   - INTEGER. */
/*           On entry, INFO specifies the position of the invalid */
/*           parameter in the parameter-list of the calling routine. */


/*  Auxiliary routine for Level 2 Blas. */

/*  Written on 20-July-1986. */

/*     .. Local Scalars .. */
/*     .. */
/*     .. Executable Statements .. */
    ici__1.icierr = 0;
    ici__1.icirnum = 1;
    ici__1.icirlen = 80;
    ici__1.iciunit = rec;
    ici__1.icifmt = fmt_99999;
    s_wsfi(&ici__1);
    do_fio(&c__1, srname, (ftnlen)13);
    do_fio(&c__1, (char *)&(*info), (ftnlen)sizeof(integer));
    e_wsfi();

    return 0;


/*     End of F06AAZ. */

} /* f06aaz_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int f06baf_(doublereal *x, doublereal *y, doublereal *cs, 
	doublereal *sn)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal a, b;
    static doublereal rtmin, eps;


/*  Note: f06baf/drot3g is different from the Nag routine f06baf. */

/*  f06baf  generates a plane rotation that reduces the vector (X, Y) to */
/*  the vector (A, 0),  where A is defined as follows... */

/*     If both X and Y are negligibly small, or */
/*     if Y is negligible relative to Y, */
/*     then  A = X,  and the identity rotation is returned. */

/*     If X is negligible relative to Y, */
/*     then  A = Y,  and the swap rotation is returned. */

/*     Otherwise,  A = sign(X) * sqrt( X**2 + Y**2 ). */

/*  In all cases,  X and Y are overwritten by A and 0,  and CS will lie */
/*  in the closed interval (0, 1).  Also,  the absolute value of CS and */
/*  SN (if nonzero) will be no less than the machine precision,  EPS. */

/*  DROT3G  guards against overflow and underflow. */
/*  It is assumed that  FLMIN .lt. EPS**2  (i.e.  RTMIN .lt. EPS). */

/*  Systems Optimization Laboratory, Stanford University. */
/*  Original version dated January 1982. */
/*  F77 version dated 28-June-1986. */
/*  This version of DROT3G dated 28-June-1986. */

    if (first) {
	first = FALSE_;
	eps = solmch_1.wmach[2];
	rtmin = solmch_1.wmach[5];
    }
    if (*y == 0.) {
	*cs = 1.;
	*sn = 0.;
    } else if (*x == 0.) {
	*cs = 0.;
	*sn = 1.;
	*x = *y;
    } else {
	a = abs(*x);
	b = abs(*y);
	if (max(a,b) <= rtmin) {
	    *cs = 1.;
	    *sn = 0.;
	} else {
	    if (a >= b) {
		if (b <= eps * a) {
		    *cs = 1.;
		    *sn = 0.;
		    goto L900;
		} else {
/* Computing 2nd power */
		    d__1 = b / a;
		    a *= sqrt(d__1 * d__1 + 1.);
		}
	    } else {
		if (a <= eps * b) {
		    *cs = 0.;
		    *sn = 1.;
		    *x = *y;
		    goto L900;
		} else {
/* Computing 2nd power */
		    d__1 = a / b;
		    a = b * sqrt(d__1 * d__1 + 1.);
		}
	    }
	    if (*x < 0.) {
		a = -a;
	    }
	    *cs = *x / a;
	    *sn = *y / a;
	    *x = a;
	}
    }
L900:
    *y = 0.;
/*     end of  f06baf (drot3g). */
    return 0;
} /* f06baf_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int f06bcf_(doublereal *t, doublereal *c__, doublereal *s)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal abst;
    static doublereal rteps, rrteps, eps;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     MARK 13 REVISED. IER-602 (MAR 1988). */
/*     .. Scalar Arguments .. */
/*     .. */

/*  F06BCF returns values c and s such that */

/*     c = cos( theta ),   s = sin( theta ) */

/*  for a given value of */

/*     t = tan( theta ). */

/*  c is always non-negative and s has the same sign as t, so that */

/*     c = 1.0/sqrt( 1.0 + t**2 ),   s = t/sqrt( 1.0 + t**2 ). */

/*  Nag Fortran 77 O( 1 ) basic linear algebra routine. */

/*  -- Written on 28-February-1986. */
/*     Sven Hammarling, Nag Central Office. */
/*  -- Modified 19-August-1987. */
/*     Sven Hammarling and Jeremy Du Croz, Nag Central Office. */
/*        No longer sets s to zero when t is less than eps. */
/*  -- Modified 24-July-1991. */
/*     Philip E. Gill, UCSD. */
/*        Modified to call mchpar instead of x02ajf */

/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. External Functions .. */
/* +    DOUBLE PRECISION   X02AJF */
/* +    EXTERNAL           X02AJF */
/*     .. Intrinsic Functions .. */
/*     .. Save statement .. */
/*     .. Data statements .. */
/*     .. */
/*     .. Executable Statements .. */
    if (first) {
	first = FALSE_;
	eps = solmch_1.wmach[2];
/* +       eps    = x02ajf( ) */
	rteps = sqrt(eps);
	rrteps = 1 / rteps;
    }

    abst = abs(*t);
    if (abst < rteps) {
	*c__ = 1.;
	*s = *t;
    } else if (abst > rrteps) {
	*c__ = 1 / abst;
	*s = d_sign(&c_b9, t);
    } else {
/* Computing 2nd power */
	d__1 = abst;
	*c__ = 1 / sqrt(d__1 * d__1 + 1);
	*s = *c__ * *t;
    }

/*     end of f06bcf. ( scsg ) */
    return 0;
} /* f06bcf_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
doublereal ddiv_(doublereal *a, doublereal *b, logical *fail)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal absb;
    static doublereal flmin, flmax;
    doublereal div;

/*     DOUBLE PRECISION          F06BLF */
/*     ENTRY                     F06BLF( A, B, FAIL ) */
/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     Modified by PEG 9/25/88. */
/*     .. Scalar Arguments .. */
/*     .. */

/*  F06BLF returns the value div given by */

/*     div = ( a/b                 if a/b does not overflow, */
/*           ( */
/*           ( 0.0                 if a .eq. 0.0, */
/*           ( */
/*           ( sign( a/b )*flmax   if a .ne. 0.0  and a/b would overflow, */

/*  where  flmax  is a large value, via the function name. In addition if */
/*  a/b would overflow then  fail is returned as true, otherwise  fail is */
/*  returned as false. */

/*  Note that when  a and b  are both zero, fail is returned as true, but */
/*  div  is returned as  0.0. In all other cases of overflow  div is such */
/*  that  abs( div ) = flmax. */

/*  When  b = 0  then  sign( a/b )  is taken as  sign( a ). */

/*  Nag Fortran 77 O( 1 ) basic linear algebra routine. */

/*  -- Written on 26-October-1982. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. Intrinsic Functions .. */
/*     .. Save statement .. */
/*     .. Data statements .. */
/*     .. */
/*     .. Executable Statements .. */
    if (*a == 0.) {
	div = 0.;
	if (*b == 0.) {
	    *fail = TRUE_;
	} else {
	    *fail = FALSE_;
	}
    } else {

	if (first) {
	    first = FALSE_;
	    flmin = solmch_1.wmach[4];
	    flmax = solmch_1.wmach[6];
	}

	if (*b == 0.) {
	    div = d_sign(&flmax, a);
	    *fail = TRUE_;
	} else {
	    absb = abs(*b);
	    if (absb >= 1.) {
		*fail = FALSE_;
		if (abs(*a) >= absb * flmin) {
		    div = *a / *b;
		} else {
		    div = 0.;
		}
	    } else {
		if (abs(*a) <= absb * flmax) {
		    *fail = FALSE_;
		    div = *a / *b;
		} else {
		    *fail = TRUE_;
		    div = flmax;
		    if (*a < 0. && *b > 0. || *a > 0. && *b < 0.) {
			div = -div;
		    }
		}
	    }
	}
    }

    ret_val = div;
/*     F06BLF = DIV */

/*     end of f06blf. ( ddiv ) */
    return ret_val;
} /* ddiv_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
doublereal f06bmf_(doublereal *scale, doublereal *ssq)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal norm;
    static doublereal flmax;
    doublereal sqt;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     Modified by PEG 9/25/88. */
/*     .. Scalar Arguments .. */
/*     .. */

/*  F06BMF returns the value norm given by */

/*     norm = ( scale*sqrt( ssq ), scale*sqrt( ssq ) .lt. flmax */
/*            ( */
/*            ( flmax,             scale*sqrt( ssq ) .ge. flmax */

/*  via the function name. */


/*  Nag Fortran 77 O( 1 ) basic linear algebra routine. */

/*  -- Written on 22-October-1982. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Local Scalars .. */
/*     .. Intrinsic Functions .. */
/*     .. Save statement .. */
/*     .. Data statements .. */
/*     .. */
/*     .. Executable Statements .. */
    if (first) {
	first = FALSE_;
	flmax = solmch_1.wmach[6];
    }

    sqt = sqrt(*ssq);
    if (*scale < flmax / sqt) {
	norm = *scale * sqt;
    } else {
	norm = flmax;
    }

    ret_val = norm;

/*     end of f06bmf. ( dnorm ) */
    return ret_val;
} /* f06bmf_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int iload_(integer *n, integer *const__, integer *x, integer 
	*incx)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer ix;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     ENTRY      F06DBF( N, CONST, X, INCX ) */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  iload/f06dbf performs the operation */

/*     x = const*e,   e' = ( 1  1 ... 1 ). */


/*  Nag Fortran 77 O( n ) basic linear algebra routine. */

/*  -- Written on 18-February-1986. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Local Scalars .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n > 0) {
	if (*const__ != 0) {
	    i__1 = (*n - 1) * *incx + 1;
	    i__2 = *incx;
	    for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
		x[ix] = *const__;
/* L10: */
	    }
	} else {
	    i__2 = (*n - 1) * *incx + 1;
	    i__1 = *incx;
	    for (ix = 1; i__1 < 0 ? ix >= i__2 : ix <= i__2; ix += i__1) {
		x[ix] = 0;
/* L20: */
	    }
	}
    }

/*     end of iload (f06dbf) */

    return 0;
} /* iload_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int dload_(integer *n, doublereal *const__, doublereal *x, 
	integer *incx)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer ix;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     ENTRY      F06FBF( N, CONST, X, INCX ) */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  dload/f06fbf performs the operation */

/*     x = const*e,   e' = ( 1  1 ... 1 ). */


/*  Nag Fortran 77 O( n ) basic linear algebra routine. */

/*  -- Written on 22-September-1983. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n > 0) {
	if (*const__ != 0.) {
	    i__1 = (*n - 1) * *incx + 1;
	    i__2 = *incx;
	    for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
		x[ix] = *const__;
/* L10: */
	    }
	} else {
	    i__2 = (*n - 1) * *incx + 1;
	    i__1 = *incx;
	    for (ix = 1; i__1 < 0 ? ix >= i__2 : ix <= i__2; ix += i__1) {
		x[ix] = 0.;
/* L20: */
	    }
	}
    }

/*     end of dload (f06fbf) */
    return 0;
} /* dload_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int ddscl_(integer *n, doublereal *d__, integer *incd, 
	doublereal *x, integer *incx)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *);
    integer id, ix;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     ENTRY      F06FCF( N, D, INCD, X, INCX ) */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  ddscl/f06fcf performs the operation */

/*     x := diag( d )*x */


/*  Nag Fortran 77 O( n ) basic linear algebra routine. */

/*  -- Written on 22-September-1983. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Local Scalars .. */
/*     .. External Subroutines .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --x;
    --d__;

    /* Function Body */
    if (*n > 0) {
	if (*incd == 0 && *incx != 0) {
	    i__1 = abs(*incx);
	    dscal_(n, &d__[1], &x[1], &i__1);
	} else if (*incd == *incx && *incd > 0) {
	    i__1 = (*n - 1) * *incd + 1;
	    i__2 = *incd;
	    for (id = 1; i__2 < 0 ? id >= i__1 : id <= i__1; id += i__2) {
		x[id] = d__[id] * x[id];
/* L10: */
	    }
	} else {
	    if (*incx >= 0) {
		ix = 1;
	    } else {
		ix = 1 - (*n - 1) * *incx;
	    }
	    if (*incd > 0) {
		i__2 = (*n - 1) * *incd + 1;
		i__1 = *incd;
		for (id = 1; i__1 < 0 ? id >= i__2 : id <= i__2; id += i__1) {
		    x[ix] = d__[id] * x[ix];
		    ix += *incx;
/* L20: */
		}
	    } else {
		id = 1 - (*n - 1) * *incd;
		i__1 = *n;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    x[ix] = d__[id] * x[ix];
		    id += *incd;
		    ix += *incx;
/* L30: */
		}
	    }
	}
    }

/*     end of ddscl (f06fcf) */
    return 0;
} /* ddscl_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int dddiv_(integer *n, doublereal *d__, integer *incd, 
	doublereal *x, integer *incx)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *);
    integer id, ix;

/*     dddiv  performs the diagonal scaling  x  =  x / d. */
    /* Parameter adjustments */
    --x;
    --d__;

    /* Function Body */
    if (*n > 0) {
	if (*incd == 0 && *incx != 0) {
	    d__1 = 1. / d__[1];
	    i__1 = abs(*incx);
	    dscal_(n, &d__1, &x[1], &i__1);
	} else if (*incd == *incx && *incd > 0) {
	    i__1 = (*n - 1) * *incd + 1;
	    i__2 = *incd;
	    for (id = 1; i__2 < 0 ? id >= i__1 : id <= i__1; id += i__2) {
		x[id] /= d__[id];
/* L10: */
	    }
	} else {
	    if (*incx >= 0) {
		ix = 1;
	    } else {
		ix = 1 - (*n - 1) * *incx;
	    }
	    if (*incd > 0) {
		i__2 = (*n - 1) * *incd + 1;
		i__1 = *incd;
		for (id = 1; i__1 < 0 ? id >= i__2 : id <= i__2; id += i__1) {
		    x[ix] /= d__[id];
		    ix += *incx;
/* L20: */
		}
	    } else {
		id = 1 - (*n - 1) * *incd;
		i__1 = *n;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    x[ix] /= d__[id];
		    id += *incd;
		    ix += *incx;
/* L30: */
		}
	    }
	}
    }
/*     end of dddiv */
    return 0;
} /* dddiv_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int icopy_(integer *n, integer *x, integer *incx, integer *y,
	 integer *incy)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer i__, ix, iy;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     ENTRY      F06DFF( N, X, INCX, Y, INCY ) */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  F06DFF performs the operation */

/*     y := x */


/*  Nag Fortran 77 O( n ) basic linear algebra routine. */

/*  -- Written on 10-February-1986. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Local Scalars .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --y;
    --x;

    /* Function Body */
    if (*n > 0) {
	if (*incx == *incy && *incy > 0) {
	    i__1 = (*n - 1) * *incy + 1;
	    i__2 = *incy;
	    for (iy = 1; i__2 < 0 ? iy >= i__1 : iy <= i__1; iy += i__2) {
		y[iy] = x[iy];
/* L10: */
	    }
	} else {
	    if (*incx >= 0) {
		ix = 1;
	    } else {
		ix = 1 - (*n - 1) * *incx;
	    }
	    if (*incy > 0) {
		i__2 = (*n - 1) * *incy + 1;
		i__1 = *incy;
		for (iy = 1; i__1 < 0 ? iy >= i__2 : iy <= i__2; iy += i__1) {
		    y[iy] = x[ix];
		    ix += *incx;
/* L20: */
		}
	    } else {
		iy = 1 - (*n - 1) * *incy;
		i__1 = *n;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    y[iy] = x[ix];
		    iy += *incy;
		    ix += *incx;
/* L30: */
		}
	    }
	}
    }

/*     end of icopy (icopy) */
    return 0;
} /* icopy_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int f06fjf_(integer *n, doublereal *x, integer *incx, 
	doublereal *scale, doublereal *sumsq)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Local variables */
    doublereal absxi;
    integer ix;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  f06fjf   returns the values scl and smsq such that */

/*     ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq, */

/*  where x( i ) = X( 1 + ( i - 1 )*INCX ). The value of sumsq is assumed */
/*  to be at least unity and the value of smsq will then satisfy */

/*     1.0 .le. smsq .le. ( sumsq + n ) . */

/*  scale is assumed to be non-negative and scl returns the value */

/*     scl = max( scale, abs( x( i ) ) ) . */

/*  scale and sumsq must be supplied in SCALE and SUMSQ respectively. */
/*  scl and smsq are overwritten on SCALE and SUMSQ respectively. */

/*  The routine makes only one pass through the vector X. */


/*  Nag Fortran 77 O( n ) basic linear algebra routine. */

/*  -- Written on 22-October-1982. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n > 0) {
	i__1 = (*n - 1) * *incx + 1;
	i__2 = *incx;
	for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
	    if (x[ix] != 0.) {
		absxi = (d__1 = x[ix], abs(d__1));
		if (*scale < absxi) {
/* Computing 2nd power */
		    d__1 = *scale / absxi;
		    *sumsq = *sumsq * (d__1 * d__1) + 1;
		    *scale = absxi;
		} else {
/* Computing 2nd power */
		    d__1 = absxi / *scale;
		    *sumsq += d__1 * d__1;
		}
	    }
/* L10: */
	}
    }

/*     end of f06fjf */
    return 0;
} /* f06fjf_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int dcond_(integer *n, doublereal *x, integer *incx, 
	doublereal *xmax, doublereal *xmin)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2, d__3;

    /* Local variables */
    integer ix;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     ENTRY      F06FLF( N, X, INCX, XMAX, XMIN ) */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  dcond/f06flf returns the values xmax and xmin given by */

/*     xmax = max( abs( x( i ) ) ),   xmin = min( abs( x( i ) ) ). */
/*             i                              i */

/*  If n is less than unity then xmax and xmin are returned as zero. */

/*  Nag Fortran 77 O( n ) basic linear algebra routine. */

/*  -- Written on 27-February-1986. */
/*     Sven Hammarling, Nag Central Office. */

/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n < 1) {
	*xmax = 0.;
	*xmin = 0.;
    } else {
	*xmax = abs(x[1]);
	*xmin = *xmax;
	i__1 = (*n - 1) * *incx + 1;
	i__2 = *incx;
	for (ix = *incx + 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
/* Computing MAX */
	    d__2 = *xmax, d__3 = (d__1 = x[ix], abs(d__1));
	    *xmax = max(d__2,d__3);
/* Computing MIN */
	    d__2 = *xmin, d__3 = (d__1 = x[ix], abs(d__1));
	    *xmin = min(d__2,d__3);
/* L10: */
	}
    }

/*     end of dcond (f06flf) */
    return 0;
} /* dcond_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
integer idrank_(integer *n, doublereal *x, integer *incx, doublereal *tol)
{
    /* System generated locals */
    integer ret_val;
    doublereal d__1, d__2, d__3;

    /* Local variables */
    doublereal xmax;
    integer k;
    doublereal tl;
    integer ix;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     INTEGER          F06KLF */
/*     ENTRY            F06KLF( N, X, INCX, TOL ) */
/*     Modified by PEG 9/25/88. */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  idrank/ F06KLF finds the first element of the n-vector x for which */

/*     abs( x( k ) ).le.tol*max( abs( x( 1 ) ), ..., abs( x( k - 1 ) ) ) */

/*  and returns the value ( k - 1 ) in the function name F06KLF. If no */
/*  such k exists then F06KLF is returned as n. */

/*  If tol is supplied as less than zero then the value epsmch, where */
/*  epsmch is the relative machine precision, is used in place of tol. */


/*  Nag Fortran 77 O( n ) basic linear algebra routine. */

/*  -- Written on 27-February-1986. */
/*     Sven Hammarling, Nag Central Office. */

/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --x;

    /* Function Body */
    k = 0;
    if (*n >= 1) {
	ix = 1;
	if (*tol < 0.) {
	    tl = solmch_1.wmach[2];
	} else {
	    tl = *tol;
	}
	xmax = (d__1 = x[ix], abs(d__1));

/* +       while( k.lt.n )loop */
L10:
	if (k < *n) {
	    if ((d__1 = x[ix], abs(d__1)) <= tl * xmax) {
		goto L20;
	    }
/* Computing MAX */
	    d__2 = xmax, d__3 = (d__1 = x[ix], abs(d__1));
	    xmax = max(d__2,d__3);
	    ++k;
	    ix += *incx;
	    goto L10;
	}
/* +       end while */

    }

L20:
    ret_val = k;
/*     f06klf = k */

/*     end of idrank (f06klf) */
    return ret_val;
} /* idrank_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int f06fqf_(char *pivot, char *direct, integer *n, 
	doublereal *alpha, doublereal *x, integer *incx, doublereal *c__, 
	doublereal *s, ftnlen pivot_len, ftnlen direct_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern /* Subroutine */ int f06baf_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    integer i__, ix;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  F06FQF generates the parameters of an orthogonal matrix P such that */

/*     when   PIVOT = 'F' or 'f'   and   DIRECT = 'F' or 'f' */
/*     or     PIVOT = 'V' or 'v'   and   DIRECT = 'B' or 'b' */

/*        P*( alpha ) = ( beta ), */
/*          (   x   )   (   0  ) */

/*     when   PIVOT = 'F' or 'f'   and   DIRECT = 'B' or 'b' */
/*     or     PIVOT = 'V' or 'v'   and   DIRECT = 'F' or 'f' */

/*        P*(   x   ) = (   0  ), */
/*          ( alpha ) = ( beta ) */

/*  where alpha is a scalar and x is an n element vector. */

/*  When  PIVOT = 'F' or 'f'  ( fixed pivot ) */
/*  and  DIRECT = 'F' or 'f'  ( forward sequence ) then */

/*     P is given as the sequence of plane rotation matrices */

/*        P = P( n )*P( n - 1 )*...*P( 1 ) */

/*     where P( k ) is a plane rotation matrix for the ( 1, k + 1 ) plane */
/*     designed to annihilate the kth element of x. */

/*  When  PIVOT = 'V' or 'v'  ( variable pivot ) */
/*  and  DIRECT = 'B' or 'b'  ( backward sequence ) then */

/*     P is given as the sequence of plane rotation matrices */

/*        P = P( 1 )*P( 2 )*...*P( n ) */

/*     where P( k ) is a plane rotation matrix for the ( k, k + 1 ) plane */
/*     designed to annihilate the kth element of x. */

/*  When  PIVOT = 'F' or 'f'  ( fixed pivot ) */
/*  and  DIRECT = 'B' or 'b'  ( backward sequence ) then */

/*     P is given as the sequence of plane rotation matrices */

/*        P = P( 1 )*P( 2 )*...*P( n ) */

/*     where P( k ) is a plane rotation matrix for the ( k, n + 1 ) plane */
/*     designed to annihilate the kth element of x. */

/*  When  PIVOT = 'V' or 'v'  ( variable pivot ) */
/*  and  DIRECT = 'F' or 'f'  ( forward sequence ) then */

/*     P is given as the sequence of plane rotation matrices */

/*        P = P( n )*P( n - 1 )*...*P( 1 ) */

/*     where P( k ) is a plane rotation matrix for the ( k, k + 1 ) plane */
/*     designed to annihilate the kth element of x. */

/*  The routine returns the cosine, c( k ), and sine, s( k ) that define */
/*  the matrix P( k ), such that the two by two rotation part of P( k ), */
/*  R( k ), has the form */

/*     R( k ) = (  c( k )  s( k ) ). */
/*              ( -s( k )  c( k ) ) */

/*  On entry, ALPHA must contain  the scalar alpha and on exit, ALPHA is */
/*  overwritten by beta. The cosines and sines are returned in the arrays */
/*  C and S and the vector x is overwritten by the tangents of the plane */
/*  rotations ( t( k ) = s( k )/c( k ) ). */



/*  Nag Fortran 77 O( n ) basic linear algebra routine. */

/*  -- Written on 19-April-1985. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Local Scalars .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --s;
    --c__;
    --x;

    /* Function Body */
    if (*n > 0) {
	if (*(unsigned char *)direct == 'B' || *(unsigned char *)direct == 
		'b') {
	    ix = (*n - 1) * *incx + 1;
	    if (*(unsigned char *)pivot == 'V' || *(unsigned char *)pivot == 
		    'v') {
		for (i__ = *n; i__ >= 2; --i__) {
		    f06baf_(&x[ix - *incx], &x[ix], &c__[i__], &s[i__]);
		    ix -= *incx;
/* L10: */
		}
		f06baf_(alpha, &x[ix], &c__[1], &s[1]);
	    } else if (*(unsigned char *)pivot == 'F' || *(unsigned char *)
		    pivot == 'f') {

/*              Here we choose c and s so that */

/*                 ( alpha ) := (  c  s )*( alpha  ) */
/*                 (   0   )    ( -s  c ) ( x( i ) ) */

/*              which is equivalent to */

/*                 (   0   ) := ( c  -s )*( x( i ) ) */
/*                 ( alpha )    ( s   c ) ( alpha  ) */

/*              and so we need to return  s( i ) = -s  in order to make */
/*              R( i ) look like */

/*                 R( i ) = (  c( i )  s( i ) ). */
/*                          ( -s( i )  c( i ) ) */

		for (i__ = *n; i__ >= 1; --i__) {
		    f06baf_(alpha, &x[ix], &c__[i__], &s[i__]);
		    s[i__] = -s[i__];
		    x[ix] = -x[ix];
		    ix -= *incx;
/* L20: */
		}
	    }
	} else if (*(unsigned char *)direct == 'F' || *(unsigned char *)
		direct == 'f') {
	    ix = 1;
	    if (*(unsigned char *)pivot == 'V' || *(unsigned char *)pivot == 
		    'v') {

/*              Here we choose c and s so that */

/*                 ( x( i + 1 ) ) := (  c  s )*( x( i + 1 ) ) */
/*                 (    0       )    ( -s  c ) ( x( i )     ) */

/*              which is equivalent to */

/*                 (    0       ) := ( c  -s )*( x( i )     ) */
/*                 ( x( i + 1 ) )    ( s   c ) ( x( i + 1 ) ) */

/*              and so we need to return  s( i ) = -s  in order to make */
/*              R( i ) look like */

/*                 R( i ) = (  c( i )  s( i ) ). */
/*                          ( -s( i )  c( i ) ) */

		i__1 = *n - 1;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    f06baf_(&x[ix + *incx], &x[ix], &c__[i__], &s[i__]);
		    s[i__] = -s[i__];
		    x[ix] = -x[ix];
		    ix += *incx;
/* L30: */
		}
		f06baf_(alpha, &x[ix], &c__[*n], &s[*n]);
		s[*n] = -s[*n];
		x[ix] = -x[ix];
	    } else if (*(unsigned char *)pivot == 'F' || *(unsigned char *)
		    pivot == 'f') {
		i__1 = *n;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    f06baf_(alpha, &x[ix], &c__[i__], &s[i__]);
		    ix += *incx;
/* L40: */
		}
	    }
	}
    }

    return 0;

/*     End of F06FQF. ( SSROTG ) */

} /* f06fqf_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int dgrfg_(integer *n, doublereal *alpha, doublereal *x, 
	integer *incx, doublereal *tol, doublereal *zeta)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *), sqrt(doublereal);

    /* Local variables */
    doublereal beta;
    extern /* Subroutine */ int f06fjf_(integer *, doublereal *, integer *, 
	    doublereal *, doublereal *), dscal_(integer *, doublereal *, 
	    doublereal *, integer *);
    doublereal scale;
    static doublereal eps;
    doublereal ssq;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     ENTRY      F06FRF( N, ALPHA, X, INCX, TOL, ZETA ) */
/*     Modified by PEG 9/25/88. */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  dgrfg/f06frf generates a generalized Householder reflection such that */

/*     P*( alpha ) = ( beta ),   P'*P = I. */
/*       (   x   )   (   0  ) */

/*  P is given in the form */

/*     P = I - ( zeta )*( zeta  z' ), */
/*             (   z  ) */

/*  where z is an n element vector and zeta is a scalar that satisfies */

/*     1.0 .le. zeta .le. sqrt( 2.0 ). */

/*  zeta is returned in ZETA unless x is such that */

/*     max( abs( x( i ) ) ) .le. max( eps*abs( alpha ), tol ) */

/*  where eps is the relative machine precision and tol is the user */
/*  supplied value TOL, in which case ZETA is returned as 0.0 and P can */
/*  be taken to be the unit matrix. */

/*  beta is overwritten on alpha and z is overwritten on x. */
/*  the routine may be called with  n = 0  and advantage is taken of the */
/*  case where  n = 1. */


/*  Nag Fortran 77 O( n ) basic linear algebra routine. */

/*  -- Written on 30-August-1984. */
/*     Sven Hammarling, Nag Central Office. */
/*     This version dated 28-September-1984. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. External Subroutines .. */
/*     .. Intrinsic Functions .. */
/*     .. Save statement .. */
/*     .. Data statements .. */
    /* Parameter adjustments */
    --x;

    /* Function Body */
/*     .. */
/*     .. Executable Statements .. */
    if (*n < 1) {
	*zeta = 0.;
    } else if (*n == 1 && x[1] == 0.) {
	*zeta = 0.;
    } else {

	if (first) {
	    first = FALSE_;
	    eps = solmch_1.wmach[2];
	}

/*        Treat case where P is a 2 by 2 matrix specially. */

	if (*n == 1) {

/*           Deal with cases where  ALPHA = zero  and */
/*           abs( X( 1 ) ) .le. max( EPS*abs( ALPHA ), TOL )  first. */

	    if (*alpha == 0.) {
		*zeta = 1.;
		*alpha = abs(x[1]);
		x[1] = -d_sign(&c_b9, &x[1]);
	    } else /* if(complicated condition) */ {
/* Computing MAX */
		d__1 = eps * abs(*alpha);
		if (abs(x[1]) <= max(d__1,*tol)) {
		    *zeta = 0.;
		} else {
		    if (abs(*alpha) >= abs(x[1])) {
/* Computing 2nd power */
			d__1 = x[1] / *alpha;
			beta = abs(*alpha) * sqrt(d__1 * d__1 + 1);
		    } else {
/* Computing 2nd power */
			d__1 = *alpha / x[1];
			beta = abs(x[1]) * sqrt(d__1 * d__1 + 1);
		    }
		    *zeta = sqrt((abs(*alpha) + beta) / beta);
		    if (*alpha >= 0.) {
			beta = -beta;
		    }
		    x[1] = -x[1] / (*zeta * beta);
		    *alpha = beta;
		}
	    }
	} else {

/*           Now P is larger than 2 by 2. */

	    ssq = 1.;
	    scale = 0.;
	    f06fjf_(n, &x[1], incx, &scale, &ssq);

/*           Treat cases where  SCALE = zero, */
/*           SCALE .le. max( EPS*abs( ALPHA ), TOL )  and */
/*           ALPHA = zero  specially. */
/*           Note that  SCALE = max( abs( X( i ) ) ). */

/* Computing MAX */
	    d__1 = eps * abs(*alpha);
	    if (scale == 0. || scale <= max(d__1,*tol)) {
		*zeta = 0.;
	    } else if (*alpha == 0.) {
		*zeta = 1.;
		*alpha = scale * sqrt(ssq);
		d__1 = -1 / *alpha;
		dscal_(n, &d__1, &x[1], incx);
	    } else {
		if (scale < abs(*alpha)) {
/* Computing 2nd power */
		    d__1 = scale / *alpha;
		    beta = abs(*alpha) * sqrt(ssq * (d__1 * d__1) + 1);
		} else {
/* Computing 2nd power */
		    d__1 = *alpha / scale;
		    beta = scale * sqrt(ssq + d__1 * d__1);
		}
		*zeta = sqrt((beta + abs(*alpha)) / beta);
		if (*alpha > 0.) {
		    beta = -beta;
		}
		d__1 = -1 / (*zeta * beta);
		dscal_(n, &d__1, &x[1], incx);
		*alpha = beta;
	    }
	}
    }

/*     end of dgrfg (f06frf) */
    return 0;
} /* dgrfg_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int f06qff_(char *matrix, integer *m, integer *n, doublereal 
	*a, integer *lda, doublereal *b, integer *ldb, ftnlen matrix_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2;

    /* Local variables */
    integer i__, j;

/*     MARK 13 RELEASE. NAG COPYRIGHT 1988. */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  F06QFF  copies  the  m by n  matrix  A  into  the  m by n  matrix  B. */

/*  If   MATRIX = 'G' or 'g'   then  A  and  B  are  regarded as  general */
/*                             matrices, */
/*  if   MATRIX = 'U' or 'u'   then  A  and  B  are  regarded  as   upper */
/*                             triangular,  and only  elements  for which */
/*                             i.le.j  are referenced, */
/*  if   MATRIX = 'L' or 'l'   then  A  and  B  are  regarded  as   lower */
/*                             triangular,  and only  elements  for which */
/*                             i.ge.j  are referenced. */


/*  Nag Fortran 77 O( n**2 ) basic linear algebra routine. */

/*  -- Written on 21-November-1986. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Local Scalars .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;

    /* Function Body */
    if (*(unsigned char *)matrix == 'G' || *(unsigned char *)matrix == 'g') {
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *m;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		b[i__ + j * b_dim1] = a[i__ + j * a_dim1];
/* L10: */
	    }
/* L20: */
	}
    } else if (*(unsigned char *)matrix == 'U' || *(unsigned char *)matrix == 
	    'u') {
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = min(*m,j);
	    for (i__ = 1; i__ <= i__2; ++i__) {
		b[i__ + j * b_dim1] = a[i__ + j * a_dim1];
/* L30: */
	    }
/* L40: */
	}
    } else if (*(unsigned char *)matrix == 'L' || *(unsigned char *)matrix == 
	    'l') {
	i__1 = min(*m,*n);
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *m;
	    for (i__ = j; i__ <= i__2; ++i__) {
		b[i__ + j * b_dim1] = a[i__ + j * a_dim1];
/* L50: */
	    }
/* L60: */
	}
    }

    return 0;

/*     End of F06QFF. ( SMCOPY ) */

} /* f06qff_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
doublereal f06qgf_(char *norm, char *matrix, integer *m, integer *n, 
	doublereal *a, integer *lda, ftnlen norm_len, ftnlen matrix_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    doublereal ret_val, d__1, d__2, d__3;

    /* Local variables */
    integer i__, j;
    extern doublereal f06bmf_(doublereal *, doublereal *);
    extern /* Subroutine */ int f06fjf_(integer *, doublereal *, integer *, 
	    doublereal *, doublereal *);
    doublereal scale, value, sum;

/*     MARK 13 RELEASE. NAG COPYRIGHT 1988. */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  Purpose */
/*  ======= */

/*  F06QGF  returns the value of the one norm,  or the Frobenius norm, or */
/*  the element of  largest absolute value of a real matrix A.  A  may be */
/*  rectangular,  or square,  or triangular,  or symmetric. */

/*  Description */
/*  =========== */

/*  F06QGF returns the value */

/*     F06QGF = ( max( abs( a( i, j ) ) ) , NORM = 'M' or 'm' */
/*              ( */
/*              ( norm1( A )  ,             NORM = '1', 'O' or 'o' */
/*              ( */
/*              ( normF( A ) ,              NORM = 'F', 'f', 'E' or 'e' */

/*  where norm1 denotes the one norm of a matrix (maximum column sum) and */
/*  normF denotes the  Frobenius norm of a matrix  (square root of sum of */
/*  squares).  Note that  max( abs( a( i, j ) ) )  is not a  matrix norm. */

/*  The type of matrix for which  F06QGF is returned is determined by the */
/*  parameter MATRIX. */

/*  If   MATRIX = 'G' or 'g'   then  A  is regarded as  a general matrix, */
/*  If   MATRIX = 'U' or 'u'   then  A  is regarded as  upper triangular, */
/*  If   MATRIX = 'L' or 'l'   then  A  is regarded as  lower triangular, */
/*  If   MATRIX = 'S' or 's'   then  A  is regarded as symmetric and only */
/*             or 'H' or 'h'   the  upper triangular part of the array  A */
/*                             is referenced, */
/*  If   MATRIX = 'Y' or 'y'   then  A  is regarded as symmetric and only */
/*             or 'E' or 'e'   the  lower triangular part of the array  A */
/*                             is referenced. */

/*  Parameters */
/*  ========== */

/*  NORM  -  CHARACTER*1. */

/*           On entry,  NORM specifies the value to be returned in F06QGF */
/*           as described above. */

/*           Unchanged on exit. */

/*  MATRIX - CHARACTER*1. */

/*           On entry,  MATRIX  specifies the type of matrix and,  in the */
/*           case of a  symmetric matrix,  the part of the array in which */
/*           the matrix is stored as described above. */

/*           Unchanged on exit. */

/*  M      - INTEGER. */

/*           On entry,  M  specifies the number of rows of the matrix  A. */
/*           M  must be at least  zero and when the  matrix is  symmetric */
/*           then  M must be equal to  N. When  M = 0  then F06QGF is set */
/*           to zero and an immediate return is effected. */

/*           Unchanged on exit. */

/*  N      - INTEGER. */
/*           On entry, N specifies the number of columns of the matrix A. */
/*           N  must be at least zero. When  N = 0  then F06QGF is set to */
/*           zero and an immediate return is effected. */

/*           Unchanged on exit. */

/*  A      - REAL array of DIMENSION ( LDA, n ). */

/*           Before entry,  A  must contain the  m by n  matrix for which */
/*           F06QGF is required. */

/*           If  MATRIX = 'U' or 'u' or 'S' or 's' or 'H' or 'h' then the */
/*           strictly lower triangular part of A is not referenced. */

/*           If  MATRIX = 'L' or 'l' or 'Y' or 'y' or 'E' or 'e' then the */
/*           strictly upper triangular part of A is not referenced. */

/*           Unchanged on exit. */

/*  LDA    - INTEGER. */
/*           On entry, LDA specifies the first dimension of A as declared */
/*           in  the  calling  (sub)  program.  LDA  must be at least  M. */

/*           Unchanged on exit. */

/*  Further comments */
/*  ================ */

/*  If A is part of a matrix B partitioned as */

/*     B = ( B1  B2 ) , */
/*         ( B3  A  ) */

/*  where  B1 is an l by k matrix  ( l.ge.0, k.ge.0 ),  then this routine */
/*  may be called with the parameter  A as  b( l + 1, k + 1 ) and  LDA as */
/*  the first dimension of  B  as declared in the calling  (sub) program. */

/*  This routine  can be inefficient on  paged machines when the one norm */
/*  is required, the matrix is symmetric and N is large. */


/*  Nag Fortran 77 O( n**2 ) basic linear algebra routine. */

/*  -- Written on 13-January-1986. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. External Functions .. */
/*     .. External Subroutines .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (min(*m,*n) == 0) {
	value = 0.;
    } else if (*(unsigned char *)norm == 'M' || *(unsigned char *)norm == 'm')
	     {

/*        Find  max( abs( a( i, j ) ) ). */

	value = 0.;
	if (*(unsigned char *)matrix == 'G' || *(unsigned char *)matrix == 
		'g') {
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		i__2 = *m;
		for (i__ = 1; i__ <= i__2; ++i__) {
/* Computing MAX */
		    d__2 = value, d__3 = (d__1 = a[i__ + j * a_dim1], abs(
			    d__1));
		    value = max(d__2,d__3);
/* L10: */
		}
/* L20: */
	    }
	} else if (*(unsigned char *)matrix == 'U' || *(unsigned char *)
		matrix == 'u' || *(unsigned char *)matrix == 'S' || *(
		unsigned char *)matrix == 's' || *(unsigned char *)matrix == 
		'H' || *(unsigned char *)matrix == 'h') {
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		i__2 = min(*m,j);
		for (i__ = 1; i__ <= i__2; ++i__) {
/* Computing MAX */
		    d__2 = value, d__3 = (d__1 = a[i__ + j * a_dim1], abs(
			    d__1));
		    value = max(d__2,d__3);
/* L30: */
		}
/* L40: */
	    }
	} else if (*(unsigned char *)matrix == 'L' || *(unsigned char *)
		matrix == 'l' || *(unsigned char *)matrix == 'Y' || *(
		unsigned char *)matrix == 'y' || *(unsigned char *)matrix == 
		'E' || *(unsigned char *)matrix == 'e') {
	    i__1 = min(*m,*n);
	    for (j = 1; j <= i__1; ++j) {
		i__2 = *m;
		for (i__ = j; i__ <= i__2; ++i__) {
/* Computing MAX */
		    d__2 = value, d__3 = (d__1 = a[i__ + j * a_dim1], abs(
			    d__1));
		    value = max(d__2,d__3);
/* L50: */
		}
/* L60: */
	    }
	}
    } else if (*(unsigned char *)norm == '1' || *(unsigned char *)norm == 'O' 
	    || *(unsigned char *)norm == 'o') {

/*        Find  norm1( A ). */

	value = 0.;
	if (*(unsigned char *)matrix == 'G' || *(unsigned char *)matrix == 
		'g') {
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		sum = 0.;
		i__2 = *m;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    sum += (d__1 = a[i__ + j * a_dim1], abs(d__1));
/* L70: */
		}
		value = max(value,sum);
/* L80: */
	    }
	} else if (*(unsigned char *)matrix == 'U' || *(unsigned char *)
		matrix == 'u') {
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		sum = 0.;
		i__2 = min(*m,j);
		for (i__ = 1; i__ <= i__2; ++i__) {
		    sum += (d__1 = a[i__ + j * a_dim1], abs(d__1));
/* L90: */
		}
		value = max(value,sum);
/* L100: */
	    }
	} else if (*(unsigned char *)matrix == 'L' || *(unsigned char *)
		matrix == 'l') {
	    i__1 = min(*m,*n);
	    for (j = 1; j <= i__1; ++j) {
		sum = 0.;
		i__2 = *m;
		for (i__ = j; i__ <= i__2; ++i__) {
		    sum += (d__1 = a[i__ + j * a_dim1], abs(d__1));
/* L110: */
		}
		value = max(value,sum);
/* L120: */
	    }
	} else if (*(unsigned char *)matrix == 'S' || *(unsigned char *)
		matrix == 's' || *(unsigned char *)matrix == 'H' || *(
		unsigned char *)matrix == 'h') {
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		sum = 0.;
		i__2 = j;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    sum += (d__1 = a[i__ + j * a_dim1], abs(d__1));
/* L130: */
		}
		i__2 = *n;
		for (i__ = j + 1; i__ <= i__2; ++i__) {
		    sum += (d__1 = a[j + i__ * a_dim1], abs(d__1));
/* L140: */
		}
		value = max(value,sum);
/* L150: */
	    }
	} else if (*(unsigned char *)matrix == 'Y' || *(unsigned char *)
		matrix == 'y' || *(unsigned char *)matrix == 'E' || *(
		unsigned char *)matrix == 'e') {
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		sum = 0.;
		i__2 = j - 1;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    sum += (d__1 = a[j + i__ * a_dim1], abs(d__1));
/* L160: */
		}
		i__2 = *n;
		for (i__ = j; i__ <= i__2; ++i__) {
		    sum += (d__1 = a[i__ + j * a_dim1], abs(d__1));
/* L170: */
		}
		value = max(value,sum);
/* L180: */
	    }
	}
    } else if (*(unsigned char *)norm == 'F' || *(unsigned char *)norm == 'f' 
	    || *(unsigned char *)norm == 'E' || *(unsigned char *)norm == 'e')
	     {

/*        Find  normF( A ). */

	scale = 0.;
	sum = 1.;
	if (*(unsigned char *)matrix == 'G' || *(unsigned char *)matrix == 
		'g') {
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		f06fjf_(m, &a[j * a_dim1 + 1], &c__1, &scale, &sum);
/* L190: */
	    }
	} else if (*(unsigned char *)matrix == 'U' || *(unsigned char *)
		matrix == 'u') {
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		i__2 = min(*m,j);
		f06fjf_(&i__2, &a[j * a_dim1 + 1], &c__1, &scale, &sum);
/* L200: */
	    }
	} else if (*(unsigned char *)matrix == 'L' || *(unsigned char *)
		matrix == 'l') {
	    i__1 = min(*m,*n);
	    for (j = 1; j <= i__1; ++j) {
		i__2 = *m - j + 1;
		f06fjf_(&i__2, &a[j + j * a_dim1], &c__1, &scale, &sum);
/* L210: */
	    }
	} else if (*(unsigned char *)matrix == 'S' || *(unsigned char *)
		matrix == 's' || *(unsigned char *)matrix == 'H' || *(
		unsigned char *)matrix == 'h' || *(unsigned char *)matrix == 
		'Y' || *(unsigned char *)matrix == 'y' || *(unsigned char *)
		matrix == 'E' || *(unsigned char *)matrix == 'e') {
	    if (*(unsigned char *)matrix == 'S' || *(unsigned char *)matrix ==
		     's' || *(unsigned char *)matrix == 'H' || *(unsigned 
		    char *)matrix == 'h') {
		i__1 = *n;
		for (j = 2; j <= i__1; ++j) {
		    i__2 = j - 1;
		    f06fjf_(&i__2, &a[j * a_dim1 + 1], &c__1, &scale, &sum);
/* L220: */
		}
	    } else {
		i__1 = *n - 1;
		for (j = 1; j <= i__1; ++j) {
		    i__2 = *n - j;
		    f06fjf_(&i__2, &a[j + 1 + j * a_dim1], &c__1, &scale, &
			    sum);
/* L230: */
		}
	    }
	    sum *= 2;
	    i__1 = *lda + 1;
	    f06fjf_(n, &a[a_dim1 + 1], &i__1, &scale, &sum);
	}
	value = f06bmf_(&scale, &sum);
    }

    ret_val = value;
    return ret_val;

/*     End of F06QGF. ( SMNRM ) */

} /* f06qgf_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int f06qhf_(char *matrix, integer *m, integer *n, doublereal 
	*const__, doublereal *diag, doublereal *a, integer *lda, ftnlen 
	matrix_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    integer i__, j;

/*     MARK 13 RELEASE. NAG COPYRIGHT 1988. */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  F06QHF forms the m by n matrix A given by */

/*     a( i, j ) = (  diag  i.eq.j, */
/*                 ( */
/*                 ( const  i.ne.j. */

/*  If   MATRIX = 'G' or 'g'   then  A  is regarded  as a general matrix, */
/*  if   MATRIX = 'U' or 'u'   then  A  is regarded  as upper triangular, */
/*                             and only  elements  for which  i.le.j  are */
/*                             referenced, */
/*  if   MATRIX = 'L' or 'l'   then  A  is regarded  as lower triangular, */
/*                             and only  elements  for which  i.ge.j  are */
/*                             referenced. */


/*  Nag Fortran 77 O( n**2 ) basic linear algebra routine. */

/*  -- Written on 21-November-1986. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Local Scalars .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (*(unsigned char *)matrix == 'G' || *(unsigned char *)matrix == 'g') {
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *m;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		a[i__ + j * a_dim1] = *const__;
/* L10: */
	    }
/* L20: */
	}
	if (*const__ != *diag) {
	    i__1 = min(*m,*n);
	    for (i__ = 1; i__ <= i__1; ++i__) {
		a[i__ + i__ * a_dim1] = *diag;
/* L30: */
	    }
	}
    } else if (*(unsigned char *)matrix == 'U' || *(unsigned char *)matrix == 
	    'u') {
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = min(*m,j);
	    for (i__ = 1; i__ <= i__2; ++i__) {
		a[i__ + j * a_dim1] = *const__;
/* L40: */
	    }
/* L50: */
	}
	if (*const__ != *diag) {
	    i__1 = min(*m,*n);
	    for (i__ = 1; i__ <= i__1; ++i__) {
		a[i__ + i__ * a_dim1] = *diag;
/* L60: */
	    }
	}
    } else if (*(unsigned char *)matrix == 'L' || *(unsigned char *)matrix == 
	    'l') {
	i__1 = min(*m,*n);
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *m;
	    for (i__ = j; i__ <= i__2; ++i__) {
		a[i__ + j * a_dim1] = *const__;
/* L70: */
	    }
/* L80: */
	}
	if (*const__ != *diag) {
	    i__1 = min(*m,*n);
	    for (i__ = 1; i__ <= i__1; ++i__) {
		a[i__ + i__ * a_dim1] = *diag;
/* L90: */
	    }
	}
    }

    return 0;

/*     End of F06QHF. ( SMLOAD ) */

} /* f06qhf_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int f06qkf_(char *side, char *trans, integer *n, doublereal *
	perm, integer *k, doublereal *b, integer *ldb, ftnlen side_len, 
	ftnlen trans_len)
{
    /* System generated locals */
    integer b_dim1, b_offset, i__1, i__2;

    /* Local variables */
    logical left;
    doublereal temp;
    logical null;
    integer i__, j, l;
    logical right, trnsp;

/*     MARK 13 RELEASE. NAG COPYRIGHT 1988. */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  Purpose */
/*  ======= */

/*  F06QKF performs one of the transformations */

/*     B := P'*B   or   B := P*B,   where B is an m by k matrix, */

/*  or */

/*     B := B*P'   or   B := B*P,   where B is a k by m matrix, */

/*  P being an m by m permutation matrix of the form */

/*     P = P( 1, index( 1 ) )*P( 2, index( 2 ) )*...*P( n, index( n ) ), */

/*  where  P( i, index( i ) ) is the permutation matrix that interchanges */
/*  items i and index( i ). That is P( i, index( i ) ) is the unit matrix */
/*  with rows and columns  i and  index( i )  interchanged. Of course, if */
/*  index( i ) = i  then  P( i, index( i ) ) = I. */

/*  This  routine is intended  for use in conjunction with  Nag auxiliary */
/*  routines  that  perform  interchange  operations,  such  as  sorting. */

/*  Parameters */
/*  ========== */

/*  SIDE   - CHARACTER*1. */
/*  TRANS */
/*           On entry,  SIDE  ( Left-hand side, or Right-hand side )  and */
/*           TRANS  ( Transpose, or No transpose )  specify the operation */
/*           to be performed as follows. */

/*           SIDE = 'L' or 'l'   and   TRANS = 'T' or 't' */

/*              Perform the operation   B := P'*B. */

/*           SIDE = 'L' or 'l'   and   TRANS = 'N' or 'n' */

/*              Perform the operation   B := P*B. */

/*           SIDE = 'R' or 'r'   and   TRANS = 'T' or 't' */

/*              Perform the operation   B := B*P'. */

/*           SIDE = 'R' or 'r'   and   TRANS = 'N' or 'n' */

/*              Perform the operation   B := B*P. */

/*           Unchanged on exit. */

/*  N      - INTEGER. */

/*           On entry, N must specify the value of n.  N must be at least */
/*           zero.  When  N = 0  then an  immediate  return  is effected. */

/*           Unchanged on exit. */

/*  PERM   - REAL             array of DIMENSION at least ( n ). */

/*           Before  entry,  PERM  must  contain  the  n indices  for the */
/*           permutation matrices. index( i ) must satisfy */

/*              1 .le. index( i ) .le. m. */

/*           It is usual for index( i ) to be at least i, but this is not */
/*           necessary for this routine. It is assumed that the statement */
/*           INDEX = PERM( I )  returns the correct integer in  INDEX, so */
/*           that,  if necessary,  PERM( I )  should contain a real value */
/*           slightly larger than  INDEX. */

/*           Unchanged on exit. */

/*  K      - INTEGER. */

/*           On entry with  SIDE = 'L' or 'l',  K must specify the number */
/*           of columns of B and on entry with  SIDE = 'R' or 'r', K must */
/*           specify the number of rows of  B.  K must be at least  zero. */
/*           When  K = 0  then an immediate return is effected. */

/*           Unchanged on exit. */

/*  B      - REAL  array  of  DIMENSION ( LDB, ncolb ),  where  ncolb = k */
/*           when  SIDE = 'L' or 'l'  and  ncolb = m  when  SIDE = 'R' or */
/*           'r'. */

/*           Before entry  with  SIDE = 'L' or 'l',  the  leading  m by K */
/*           part  of  the  array   B  must  contain  the  matrix  to  be */
/*           transformed  and before  entry with  SIDE = 'R' or 'r',  the */
/*           leading  K by m part of the array  B must contain the matrix */
/*           to  be  transformed.  On exit,   B  is  overwritten  by  the */
/*           transformed matrix. */

/*  LDB    - INTEGER. */

/*           On entry,  LDB  must specify  the  leading dimension  of the */
/*           array  B  as declared  in the  calling  (sub) program.  When */
/*           SIDE = 'L' or 'l'   then  LDB  must  be  at  least  m,  when */
/*           SIDE = 'R' or 'r'   then  LDB  must  be  at  least  k. */
/*           Unchanged on exit. */


/*  Nag Fortran 77 O( n**2 ) basic linear algebra routine. */

/*  -- Written on 11-August-1987. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Local Scalars .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --perm;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;

    /* Function Body */
    if (min(*n,*k) == 0) {
	return 0;
    }
    left = *(unsigned char *)side == 'L' || *(unsigned char *)side == 'l';
    right = *(unsigned char *)side == 'R' || *(unsigned char *)side == 'r';
    null = *(unsigned char *)trans == 'N' || *(unsigned char *)trans == 'n';
    trnsp = *(unsigned char *)trans == 'T' || *(unsigned char *)trans == 't';
    if (left) {
	if (trnsp) {
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		l = (integer) perm[i__];
		if (l != i__) {
		    i__2 = *k;
		    for (j = 1; j <= i__2; ++j) {
			temp = b[i__ + j * b_dim1];
			b[i__ + j * b_dim1] = b[l + j * b_dim1];
			b[l + j * b_dim1] = temp;
/* L10: */
		    }
		}
/* L20: */
	    }
	} else if (null) {
	    for (i__ = *n; i__ >= 1; --i__) {
		l = (integer) perm[i__];
		if (l != i__) {
		    i__1 = *k;
		    for (j = 1; j <= i__1; ++j) {
			temp = b[l + j * b_dim1];
			b[l + j * b_dim1] = b[i__ + j * b_dim1];
			b[i__ + j * b_dim1] = temp;
/* L30: */
		    }
		}
/* L40: */
	    }
	}
    } else if (right) {
	if (trnsp) {
	    for (j = *n; j >= 1; --j) {
		l = (integer) perm[j];
		if (l != j) {
		    i__1 = *k;
		    for (i__ = 1; i__ <= i__1; ++i__) {
			temp = b[i__ + j * b_dim1];
			b[i__ + j * b_dim1] = b[i__ + l * b_dim1];
			b[i__ + l * b_dim1] = temp;
/* L50: */
		    }
		}
/* L60: */
	    }
	} else if (null) {
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		l = (integer) perm[j];
		if (l != j) {
		    i__2 = *k;
		    for (i__ = 1; i__ <= i__2; ++i__) {
			temp = b[i__ + l * b_dim1];
			b[i__ + l * b_dim1] = b[i__ + j * b_dim1];
			b[i__ + j * b_dim1] = temp;
/* L70: */
		    }
		}
/* L80: */
	    }
	}
    }

    return 0;

/*     End of F06QKF. ( SGEAPR ) */

} /* f06qkf_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int f06qnf_(char *side, integer *n, integer *k1, integer *k2,
	 doublereal *s, doublereal *a, integer *lda, ftnlen side_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    doublereal temp;
    integer i__, j;
    doublereal aij;

/*     MARK 13 RELEASE. NAG COPYRIGHT 1988. */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  F06QNF applies a  sequence  of  pairwise interchanges to either  the */
/*  left,  or the right,  of the  n by n  upper triangular matrix  U,  to */
/*  transform U to an  upper Hessenberg matrix. The interchanges are */
/*  applied in planes k1 up to k2. */

/*  The upper Hessenberg matrix, H, is formed as */

/*     H = P*U,    when   SIDE = 'L' or 'l',  (  Left-hand side ) */

/*  where P is a permutation matrix of the form */

/*     P = P( k1 )*P( k1 + 1 )*...*P( k2 - 1 ) */

/*  and is formed as */

/*     H = U*P',   when   SIDE = 'R' or 'r',  ( Right-hand side ) */

/*  where P is a permutation matrix of the form */

/*     P = P( k2 - 1 )*...*P( k1 + 1 )*P( k1 ), */

/*  P( k ) being a pairwise interchange for the  ( k, k + 1 ) plane. */
/*  The  two by two */
/*  interchange part of P( k ), R( k ), is assumed to have the form */

/*     R( k ) = ( 0  1 ). */
/*              ( 1  0 ) */

/*  The matrix  U must be supplied in the n by n leading upper triangular */
/*  part of the array  A, and this is overwritten by the upper triangular */
/*  part of  H. */

/*  The  sub-diagonal elements of  H, h( k + 1, k ),  are returned in the */
/*  elements s( k ),  k = k1, k1 + 1, ..., k2 - 1. */

/*  If n or k1 are less than unity,  or k1 is not less than k2,  or k2 is */
/*  greater than n then an immediate return is effected. */


/*  Nag Fortran 77 O( n**2 ) basic linear algebra routine. */

/*  -- Written on 16-May-1988. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --s;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (min(*n,*k1) < 1 || *k2 <= *k1 || *k2 > *n) {
	return 0;
    }
    if (*(unsigned char *)side == 'L' || *(unsigned char *)side == 'l') {

/*        Apply the permutations to columns n back to k1. */

	i__1 = *k1;
	for (j = *n; j >= i__1; --j) {
	    if (j >= *k2) {
		aij = a[*k2 + j * a_dim1];
	    } else {

/*              Form  the  additional sub-diagonal element  h( j + 1, j ) */
/*              and store it in s( j ). */

		aij = 0.;
		s[j] = a[j + j * a_dim1];
	    }
	    i__2 = *k1;
	    for (i__ = min(*k2,j) - 1; i__ >= i__2; --i__) {
		temp = a[i__ + j * a_dim1];
		a[i__ + 1 + j * a_dim1] = temp;
		aij = aij;
/* L10: */
	    }
	    a[*k1 + j * a_dim1] = aij;
/* L20: */
	}
    } else if (*(unsigned char *)side == 'R' || *(unsigned char *)side == 'r')
	     {

/*        Apply  the  plane interchanges to  columns  k1  up to */
/*        ( k2 - 1 ) and  form   the   additional  sub-diagonal */
/*        elements,   storing  h( j + 1, j ) in s( j ). */

	i__1 = *k2 - 1;
	for (j = *k1; j <= i__1; ++j) {
	    i__2 = j;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		temp = a[i__ + (j + 1) * a_dim1];
		a[i__ + (j + 1) * a_dim1] = a[i__ + j * a_dim1];
		a[i__ + j * a_dim1] = temp;
/* L30: */
	    }
	    s[j] = a[j + 1 + (j + 1) * a_dim1];
	    a[j + 1 + (j + 1) * a_dim1] = 0.;
/* L40: */
	}
    }

    return 0;

/*     End of F06QNF. ( SUTSRH ) */

} /* f06qnf_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int f06qrf_(char *side, integer *n, integer *k1, integer *k2,
	 doublereal *c__, doublereal *s, doublereal *a, integer *lda, ftnlen 
	side_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    doublereal subh, temp;
    extern /* Subroutine */ int f06baf_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    integer i__, j;
    doublereal ctemp, stemp, aij;

/*     MARK 13 RELEASE. NAG COPYRIGHT 1988. */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  F06QRF restores an upper Hessenberg matrix H to upper triangular form */
/*  by  applying a sequence of  plane rotations  from either the left, or */
/*  the right.  The matrix  H  is assumed to have  non-zero  sub-diagonal */
/*  elements  in  positions  h( k + 1, k ),  k = k1, k1 + 1, ..., k2 - 1, */
/*  only  and  h( k + 1, k )  must  be  supplied  in  s( k ). */

/*  H is restored to the upper triangular matrix R either as */

/*     R = P*H,   when   SIDE = 'L' or 'l'  (  Left-hand side ) */

/*  where P is an orthogonal matrix of the form */

/*     P = P( k2 - 1 )*...*P( k1 + 1 )*P( k1 ), */

/*  or as */

/*     R = H*P',  when   SIDE = 'R' or 'r'  ( Right-hand side ) */

/*  where P is an orthogonal matrix of the form */

/*     P = P( k1 )*P( k1 + 1 )*...*P( k2 - 1 ), */

/*  in both cases  P( k )  being a  plane rotation  for the  ( k, k + 1 ) */
/*  plane.  The cosine and sine that define P( k ) are returned in c( k ) */
/*  and  s( k )  respectively.  The two by two  rotation part of  P( k ), */
/*  Q( k ), is of the form */

/*     Q( k ) = (  c( k )  s( k ) ). */
/*              ( -s( k )  c( k ) ) */

/*  The upper triangular part of the matrix  H  must be supplied in the n */
/*  by n  leading upper triangular part of  A, and this is overwritten by */
/*  the upper triangular matrix R. */

/*  If n or k1 are less than unity,  or k1 is not less than k2,  or k2 is */
/*  greater than n then an immediate return is effected. */


/*  Nag Fortran 77 O( n**2 ) basic linear algebra routine. */

/*  -- Written on 13-January-1986. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. External Subroutines .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --c__;
    --s;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (min(*n,*k1) < 1 || *k2 <= *k1 || *k2 > *n) {
	return 0;
    }
    if (*(unsigned char *)side == 'L' || *(unsigned char *)side == 'l') {

/*        Restore   H  to  upper  triangular  form  by  annihilating  the */
/*        sub-diagonal elements of H.  The jth rotation is chosen so that */

/*           ( h( j, j ) ) := (  c  s )*( h( j, j )     ). */
/*           (     0     )    ( -s  c ) ( h( j + 1, j ) ) */

/*        Apply the rotations in columns k1 up to n. */

	i__1 = *n;
	for (j = *k1; j <= i__1; ++j) {
	    aij = a[*k1 + j * a_dim1];
	    i__2 = min(j,*k2) - 1;
	    for (i__ = *k1; i__ <= i__2; ++i__) {
		temp = a[i__ + 1 + j * a_dim1];
		a[i__ + j * a_dim1] = s[i__] * temp + c__[i__] * aij;
		aij = c__[i__] * temp - s[i__] * aij;
/* L10: */
	    }
	    if (j < *k2) {

/*              Set up the rotation. */

		subh = s[j];
		f06baf_(&aij, &subh, &c__[j], &s[j]);
		a[j + j * a_dim1] = aij;
	    } else {
		a[*k2 + j * a_dim1] = aij;
	    }
/* L20: */
	}
    } else if (*(unsigned char *)side == 'R' || *(unsigned char *)side == 'r')
	     {

/*        Restore   H  to  upper  triangular  form  by  annihilating  the */
/*        sub-diagonal elements of H.  The jth rotation is chosen so that */

/*           ( h( j + 1, j + 1 ) ) := (  c  s )*( h( j + 1, j + 1 ) ), */
/*           (         0         )    ( -s  c ) ( h( j + 1, j )     ) */

/*        which can be expressed as */

/*           ( 0  h( j + 1, j + 1 ) ) := */

/*               ( h( j + 1, j )  h( j + 1, j + 1 ) )*(  c  s ). */
/*                                                    ( -s  c ) */

/*        Thus we return  c( j ) = c  and  s( j ) = -s  to make the plane */
/*        rotation matrix look like */

/*           Q( j ) = (  c( j )  s( j ) ). */
/*                    ( -s( j )  c( j ) ) */

	i__1 = *k1;
	for (j = *k2 - 1; j >= i__1; --j) {
	    subh = s[j];
	    f06baf_(&a[j + 1 + (j + 1) * a_dim1], &subh, &ctemp, &stemp);
	    stemp = -stemp;
	    s[j] = stemp;
	    c__[j] = ctemp;
	    if (ctemp != 1. || stemp != 0.) {
		for (i__ = j; i__ >= 1; --i__) {
		    temp = a[i__ + (j + 1) * a_dim1];
		    a[i__ + (j + 1) * a_dim1] = ctemp * temp - stemp * a[i__ 
			    + j * a_dim1];
		    a[i__ + j * a_dim1] = stemp * temp + ctemp * a[i__ + j * 
			    a_dim1];
/* L30: */
		}
	    }
/* L40: */
	}
    }

    return 0;

/*     End of F06QRF. ( SUHQR ) */

} /* f06qrf_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int f06qsf_(char *side, integer *n, integer *k1, integer *k2,
	 doublereal *c__, doublereal *s, doublereal *a, integer *lda, ftnlen 
	side_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    doublereal temp;
    extern /* Subroutine */ int f06baf_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    integer i__, j;
    doublereal ctemp, spike, stemp, aij;

/*     MARK 13 RELEASE. NAG COPYRIGHT 1988. */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  F06QSF restores an upper spiked matrix  H to upper triangular form by */
/*  applying a sequence of plane rotations, in planes  k1 up to k2,  from */
/*  either the left, or the right. */

/*  The matrix  H is assumed to have non-zero elements only in the spiked */
/*  positions, h( k2, k ) for a row spike and h( k + 1, k1 ) for a column */
/*  spike, k = k1, k1 + 1, ..., k2 - 1, and these must be supplied in the */
/*  elements s( k ). */

/*  When  SIDE = 'L' or 'l'  (  Left-hand side ) */

/*     H  is  assumed  to have a  row spike  and is restored to the upper */
/*     triangular matrix  R as */

/*        R = P*H, */

/*     where P is an orthogonal matrix of the form */

/*        P = P( k2 - 1 )*...*P( k1 + 1 )*P( k1 ), */

/*     P( k )  being a  plane rotation  matrix for the  ( k, k2 )  plane. */

/*  When  SIDE = 'R' or 'r'  ( Right-hand side ) */

/*     H  is assumed to have a  column spike and is restored to the upper */
/*     triangular matrix R as */

/*        R = H*P', */

/*     where P is an orthogonal matrix of the form */

/*        P = P( k1 )*P( k1 + 1 )*...*P( k2 - 1 ), */

/*     P( k ) being a plane rotation matrix for the  ( k1, k + 1 ) plane. */

/*  The  two by two  rotation  part of  P( k ),  Q( k ),  is of  the form */

/*     Q( k ) = (  c( k )  s( k ) ) */
/*              ( -s( k )  c( k ) ) */

/*  and  c( k ) and s( k ) are returned in the kth elements of the arrays */
/*  C and S respectively. */

/*  The upper triangular part of the matrix  H must be supplied in the  n */
/*  by n  leading upper triangular part of  A, and this is overwritten by */
/*  the upper triangular matrix R. */

/*  If n or k1 are less than unity,  or k1 is not less than k2,  or k2 is */
/*  greater than n then an immediate return is effected. */


/*  Nag Fortran 77 O( n**2 ) basic linear algebra routine. */

/*  -- Written on 13-January-1986. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. External Subroutines .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --c__;
    --s;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (min(*n,*k1) < 1 || *k2 <= *k1 || *k2 > *n) {
	return 0;
    }
    if (*(unsigned char *)side == 'L' || *(unsigned char *)side == 'l') {

/*        Restore H to upper triangular form by annihilating the elements */
/*        in  the  spike  of  H.  The  jth rotation  is  chosen  so  that */

/*        ( h( j, j ) ) := (  c  s )*( h( j , j ) ). */
/*        (     0     )    ( -s  c ) ( h( k2, j ) ) */

/*        Apply the rotations in columns k1 up to ( k2 - 1 ). */

	i__1 = *k2 - 1;
	for (j = *k1; j <= i__1; ++j) {
	    spike = s[j];
	    i__2 = j - 1;
	    for (i__ = *k1; i__ <= i__2; ++i__) {
		aij = a[i__ + j * a_dim1];
		a[i__ + j * a_dim1] = s[i__] * spike + c__[i__] * aij;
		spike = c__[i__] * spike - s[i__] * aij;
/* L10: */
	    }

/*           Set up the rotation. */

	    f06baf_(&a[j + j * a_dim1], &spike, &c__[j], &s[j]);
/* L20: */
	}

/*        Apply the rotations to columns k2 up to n. */

	i__1 = *n;
	for (j = *k2; j <= i__1; ++j) {
	    temp = a[*k2 + j * a_dim1];
	    i__2 = *k2 - 1;
	    for (i__ = *k1; i__ <= i__2; ++i__) {
		aij = a[i__ + j * a_dim1];
		a[i__ + j * a_dim1] = s[i__] * temp + c__[i__] * aij;
		temp = c__[i__] * temp - s[i__] * aij;
/* L30: */
	    }
	    a[*k2 + j * a_dim1] = temp;
/* L40: */
	}
    } else if (*(unsigned char *)side == 'R' || *(unsigned char *)side == 'r')
	     {

/*        Restore H to upper triangular form by annihilating the spike of */
/*        H. The jth rotation is chosen so that */

/*           ( h( j, j ) ) := (  c  s )*( h( j, j )  ), */
/*           (     0     )    ( -s  c ) ( h( j, k1 ) ) */

/*        which can be expressed as */

/*           ( 0  h( j, j ) ) := ( h( j, k1 )  h( j, j ) )*(  c  s ). */
/*                                                         ( -s  c ) */

/*        Thus we return  c( j ) = c  and  s( j ) = -s  to make the plane */
/*        rotation matrix look like */

/*           Q( j ) = (  c( j )  s( j ) ). */
/*                    ( -s( j )  c( j ) ) */

	i__1 = *k1 + 1;
	for (j = *k2; j >= i__1; --j) {
	    f06baf_(&a[j + j * a_dim1], &s[j - 1], &ctemp, &stemp);
	    stemp = -stemp;
	    s[j - 1] = stemp;
	    c__[j - 1] = ctemp;
	    if (ctemp != 1. || stemp != 0.) {
		i__2 = *k1 + 1;
		for (i__ = j - 1; i__ >= i__2; --i__) {
		    spike = s[i__ - 1];
		    s[i__ - 1] = stemp * a[i__ + j * a_dim1] + ctemp * spike;
		    a[i__ + j * a_dim1] = ctemp * a[i__ + j * a_dim1] - stemp 
			    * spike;
/* L50: */
		}
		for (i__ = *k1; i__ >= 1; --i__) {
		    temp = a[i__ + *k1 * a_dim1];
		    a[i__ + *k1 * a_dim1] = stemp * a[i__ + j * a_dim1] + 
			    ctemp * temp;
		    a[i__ + j * a_dim1] = ctemp * a[i__ + j * a_dim1] - stemp 
			    * temp;
/* L60: */
		}
	    }
/* L70: */
	}
    }

    return 0;

/*     End of F06QSF. ( SUSQR ) */

} /* f06qsf_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int f06qtf_(char *side, integer *n, integer *k1, integer *k2,
	 doublereal *c__, doublereal *s, doublereal *a, integer *lda, ftnlen 
	side_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;

    /* Local variables */
    doublereal fill, temp;
    extern /* Subroutine */ int f06baf_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    integer i__, j;
    doublereal ctemp, stemp;
    integer i1;
    doublereal aij;

/*     MARK 13 RELEASE. NAG COPYRIGHT 1988. */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  F06QTF performs the transformation */

/*     R := P*U*Q'  when  SIDE = 'L' or 'l'  (  Left-hand side ) */

/*     R := Q*U*P'  when  SIDE = 'R' or 'r'  ( Right-hand side ), */

/*  where  U and R  are  n by n  upper  triangular  matrices,   P  is  an */
/*  orthogonal matrix,  consisting of a given sequence of plane rotations */
/*  to be  applied  in  planes  k1 to k2,  and  Q  is  a  unitary  matrix */
/*  consisting of a sequence of plane rotations, applied in planes  k1 to */
/*  k2,  chosen to make  R  upper triangular. */

/*  When  SIDE = 'L' or 'l'  then  P  is  given  as a  sequence of  plane */
/*  rotation matrices */

/*     P = P( k2 - 1 )*...*P( k1 + 1 )*P( k1 ), */

/*  where  P( k ) is a plane rotation matrix for the  ( k, k + 1 ) plane. */
/*  In this case the matrix Q is given as */

/*     Q = Q( k2 - 1 )*...*Q( k1 + 1 )*Q( k1 ), */

/*  where  Q( k ) is a plane rotation matrix for the  ( k, k + 1 ) plane. */

/*  When  SIDE = 'R' or 'r'  then  P  is  given  as a  sequence of  plane */
/*  rotation matrices */

/*     P = P( k1 )*P( k1 + 1 )*...*P( k2 - 1 ), */

/*  where  P( k ) is a plane rotation matrix for the  ( k, k + 1 ) plane. */
/*  In this case the matrix Q is given as */

/*     Q = Q( k1 )*Q( k1 + 1 )*...*Q( k2 - 1 ), */

/*  where  Q( k ) is a plane rotation matrix for the  ( k, k + 1 ) plane. */

/*  The  upper  triangular  matrix  U  must  be  supplied  in the  n by n */
/*  leading upper triangular part of  A,  and this  is overwritten by the */
/*  upper triangular matrix  R.  The cosine  and  sine  that  define  the */
/*  plane rotation matrix  P( k )  must be supplied in  c( k ) and s( k ) */
/*  respectively,  and  the two by two rotation part of  P( k ),  T( k ), */
/*  is assumed to be of the form */

/*     T( k ) = (  c( k )  s( k ) ). */
/*              ( -s( k )  c( k ) ) */

/*  The cosine  and  sine that define  Q( k )  are overwritten on  c( k ) */
/*  and  s( k )  respectively and the two by two rotation part of  Q( k ) */
/*  will have the form of  T( k )  above. */

/*  If  n or k1  are less  than  unity, or  k1  is not  less than  k2, or */
/*  k2  is greater than  n  then an immediate return is effected. */


/*  Nag Fortran 77 O( n**2 ) basic linear algebra routine. */

/*  -- Written on 26-November-1987. */
/*     Sven Hammarling and Mick Pont, Nag Central Office. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. External Subroutines .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --c__;
    --s;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (min(*n,*k1) < 1 || *k2 <= *k1 || *k2 > *n) {
	return 0;
    }
    if (*(unsigned char *)side == 'L' || *(unsigned char *)side == 'l') {

/*        Apply the left-hand transformations,  column by column,  to the */
/*        triangular part of  U,  but not to  anywhere  that would  cause */
/*        fill. */

	i__1 = *n;
	for (j = *k1 + 1; j <= i__1; ++j) {

/*           Apply  P( k1 ) ... P( j - 1 )  to column j. */

	    aij = a[*k1 + j * a_dim1];
/* Computing MIN */
	    i__3 = j - 1, i__4 = *k2 - 1;
	    i__2 = min(i__3,i__4);
	    for (i__ = *k1; i__ <= i__2; ++i__) {
		a[i__ + j * a_dim1] = s[i__] * a[i__ + 1 + j * a_dim1] + c__[
			i__] * aij;
		aij = c__[i__] * a[i__ + 1 + j * a_dim1] - s[i__] * aij;
/* L10: */
	    }
	    a[i__ + j * a_dim1] = aij;
/* L20: */
	}

/*           Now apply each  left-hand tranformation  to form the fill-in */
/*           elements and apply a  right-hand transformation to eliminate */
/*           the fill-in element. */

	i__1 = *k2 - 1;
	for (j = *k1; j <= i__1; ++j) {

/*           Apply  P( j )  to the jth diagonal element  and the  fill-in */
/*           position. */

	    fill = -s[j] * a[j + j * a_dim1];
	    a[j + j * a_dim1] = c__[j] * a[j + j * a_dim1];

/*           Now  set up  the rotation  Q( j )  to eliminate the  fill-in */
/*           element,  and  apply  Q( j )  to  the  jth  and  ( j + 1 )th */
/*           columns. */

	    f06baf_(&a[j + 1 + (j + 1) * a_dim1], &fill, &ctemp, &stemp);
	    c__[j] = ctemp;
	    s[j] = -stemp;
	    if (ctemp != 1. || stemp != 0.) {
		stemp = -stemp;
		i__2 = j;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    temp = a[i__ + (j + 1) * a_dim1];
		    a[i__ + (j + 1) * a_dim1] = ctemp * temp - stemp * a[i__ 
			    + j * a_dim1];
		    a[i__ + j * a_dim1] = stemp * temp + ctemp * a[i__ + j * 
			    a_dim1];
/* L30: */
		}
	    }
/* L40: */
	}
    } else if (*(unsigned char *)side == 'R' || *(unsigned char *)side == 'r')
	     {

/*        We intermingle the  left and right hand transformations so that */
/*        at the kth step we form */

/*           A := Q( k )*A*P( k )'. */

/*        First  apply  the  transformations  in  columns  k2 back to k1. */

	i__1 = *k1;
	for (j = *k2 - 1; j >= i__1; --j) {

/*           First apply  P( j ). */

	    if (c__[j] != 1. || s[j] != 0.) {
		ctemp = c__[j];
		stemp = s[j];
		i__2 = j;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    temp = a[i__ + (j + 1) * a_dim1];
		    a[i__ + (j + 1) * a_dim1] = ctemp * temp - stemp * a[i__ 
			    + j * a_dim1];
		    a[i__ + j * a_dim1] = stemp * temp + ctemp * a[i__ + j * 
			    a_dim1];
/* L50: */
		}

/*              Next form the fill-in element  a( j + 1, j )  by applying */
/*              P( j ). */

		fill = s[j] * a[j + 1 + (j + 1) * a_dim1];
		a[j + 1 + (j + 1) * a_dim1] = c__[j] * a[j + 1 + (j + 1) * 
			a_dim1];

/*              Now set up the rotation  Q( j )  to eliminate the fill-in */
/*              element. */

		f06baf_(&a[j + j * a_dim1], &fill, &c__[j], &s[j]);
	    }
/* L60: */
	}

/*        Finally  apply  Q( k2 - 1 ) ... Q( k1 )  to columns  n  back to */
/*        ( k1 + 1 ). */

	i__1 = *k1 + 1;
	for (j = *n; j >= i__1; --j) {
	    i1 = min(*k2,j);
	    aij = a[i1 + j * a_dim1];
	    i__2 = *k1;
	    for (i__ = i1 - 1; i__ >= i__2; --i__) {
		temp = a[i__ + j * a_dim1];
		a[i__ + 1 + j * a_dim1] = c__[i__] * aij - s[i__] * temp;
		aij = s[i__] * aij + c__[i__] * temp;
/* L70: */
	    }
	    a[*k1 + j * a_dim1] = aij;
/* L80: */
	}
    }
    return 0;

/*     End of F06QTF. ( SUTSQR ) */

} /* f06qtf_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int f06qvf_(char *side, integer *n, integer *k1, integer *k2,
	 doublereal *c__, doublereal *s, doublereal *a, integer *lda, ftnlen 
	side_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    doublereal temp;
    integer i__, j;
    doublereal ctemp, stemp, aij;

/*     MARK 13 RELEASE. NAG COPYRIGHT 1988. */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  F06QVF applies a  given sequence  of  plane rotations  to either  the */
/*  left,  or the right,  of the  n by n  upper triangular matrix  U,  to */
/*  transform U to an  upper Hessenberg matrix. The rotations are applied */
/*  in planes k1 up to k2. */

/*  The upper Hessenberg matrix, H, is formed as */

/*     H = P*U,    when   SIDE = 'L' or 'l',  (  Left-hand side ) */

/*  where P is an orthogonal matrix of the form */

/*     P = P( k1 )*P( k1 + 1 )*...*P( k2 - 1 ) */

/*  and is formed as */

/*     H = U*P',   when   SIDE = 'R' or 'r',  ( Right-hand side ) */

/*  where P is an orthogonal matrix of the form */

/*     P = P( k2 - 1 )*...*P( k1 + 1 )*P( k1 ), */

/*  P( k ) being a plane rotation matrix for the  ( k, k + 1 ) plane. The */
/*  cosine and sine that define P( k ), k = k1, k1 + 1, ..., k2 - 1, must */
/*  be  supplied  in  c( k )  and  s( k )  respectively.  The  two by two */
/*  rotation part of P( k ), R( k ), is assumed to have the form */

/*     R( k ) = (  c( k )  s( k ) ). */
/*              ( -s( k )  c( k ) ) */

/*  The matrix  U must be supplied in the n by n leading upper triangular */
/*  part of the array  A, and this is overwritten by the upper triangular */
/*  part of  H. */

/*  The  sub-diagonal elements of  H, h( k + 1, k ),  are returned in the */
/*  elements s( k ),  k = k1, k1 + 1, ..., k2 - 1. */

/*  If n or k1 are less than unity,  or k1 is not less than k2,  or k2 is */
/*  greater than n then an immediate return is effected. */


/*  Nag Fortran 77 O( n**2 ) basic linear algebra routine. */

/*  -- Written on 13-January-1986. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --c__;
    --s;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (min(*n,*k1) < 1 || *k2 <= *k1 || *k2 > *n) {
	return 0;
    }
    if (*(unsigned char *)side == 'L' || *(unsigned char *)side == 'l') {

/*        Apply the plane rotations to columns n back to k1. */

	i__1 = *k1;
	for (j = *n; j >= i__1; --j) {
	    if (j >= *k2) {
		aij = a[*k2 + j * a_dim1];
	    } else {

/*              Form  the  additional sub-diagonal element  h( j + 1, j ) */
/*              and store it in s( j ). */

		aij = c__[j] * a[j + j * a_dim1];
		s[j] = -s[j] * a[j + j * a_dim1];
	    }
	    i__2 = *k1;
	    for (i__ = min(*k2,j) - 1; i__ >= i__2; --i__) {
		temp = a[i__ + j * a_dim1];
		a[i__ + 1 + j * a_dim1] = c__[i__] * aij - s[i__] * temp;
		aij = s[i__] * aij + c__[i__] * temp;
/* L10: */
	    }
	    a[*k1 + j * a_dim1] = aij;
/* L20: */
	}
    } else if (*(unsigned char *)side == 'R' || *(unsigned char *)side == 'r')
	     {

/*        Apply  the  plane rotations  to  columns  k1  up to  ( k2 - 1 ) */
/*        and  form   the   additional  sub-diagonal  elements,   storing */
/*        h( j + 1, j ) in s( j ). */

	i__1 = *k2 - 1;
	for (j = *k1; j <= i__1; ++j) {
	    if (c__[j] != 1. || s[j] != 0.) {
		stemp = s[j];
		ctemp = c__[j];
		i__2 = j;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    temp = a[i__ + (j + 1) * a_dim1];
		    a[i__ + (j + 1) * a_dim1] = ctemp * temp - stemp * a[i__ 
			    + j * a_dim1];
		    a[i__ + j * a_dim1] = stemp * temp + ctemp * a[i__ + j * 
			    a_dim1];
/* L30: */
		}
		s[j] = stemp * a[j + 1 + (j + 1) * a_dim1];
		a[j + 1 + (j + 1) * a_dim1] = ctemp * a[j + 1 + (j + 1) * 
			a_dim1];
	    }
/* L40: */
	}
    }

    return 0;

/*     End of F06QVF. ( SUTSRH ) */

} /* f06qvf_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int f06qwf_(char *side, integer *n, integer *k1, integer *k2,
	 doublereal *c__, doublereal *s, doublereal *a, integer *lda, ftnlen 
	side_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    doublereal temp;
    integer i__, j;
    doublereal ctemp, spike, stemp, aij;

/*     MARK 13 RELEASE. NAG COPYRIGHT 1988. */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  F06QWF applies a  given sequence  of  plane rotations  to either  the */
/*  left,  or the right,  of the  n by n  upper triangular  matrix  U  to */
/*  transform  U  to an upper spiked matrix. The rotations are applied in */
/*  planes k1 up to k2. */

/*  The upper spiked matrix, H, is formed as */

/*     H = P*U,   when   SIDE = 'L' or 'l',  ( Left-hand side ) */

/*  where P is an orthogonal matrix of the form */

/*     P = P( k1 )*P( k1 + 1 )*...*P( k2 - 1 ), */

/*  P( k ) being a plane rotation matrix for the ( k, k2 ) plane, and is */
/*  formed as */

/*     H = U*P',   when   SIDE = 'R' or 'r',  ( Right-hand side ) */

/*  where P is an orthogonal matrix of the form */

/*     P = P( k2 - 1 )*...*P( k1 + 1 )*P( k1 ), */

/*  P( k )  being a  plane rotation matrix for the  ( k1, k + 1 )  plane. */

/*  The cosine and sine that define  P( k ), k = k1, k1 + 1, ..., k2 - 1, */
/*  must be  supplied  in  c( k ) and s( k ) respectively. The two by two */
/*  rotation part of P( k ), R( k ), is assumed to have the form */

/*     R( k ) = (  c( k )  s( k ) ). */
/*              ( -s( k )  c( k ) ) */

/*  The matrix  U must be supplied in the n by n leading upper triangular */
/*  part of the array  A, and this is overwritten by the upper triangular */
/*  part of H. */

/*  When  SIDE = 'L' or 'l'  then a  row spike  is  generated  in  H  and */
/*  when  SIDE = 'R' or 'r'  then a  column spike is generated. For a row */
/*  spike the elements  h( k2, k )  and for a  column spike  the elements */
/*  h( k + 1, k1 ), k = k1, k1 + 1, ..., k2 - 1, are returned in  s( k ). */

/*  If n or k1 are less than unity,  or k1 is not less than k2,  or k2 is */
/*  greater than n then an immediate return is effected. */


/*  Nag Fortran 77 O( n**2 ) basic linear algebra routine. */

/*  -- Written on 13-January-1986. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --c__;
    --s;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (min(*n,*k1) < 1 || *k2 <= *k1 || *k2 > *n) {
	return 0;
    }
    if (*(unsigned char *)side == 'L' || *(unsigned char *)side == 'l') {

/*        Apply the plane rotations to columns n back to k2. */

	i__1 = *k2;
	for (j = *n; j >= i__1; --j) {
	    temp = a[*k2 + j * a_dim1];
	    i__2 = *k1;
	    for (i__ = *k2 - 1; i__ >= i__2; --i__) {
		aij = a[i__ + j * a_dim1];
		a[i__ + j * a_dim1] = s[i__] * temp + c__[i__] * aij;
		temp = c__[i__] * temp - s[i__] * aij;
/* L10: */
	    }
	    a[*k2 + j * a_dim1] = temp;
/* L20: */
	}

/*        Form  the spike  and apply the rotations in columns  ( k2 - 1 ) */
/*        back to k1. */

	i__1 = *k1;
	for (j = *k2 - 1; j >= i__1; --j) {
	    spike = -s[j] * a[j + j * a_dim1];
	    a[j + j * a_dim1] = c__[j] * a[j + j * a_dim1];
	    i__2 = *k1;
	    for (i__ = j - 1; i__ >= i__2; --i__) {
		aij = a[i__ + j * a_dim1];
		a[i__ + j * a_dim1] = s[i__] * spike + c__[i__] * aij;
		spike = c__[i__] * spike - s[i__] * aij;
/* L30: */
	    }
	    s[j] = spike;
/* L40: */
	}
    } else if (*(unsigned char *)side == 'R' || *(unsigned char *)side == 'r')
	     {

/*        Apply the  plane rotations to columns  ( k1 + 1 ) up to k2  and */
/*        form the spike. */

	i__1 = *k2;
	for (j = *k1 + 1; j <= i__1; ++j) {
	    ctemp = c__[j - 1];
	    stemp = s[j - 1];
	    if (ctemp != 1. || stemp != 0.) {
		i__2 = *k1;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    temp = a[i__ + *k1 * a_dim1];
		    a[i__ + *k1 * a_dim1] = stemp * a[i__ + j * a_dim1] + 
			    ctemp * temp;
		    a[i__ + j * a_dim1] = ctemp * a[i__ + j * a_dim1] - stemp 
			    * temp;
/* L50: */
		}
		i__2 = j - 1;
		for (i__ = *k1 + 1; i__ <= i__2; ++i__) {
		    spike = s[i__ - 1];
		    s[i__ - 1] = stemp * a[i__ + j * a_dim1] + ctemp * spike;
		    a[i__ + j * a_dim1] = ctemp * a[i__ + j * a_dim1] - stemp 
			    * spike;
/* L60: */
		}
		s[j - 1] = stemp * a[j + j * a_dim1];
		a[j + j * a_dim1] = ctemp * a[j + j * a_dim1];
	    }
/* L70: */
	}
    }

    return 0;

/*     End of F06QWF. ( SUTSRS ) */

} /* f06qwf_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int f06qxf_(char *side, char *pivot, char *direct, integer *
	m, integer *n, integer *k1, integer *k2, doublereal *c__, doublereal *
	s, doublereal *a, integer *lda, ftnlen side_len, ftnlen pivot_len, 
	ftnlen direct_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    logical left;
    doublereal temp;
    integer i__, j;
    doublereal ctemp;
    logical right;
    doublereal stemp, aij;

/*     MARK 13 RELEASE. NAG COPYRIGHT 1988. */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  F06QXF  performs the transformation */

/*     A := P*A,   when   SIDE = 'L' or 'l'  (  Left-hand side ) */

/*     A := A*P',  when   SIDE = 'R' or 'r'  ( Right-hand side ) */

/*  where A is an m by n matrix and P is an orthogonal matrix, consisting */
/*  of a  sequence  of  plane  rotations,  applied  in  planes  k1 to k2, */
/*  determined by the parameters PIVOT and DIRECT as follows: */

/*     When  PIVOT  = 'V' or 'v'  ( Variable pivot ) */
/*     and   DIRECT = 'F' or 'f'  ( Forward sequence ) then */

/*        P is given as a sequence of plane rotation matrices */

/*           P = P( k2 - 1 )*...*P( k1 + 1 )*P( k1 ), */

/*        where  P( k )  is a plane rotation matrix for the  ( k, k + 1 ) */
/*        plane. */

/*     When  PIVOT  = 'V' or 'v'  ( Variable pivot ) */
/*     and   DIRECT = 'B' or 'b'  ( Backward sequence ) then */

/*        P is given as a sequence of plane rotation matrices */

/*           P = P( k1 )*P( k1 + 1 )*...*P( k2 - 1 ), */

/*        where  P( k )  is a plane rotation matrix for the  ( k, k + 1 ) */
/*        plane. */

/*     When  PIVOT  = 'T' or 't'  ( Top pivot ) */
/*     and   DIRECT = 'F' or 'f'  ( Forward sequence ) then */

/*        P is given as a sequence of plane rotation matrices */

/*           P = P( k2 - 1 )*P( k2 - 2 )*...*P( k1 ), */

/*        where  P( k )  is a plane rotation matrix for the ( k1, k + 1 ) */
/*        plane. */

/*     When  PIVOT  = 'T' or 't'  ( Top pivot ) */
/*     and   DIRECT = 'B' or 'b'  ( Backward sequence ) then */

/*        P is given as a sequence of plane rotation matrices */

/*           P = P( k1 )*P( k1 + 1 )*...*P( k2 - 1 ), */

/*        where  P( k )  is a plane rotation matrix for the ( k1, k + 1 ) */
/*        plane. */

/*     When  PIVOT  = 'B' or 'b'  ( Bottom pivot ) */
/*     and   DIRECT = 'F' or 'f'  ( Forward sequence ) then */

/*        P is given as a sequence of plane rotation matrices */

/*           P = P( k2 - 1 )*P( k2 - 2 )*...*P( k1 ), */

/*        where  P( k )  is a  plane rotation  matrix  for the  ( k, k2 ) */
/*        plane. */

/*     When  PIVOT  = 'B' or 'b'  ( Bottom pivot ) */
/*     and   DIRECT = 'B' or 'b'  ( Backward sequence ) then */

/*        P is given as a sequence of plane rotation matrices */

/*           P = P( k1 )*P( k1 + 1 )*...*P( k2 - 1 ), */

/*        where  P( k )  is a  plane rotation  matrix  for the  ( k, k2 ) */
/*        plane. */

/*  c( k ) and s( k )  must contain the  cosine and sine  that define the */
/*  matrix  P( k ).  The  two by two  plane rotation  part of the  matrix */
/*  P( k ), R( k ), is assumed to be of the form */

/*     R( k ) = (  c( k )  s( k ) ). */
/*              ( -s( k )  c( k ) ) */

/*  If m, n or k1 are less than unity,  or k2 is not greater than k1,  or */
/*  SIDE = 'L' or 'l'  and  k2  is greater than  m, or  SIDE = 'R' or 'r' */
/*  and  k2  is greater than  n,  then an  immediate return  is effected. */


/*  Nag Fortran 77 O( n**2 ) basic linear algebra routine. */

/*  -- Written on 20-November-1986. */
/*     Sven Hammarling and Mick Pont, Nag Central Office. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --c__;
    --s;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    left = *(unsigned char *)side == 'L' || *(unsigned char *)side == 'l';
    right = *(unsigned char *)side == 'R' || *(unsigned char *)side == 'r';
/* Computing MIN */
    i__1 = min(*m,*n);
    if (min(i__1,*k1) < 1 || *k2 <= *k1 || left && *k2 > *m || right && *k2 > 
	    *n) {
	return 0;
    }
    if (left) {
	if (*(unsigned char *)pivot == 'V' || *(unsigned char *)pivot == 'v') 
		{
	    if (*(unsigned char *)direct == 'F' || *(unsigned char *)direct ==
		     'f') {
		i__1 = *n;
		for (j = 1; j <= i__1; ++j) {
		    aij = a[*k1 + j * a_dim1];
		    i__2 = *k2 - 1;
		    for (i__ = *k1; i__ <= i__2; ++i__) {
			temp = a[i__ + 1 + j * a_dim1];
			a[i__ + j * a_dim1] = s[i__] * temp + c__[i__] * aij;
			aij = c__[i__] * temp - s[i__] * aij;
/* L10: */
		    }
		    a[*k2 + j * a_dim1] = aij;
/* L20: */
		}
	    } else if (*(unsigned char *)direct == 'B' || *(unsigned char *)
		    direct == 'b') {
		i__1 = *n;
		for (j = 1; j <= i__1; ++j) {
		    aij = a[*k2 + j * a_dim1];
		    i__2 = *k1;
		    for (i__ = *k2 - 1; i__ >= i__2; --i__) {
			temp = a[i__ + j * a_dim1];
			a[i__ + 1 + j * a_dim1] = c__[i__] * aij - s[i__] * 
				temp;
			aij = s[i__] * aij + c__[i__] * temp;
/* L30: */
		    }
		    a[*k1 + j * a_dim1] = aij;
/* L40: */
		}
	    }
	} else if (*(unsigned char *)pivot == 'T' || *(unsigned char *)pivot 
		== 't') {
	    if (*(unsigned char *)direct == 'F' || *(unsigned char *)direct ==
		     'f') {
		i__1 = *n;
		for (j = 1; j <= i__1; ++j) {
		    temp = a[*k1 + j * a_dim1];
		    i__2 = *k2 - 1;
		    for (i__ = *k1; i__ <= i__2; ++i__) {
			aij = a[i__ + 1 + j * a_dim1];
			a[i__ + 1 + j * a_dim1] = c__[i__] * aij - s[i__] * 
				temp;
			temp = s[i__] * aij + c__[i__] * temp;
/* L50: */
		    }
		    a[*k1 + j * a_dim1] = temp;
/* L60: */
		}
	    } else if (*(unsigned char *)direct == 'B' || *(unsigned char *)
		    direct == 'b') {
		i__1 = *n;
		for (j = 1; j <= i__1; ++j) {
		    temp = a[*k1 + j * a_dim1];
		    i__2 = *k1;
		    for (i__ = *k2 - 1; i__ >= i__2; --i__) {
			aij = a[i__ + 1 + j * a_dim1];
			a[i__ + 1 + j * a_dim1] = c__[i__] * aij - s[i__] * 
				temp;
			temp = s[i__] * aij + c__[i__] * temp;
/* L70: */
		    }
		    a[*k1 + j * a_dim1] = temp;
/* L80: */
		}
	    }
	} else if (*(unsigned char *)pivot == 'B' || *(unsigned char *)pivot 
		== 'b') {
	    if (*(unsigned char *)direct == 'F' || *(unsigned char *)direct ==
		     'f') {
		i__1 = *n;
		for (j = 1; j <= i__1; ++j) {
		    temp = a[*k2 + j * a_dim1];
		    i__2 = *k2 - 1;
		    for (i__ = *k1; i__ <= i__2; ++i__) {
			aij = a[i__ + j * a_dim1];
			a[i__ + j * a_dim1] = s[i__] * temp + c__[i__] * aij;
			temp = c__[i__] * temp - s[i__] * aij;
/* L90: */
		    }
		    a[*k2 + j * a_dim1] = temp;
/* L100: */
		}
	    } else if (*(unsigned char *)direct == 'B' || *(unsigned char *)
		    direct == 'b') {
		i__1 = *n;
		for (j = 1; j <= i__1; ++j) {
		    temp = a[*k2 + j * a_dim1];
		    i__2 = *k1;
		    for (i__ = *k2 - 1; i__ >= i__2; --i__) {
			aij = a[i__ + j * a_dim1];
			a[i__ + j * a_dim1] = s[i__] * temp + c__[i__] * aij;
			temp = c__[i__] * temp - s[i__] * aij;
/* L110: */
		    }
		    a[*k2 + j * a_dim1] = temp;
/* L120: */
		}
	    }
	}
    } else if (right) {
	if (*(unsigned char *)pivot == 'V' || *(unsigned char *)pivot == 'v') 
		{
	    if (*(unsigned char *)direct == 'F' || *(unsigned char *)direct ==
		     'f') {
		i__1 = *k2 - 1;
		for (j = *k1; j <= i__1; ++j) {
		    if (c__[j] != 1. || s[j] != 0.) {
			ctemp = c__[j];
			stemp = s[j];
			i__2 = *m;
			for (i__ = 1; i__ <= i__2; ++i__) {
			    temp = a[i__ + (j + 1) * a_dim1];
			    a[i__ + (j + 1) * a_dim1] = ctemp * temp - stemp *
				     a[i__ + j * a_dim1];
			    a[i__ + j * a_dim1] = stemp * temp + ctemp * a[
				    i__ + j * a_dim1];
/* L130: */
			}
		    }
/* L140: */
		}
	    } else if (*(unsigned char *)direct == 'B' || *(unsigned char *)
		    direct == 'b') {
		i__1 = *k1;
		for (j = *k2 - 1; j >= i__1; --j) {
		    if (c__[j] != 1. || s[j] != 0.) {
			ctemp = c__[j];
			stemp = s[j];
			for (i__ = *m; i__ >= 1; --i__) {
			    temp = a[i__ + (j + 1) * a_dim1];
			    a[i__ + (j + 1) * a_dim1] = ctemp * temp - stemp *
				     a[i__ + j * a_dim1];
			    a[i__ + j * a_dim1] = stemp * temp + ctemp * a[
				    i__ + j * a_dim1];
/* L150: */
			}
		    }
/* L160: */
		}
	    }
	} else if (*(unsigned char *)pivot == 'T' || *(unsigned char *)pivot 
		== 't') {
	    if (*(unsigned char *)direct == 'F' || *(unsigned char *)direct ==
		     'f') {
		i__1 = *k2;
		for (j = *k1 + 1; j <= i__1; ++j) {
		    ctemp = c__[j - 1];
		    stemp = s[j - 1];
		    if (ctemp != 1. || stemp != 0.) {
			i__2 = *m;
			for (i__ = 1; i__ <= i__2; ++i__) {
			    temp = a[i__ + j * a_dim1];
			    a[i__ + j * a_dim1] = ctemp * temp - stemp * a[
				    i__ + *k1 * a_dim1];
			    a[i__ + *k1 * a_dim1] = stemp * temp + ctemp * a[
				    i__ + *k1 * a_dim1];
/* L170: */
			}
		    }
/* L180: */
		}
	    } else if (*(unsigned char *)direct == 'B' || *(unsigned char *)
		    direct == 'b') {
		i__1 = *k1 + 1;
		for (j = *k2; j >= i__1; --j) {
		    ctemp = c__[j - 1];
		    stemp = s[j - 1];
		    if (ctemp != 1. || stemp != 0.) {
			for (i__ = *m; i__ >= 1; --i__) {
			    temp = a[i__ + j * a_dim1];
			    a[i__ + j * a_dim1] = ctemp * temp - stemp * a[
				    i__ + *k1 * a_dim1];
			    a[i__ + *k1 * a_dim1] = stemp * temp + ctemp * a[
				    i__ + *k1 * a_dim1];
/* L190: */
			}
		    }
/* L200: */
		}
	    }
	} else if (*(unsigned char *)pivot == 'B' || *(unsigned char *)pivot 
		== 'b') {
	    if (*(unsigned char *)direct == 'F' || *(unsigned char *)direct ==
		     'f') {
		i__1 = *k2 - 1;
		for (j = *k1; j <= i__1; ++j) {
		    if (c__[j] != 1. || s[j] != 0.) {
			ctemp = c__[j];
			stemp = s[j];
			i__2 = *m;
			for (i__ = 1; i__ <= i__2; ++i__) {
			    temp = a[i__ + j * a_dim1];
			    a[i__ + j * a_dim1] = stemp * a[i__ + *k2 * 
				    a_dim1] + ctemp * temp;
			    a[i__ + *k2 * a_dim1] = ctemp * a[i__ + *k2 * 
				    a_dim1] - stemp * temp;
/* L210: */
			}
		    }
/* L220: */
		}
	    } else if (*(unsigned char *)direct == 'B' || *(unsigned char *)
		    direct == 'b') {
		i__1 = *k1;
		for (j = *k2 - 1; j >= i__1; --j) {
		    if (c__[j] != 1. || s[j] != 0.) {
			ctemp = c__[j];
			stemp = s[j];
			for (i__ = *m; i__ >= 1; --i__) {
			    temp = a[i__ + j * a_dim1];
			    a[i__ + j * a_dim1] = stemp * a[i__ + *k2 * 
				    a_dim1] + ctemp * temp;
			    a[i__ + *k2 * a_dim1] = ctemp * a[i__ + *k2 * 
				    a_dim1] - stemp * temp;
/* L230: */
			}
		    }
/* L240: */
		}
	    }
	}
    }

    return 0;

/*     End of F06QXF. ( SGESRC ) */

} /* f06qxf_ */

/* * END OF F06QXFTEXT */
/* Subroutine */ int f06qzf_(char *hess, integer *n, integer *k1, integer *k2,
	 doublereal *c__, doublereal *s, doublereal *a, integer *lda, ftnlen 
	hess_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    doublereal temp, suph;
    extern /* Subroutine */ int f06baf_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    integer i__, j;
    doublereal ctemp, stemp;

/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  F06QZF  either applies a  given sequence  of  plane rotations  to the */
/*  right of the n by n reverse lower triangular matrix T, to transform T */
/*  to a  reverse lower Hessenberg matrix  H, or restores a reverse lower */
/*  Hessenberg matrix H to reverse lower triangular form T, by applying a */
/*  sequence of plane rotations from the right. */

/*  The rotations are applied  in planes k1 up to k2. */

/*  When   HESS = 'C' or 'c',   ( Create ),  then   the   reverse   lower */
/*  Hessenberg matrix, H, is formed as */

/*     H = T*P', */

/*  where P is an orthogonal matrix of the form */

/*     P = P( k2 - 1 )*...*P( k1 + 1 )*P( k1 ), */

/*  P( k ) being a plane rotation matrix for the  ( k, k + 1 ) plane. The */
/*  cosine and sine that define P( k ), k = k1, k1 + 1, ..., k2 - 1, must */
/*  be  supplied  in  c( k )  and  s( k )  respectively.  The  two by two */
/*  rotation part of P( k ), R( k ), is assumed to have the form */

/*     R( k ) = (  c( k )  s( k ) ). */
/*              ( -s( k )  c( k ) ) */

/*  The matrix  T must be supplied in the n by n reverse lower triangular */
/*  part  of the array  A,  and this is overwritten by the  reverse lower */
/*  triangular part of  H. */

/*  The super-diagonal elements of  H, h( n - k, k ), are returned in the */
/*  elements s( k ),  k = k1, k1 + 1, ..., k2 - 1. */

/*  If n or k1 are less than unity,  or k1 is not less than k2,  or k2 is */
/*  greater than n then an immediate return is effected. */

/*  When   HESS = 'R' or 'r',   ( Remove ),  then   the   reverse   lower */
/*  Hessenberg matrix  H  is  assumed  to  have  non-zero  super-diagonal */
/*  elements  in  positions  h( n - k, k ),  k = k1, k1 + 1, ..., k2 - 1, */
/*  only and  h( n - k, k ) must be supplied in  s( k ). H is restored to */
/*  the reverse lower triangular matrix T as */

/*     T = H*P', */

/*  where P is an orthogonal matrix of the form */

/*     P = P( k1 )*P( k1 + 1 )*...*P( k2 - 1 ), */

/*  P( k ) being a plane rotation for the  ( k, k + 1 ) plane. The cosine */
/*  and  sine  that  define  P( k )  are  returned  in  c( k ) and s( k ) */
/*  respectively.  The  two by two  rotation part of  P( k ),  R( k ), is */
/*  of the form */

/*     R( k ) = (  c( k )  s( k ) ). */
/*              ( -s( k )  c( k ) ) */

/*  The reverse lower triangular part of the matrix H must be supplied in */
/*  the  n by n  reverse  lower  triangular  part  of  A,   and  this  is */
/*  overwritten by the reverse triangular matrix T. */

/*  If n or k1 are less than unity,  or k1 is not less than k2,  or k2 is */
/*  greater than n then an immediate return is effected. */

/*  When   n = 7, k1 = 2 and k2 = 5   then  T  and  H  are  of  the  form */

/*     T = ( 0  0  0  0  0  0  X ),   H = ( 0  0  0  0  0  0  X ). */
/*         ( 0  0  0  0  0  X  X )        ( 0  0  0  0  X  X  X ) */
/*         ( 0  0  0  0  X  X  X )        ( 0  0  0  X  X  X  X ) */
/*         ( 0  0  0  X  X  X  X )        ( 0  0  X  X  X  X  X ) */
/*         ( 0  0  X  X  X  X  X )        ( 0  X  X  X  X  X  X ) */
/*         ( 0  X  X  X  X  X  X )        ( 0  X  X  X  X  X  X ) */
/*         ( X  X  X  X  X  X  X )        ( X  X  X  X  X  X  X ) */


/*  This routine  is  principally intended  for use  with the  non-linear */
/*  optimization routines such as E04UCF, in order to help vectorization. */
/*  Nag Fortran 77 O( n**2 ) basic linear algebra routine. */

/*  -- Written on 10-May-1988. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Intrinsic Functions .. */
/*     .. External Subroutines .. */
/*     .. Local Scalars .. */
/*     .. Parameters .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --c__;
    --s;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (min(*n,*k1) < 1 || *k2 <= *k1 || *k2 > *n) {
	return 0;
    }
    if (*(unsigned char *)hess == 'C' || *(unsigned char *)hess == 'c') {

/*        Apply  the  plane rotations  to  columns  k1  up to  ( k2 - 1 ) */
/*        and  form   the  additional  super-diagonal  elements,  storing */
/*        h( n - j, j ) in s( j ). */

	i__1 = *k2 - 1;
	for (j = *k1; j <= i__1; ++j) {
	    if (c__[j] != 1. || s[j] != 0.) {
		stemp = s[j];
		ctemp = c__[j];
		s[j] = stemp * a[*n - j + (j + 1) * a_dim1];
		a[*n - j + (j + 1) * a_dim1] = ctemp * a[*n - j + (j + 1) * 
			a_dim1];
		i__2 = *n;
		for (i__ = *n - j + 1; i__ <= i__2; ++i__) {
		    temp = a[i__ + (j + 1) * a_dim1];
		    a[i__ + (j + 1) * a_dim1] = ctemp * temp - stemp * a[i__ 
			    + j * a_dim1];
		    a[i__ + j * a_dim1] = stemp * temp + ctemp * a[i__ + j * 
			    a_dim1];
/* L10: */
		}
	    }
/* L20: */
	}
    } else if (*(unsigned char *)hess == 'R' || *(unsigned char *)hess == 'r')
	     {

/*        Restore  H to reverse lower triangular form by annihilating the */
/*        super-diagonal elements of  H.  The  jth rotation  is chosen so */
/*        that */

/*          ( h( n - j, n - j ) ) := (  c  s )*( h( n - j, n - j     ) ), */
/*          (         0         )    ( -s  c ) ( h( n - j, n - j - 1 ) ) */

/*        which can be expressed as */

/*           ( 0  h( n - j, n - j ) ) := */

/*               ( h( n - j, n - j - 1 )  h( n - j, n - j ) )*(  c  s ). */
/*                                                            ( -s  c ) */

/*        Thus we return  c( j ) = c  and  s( j ) = -s  to make the plane */
/*        rotation matrix look like */

/*           R( j ) = (  c( j )  s( j ) ). */
/*                    ( -s( j )  c( j ) ) */

	i__1 = *k1;
	for (j = *k2 - 1; j >= i__1; --j) {
	    suph = s[j];
	    f06baf_(&a[*n - j + (j + 1) * a_dim1], &suph, &ctemp, &stemp);
	    stemp = -stemp;
	    s[j] = stemp;
	    c__[j] = ctemp;
	    if (ctemp != 1. || stemp != 0.) {
		i__2 = *n;
		for (i__ = *n - j + 1; i__ <= i__2; ++i__) {
		    temp = a[i__ + (j + 1) * a_dim1];
		    a[i__ + (j + 1) * a_dim1] = ctemp * temp - stemp * a[i__ 
			    + j * a_dim1];
		    a[i__ + j * a_dim1] = stemp * temp + ctemp * a[i__ + j * 
			    a_dim1];
/* L30: */
		}
	    }
/* L40: */
	}
    }

    return 0;

/*     End of F06QZF. */

} /* f06qzf_ */


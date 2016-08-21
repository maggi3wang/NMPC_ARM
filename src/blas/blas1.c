/* ../src/blas1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*     File  blas1.f */
/*     NAG versions of the Level 1  BLAS Vector routines. */

/*     daxpy           dcopy           ddot            dnrm2 */
/*     dscal           dswap           idamax          drot */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int daxpy_(integer *n, doublereal *alpha, doublereal *x, 
	integer *incx, doublereal *y, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer i__, ix, iy;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     Modified by PEG 9/25/88. 8/31/94. */
/*     .. Entry Points .. */
/*     ENTRY      F06ECF( N, ALPHA, X, INCX, Y, INCY ) */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  daxpy/F06ECF performs the operation */

/*     y := alpha*x + y */


/*  Nag Fortran 77 version of the Blas routine DAXPY. */
/*  Nag Fortran 77 O( n ) basic linear algebra routine. */

/*  -- Written on 3-September-1982. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --y;
    --x;

    /* Function Body */
    if (*n > 0) {
	if (*alpha != 0.) {
	    if (*incx == *incy && *incx > 0) {
		i__1 = (*n - 1) * *incx + 1;
		i__2 = *incx;
		for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
		    y[ix] = *alpha * x[ix] + y[ix];
/* L10: */
		}
	    } else {
		if (*incy >= 0) {
		    iy = 1;
		} else {
		    iy = 1 - (*n - 1) * *incy;
		}
		if (*incx > 0) {
		    i__2 = (*n - 1) * *incx + 1;
		    i__1 = *incx;
		    for (ix = 1; i__1 < 0 ? ix >= i__2 : ix <= i__2; ix += 
			    i__1) {
			y[iy] = *alpha * x[ix] + y[iy];
			iy += *incy;
/* L20: */
		    }
		} else {
		    ix = 1 - (*n - 1) * *incx;
		    i__1 = *n;
		    for (i__ = 1; i__ <= i__1; ++i__) {
			y[iy] = *alpha * x[ix] + y[iy];
			ix += *incx;
			iy += *incy;
/* L30: */
		    }
		}
	    }
	}
    }

/*     end of daxpy (f06ecf) */
    return 0;
} /* daxpy_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int dcopy_(integer *n, doublereal *x, integer *incx, 
	doublereal *y, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer i__, ix, iy;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     Modified by PEG 8/31/94. */
/*     .. Entry Points .. */
/*     ENTRY      F06EFF( N, X, INCX, Y, INCY ) */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  dcopy/F06EFF performs the operation */

/*     y := x */


/*  Nag Fortran 77 version of the Blas routine DCOPY. */
/*  Nag Fortran 77 O( n ) basic linear algebra routine. */

/*  -- Written on 26-November-1982. */
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

/*     end of dcopy (f06eff) */
    return 0;
} /* dcopy_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
doublereal ddot_(integer *n, doublereal *x, integer *incx, doublereal *y, 
	integer *incy)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal ret_val;

    /* Local variables */
    integer i__, ix, iy;
    doublereal sum;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     Modified by PEG 8/31/94. */
/*     .. Entry Points .. */
/*     DOUBLE PRECISION          F06EAF */
/*     ENTRY                     F06EAF( N, X, INCX, Y, INCY ) */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  ddot/F06EAF returns the value */

/*     F06EAF = x'y */


/*  Nag Fortran 77 version of the Blas routine DDOT. */
/*  Nag Fortran 77 O( n ) basic linear algebra routine. */

/*  -- Written on 21-September-1982. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --y;
    --x;

    /* Function Body */
    sum = 0.;
    if (*n > 0) {
	if (*incx == *incy && *incx > 0) {
	    i__1 = (*n - 1) * *incx + 1;
	    i__2 = *incx;
	    for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
		sum += x[ix] * y[ix];
/* L10: */
	    }
	} else {
	    if (*incy >= 0) {
		iy = 1;
	    } else {
		iy = 1 - (*n - 1) * *incy;
	    }
	    if (*incx > 0) {
		i__2 = (*n - 1) * *incx + 1;
		i__1 = *incx;
		for (ix = 1; i__1 < 0 ? ix >= i__2 : ix <= i__2; ix += i__1) {
		    sum += x[ix] * y[iy];
		    iy += *incy;
/* L20: */
		}
	    } else {
		ix = 1 - (*n - 1) * *incx;
		i__1 = *n;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    sum += x[ix] * y[iy];
		    ix += *incx;
		    iy += *incy;
/* L30: */
		}
	    }
	}
    }

    ret_val = sum;
/*     F06EAF = SUM */

/*     end of ddot (f06eaf) */
    return ret_val;
} /* ddot_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
doublereal dnrm2_(integer *n, doublereal *x, integer *incx)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    doublereal norm;
    extern doublereal f06bmf_(doublereal *, doublereal *);
    extern /* Subroutine */ int f06fjf_(integer *, doublereal *, integer *, 
	    doublereal *, doublereal *);
    doublereal scale, ssq;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     Modified by PEG 8/31/94. */
/*     .. Entry Points .. */
/*     DOUBLE PRECISION          F06EJF */
/*     ENTRY                     F06EJF( N, X, INCX ) */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  dnrm2/F06EJF returns the euclidean norm of a vector via the function */
/*  name, so that */

/*     dnrm2/F06EJF := sqrt( x'*x ) */


/*  Nag Fortran 77 version of the Blas routine DNRM2. */
/*  Nag Fortran 77 O( n ) basic linear algebra routine. */

/*  -- Written on 25-October-1982. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. External Functions .. */
/*     .. External Subroutines .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n < 1) {
	norm = 0.;
    } else if (*n == 1) {
	norm = abs(x[1]);
    } else {
	scale = 0.;
	ssq = 1.;
	f06fjf_(n, &x[1], incx, &scale, &ssq);
	norm = f06bmf_(&scale, &ssq);
    }

    ret_val = norm;
/*     F06EJF = NORM */

/*     end of dnrm2 (f06ejf) */
    return ret_val;
} /* dnrm2_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int dscal_(integer *n, doublereal *alpha, doublereal *x, 
	integer *incx)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer ix;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     Modified by PEG 8/31/94. */
/*     .. Entry Points .. */
/*     ENTRY      f06EDF( N, ALPHA, X, INCX ) */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  dscal/F06EDF performs the operation */

/*     x := alpha*x */


/*  Nag Fortran 77 version of the Blas routine DSCAL. */
/*  Nag Fortran 77 O( n ) basic linear algebra routine. */

/*  -- Written on 26-November-1982. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n > 0) {
	if (*alpha == 0.) {
	    i__1 = (*n - 1) * *incx + 1;
	    i__2 = *incx;
	    for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
		x[ix] = 0.;
/* L10: */
	    }
	} else if (*alpha == -1.) {
	    i__2 = (*n - 1) * *incx + 1;
	    i__1 = *incx;
	    for (ix = 1; i__1 < 0 ? ix >= i__2 : ix <= i__2; ix += i__1) {
		x[ix] = -x[ix];
/* L20: */
	    }
	} else if (*alpha != 1.) {
	    i__1 = (*n - 1) * *incx + 1;
	    i__2 = *incx;
	    for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
		x[ix] = *alpha * x[ix];
/* L30: */
	    }
	}
    }

/*     end of dscal (f06edf) */
    return 0;
} /* dscal_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int dswap_(integer *n, doublereal *x, integer *incx, 
	doublereal *y, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    doublereal temp;
    integer i__, ix, iy;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     Modified by PEG 8/31/94. */
/*     .. Entry Points .. */
/*     ENTRY      F06EGF( N, X, INCX, Y, INCY ) */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  dswap/F06EGF performs the operations */

/*     temp := x,   x := y,   y := temp. */


/*  Nag Fortran 77 version of the Blas routine DSWAP. */
/*  Nag Fortran 77 O( n ) basic linear algebra routine. */

/*  -- Written on 26-November-1982. */
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
		temp = x[iy];
		x[iy] = y[iy];
		y[iy] = temp;
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
		    temp = x[ix];
		    x[ix] = y[iy];
		    y[iy] = temp;
		    ix += *incx;
/* L20: */
		}
	    } else {
		iy = 1 - (*n - 1) * *incy;
		i__1 = *n;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    temp = x[ix];
		    x[ix] = y[iy];
		    y[iy] = temp;
		    iy += *incy;
		    ix += *incx;
/* L30: */
		}
	    }
	}
    }

/*     end of dswap (f06egf) */
    return 0;
} /* dswap_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int drot_(integer *n, doublereal *x, integer *incx, 
	doublereal *y, integer *incy, doublereal *c__, doublereal *s)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    doublereal temp1;
    integer i__, ix, iy;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     Modified by PEG 8/31/94. */
/*     .. Entry Points .. */
/*     ENTRY      F06EPF( N, X, INCX, Y, INCY, C, S ) */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  drot/F06EPF performs the plane rotation */

/*     ( x  y ) = ( x  y )*( c  -s ). */
/*                         ( s   c ) */


/*  Nag Fortran 77 version of the Blas routine DROT. */
/*  Nag Fortran 77 O( n ) basic linear algebra routine. */

/*  -- Written on 23-January-1984. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --y;
    --x;

    /* Function Body */
    if (*n > 0) {
	if (*s != 0. || *c__ != 1.) {
	    if (*c__ == 0. && *s == 1.) {
		if (*incx == *incy && *incx > 0) {
		    i__1 = (*n - 1) * *incx + 1;
		    i__2 = *incx;
		    for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += 
			    i__2) {
			temp1 = -x[ix];
			x[ix] = y[ix];
			y[ix] = temp1;
/* L10: */
		    }
		} else {
		    if (*incy >= 0) {
			iy = 1;
		    } else {
			iy = 1 - (*n - 1) * *incy;
		    }
		    if (*incx > 0) {
			i__2 = (*n - 1) * *incx + 1;
			i__1 = *incx;
			for (ix = 1; i__1 < 0 ? ix >= i__2 : ix <= i__2; ix +=
				 i__1) {
			    temp1 = -x[ix];
			    x[ix] = y[iy];
			    y[iy] = temp1;
			    iy += *incy;
/* L20: */
			}
		    } else {
			ix = 1 - (*n - 1) * *incx;
			i__1 = *n;
			for (i__ = 1; i__ <= i__1; ++i__) {
			    temp1 = -x[ix];
			    x[ix] = y[iy];
			    y[iy] = temp1;
			    ix += *incx;
			    iy += *incy;
/* L30: */
			}
		    }
		}
	    } else if (*c__ == 0. && *s == -1.) {
		if (*incx == *incy && *incx > 0) {
		    i__1 = (*n - 1) * *incx + 1;
		    i__2 = *incx;
		    for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += 
			    i__2) {
			temp1 = x[ix];
			x[ix] = -y[ix];
			y[ix] = temp1;
/* L40: */
		    }
		} else {
		    if (*incy >= 0) {
			iy = 1;
		    } else {
			iy = 1 - (*n - 1) * *incy;
		    }
		    if (*incx > 0) {
			i__2 = (*n - 1) * *incx + 1;
			i__1 = *incx;
			for (ix = 1; i__1 < 0 ? ix >= i__2 : ix <= i__2; ix +=
				 i__1) {
			    temp1 = x[ix];
			    x[ix] = -y[iy];
			    y[iy] = temp1;
			    iy += *incy;
/* L50: */
			}
		    } else {
			ix = 1 - (*n - 1) * *incx;
			i__1 = *n;
			for (i__ = 1; i__ <= i__1; ++i__) {
			    temp1 = x[ix];
			    x[ix] = -y[iy];
			    y[iy] = temp1;
			    ix += *incx;
			    iy += *incy;
/* L60: */
			}
		    }
		}
	    } else {
		if (*incx == *incy && *incx > 0) {
		    i__1 = (*n - 1) * *incx + 1;
		    i__2 = *incx;
		    for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += 
			    i__2) {
			temp1 = x[ix];
			x[ix] = *s * y[ix] + *c__ * temp1;
			y[ix] = *c__ * y[ix] - *s * temp1;
/* L70: */
		    }
		} else {
		    if (*incy >= 0) {
			iy = 1;
		    } else {
			iy = 1 - (*n - 1) * *incy;
		    }
		    if (*incx > 0) {
			i__2 = (*n - 1) * *incx + 1;
			i__1 = *incx;
			for (ix = 1; i__1 < 0 ? ix >= i__2 : ix <= i__2; ix +=
				 i__1) {
			    temp1 = x[ix];
			    x[ix] = *s * y[iy] + *c__ * temp1;
			    y[iy] = *c__ * y[iy] - *s * temp1;
			    iy += *incy;
/* L80: */
			}
		    } else {
			ix = 1 - (*n - 1) * *incx;
			i__1 = *n;
			for (i__ = 1; i__ <= i__1; ++i__) {
			    temp1 = x[ix];
			    x[ix] = *s * y[iy] + *c__ * temp1;
			    y[iy] = *c__ * y[iy] - *s * temp1;
			    ix += *incx;
			    iy += *incy;
/* L90: */
			}
		    }
		}
	    }
	}
    }

/*     end of drot (f06epf) */
    return 0;
} /* drot_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
integer idamax_(integer *n, doublereal *x, integer *incx)
{
    /* System generated locals */
    integer ret_val, i__1;
    doublereal d__1;

    /* Local variables */
    integer imax;
    doublereal xmax;
    integer i__, ix;

/*     MARK 12 RELEASE. NAG COPYRIGHT 1986. */
/*     Modified by PEG 8/31/94. */
/*     .. Entry Points .. */
/*     INTEGER          F06JLF */
/*     ENTRY            F06JLF( N, X, INCX ) */
/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  F06JLF returns the smallest value of i such that */

/*     abs( x( i ) ) = max( abs( x( j ) ) ) */
/*                      j */


/*  Nag Fortran 77 version of the Blas routine IDAMAX. */
/*  Nag Fortran 77 O( n ) basic linear algebra routine. */

/*  -- Written on 31-May-1983. */
/*     Sven Hammarling, Nag Central Office. */


/*     .. Local Scalars .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n > 0) {
	imax = 1;
	if (*n > 1) {
	    xmax = abs(x[1]);
	    ix = 1;
	    i__1 = *n;
	    for (i__ = 2; i__ <= i__1; ++i__) {
		ix += *incx;
		if (xmax < (d__1 = x[ix], abs(d__1))) {
		    xmax = (d__1 = x[ix], abs(d__1));
		    imax = i__;
		}
/* L10: */
	    }
	}
    } else {
	imax = 0;
    }

    ret_val = imax;
/*     F06JLF = IMAX */

/*     end of idamax (f06jlf) */
    return ret_val;
} /* idamax_ */


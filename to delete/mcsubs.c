/* ../src/mcsubs.f -- translated by f2c (version 19980913).
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

/* Table of constant values */

static integer c__1 = 1;

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*     File  mcsubs.f */

/*     mchpar   mcout    mcenvn   mcenv2   mcstor   mcmin */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int mchpar_(void)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double pow_di(doublereal *, integer *), sqrt(doublereal);

    /* Local variables */
    doublereal base;
    integer emin;
    doublereal rmin;
    integer nbase;
    doublereal small;
    integer ndigit;
    logical hdwire;
    extern /* Subroutine */ int mcenvn_(integer *, integer *, doublereal *, 
	    integer *, doublereal *);
    doublereal big, eps;

/*     ================================================================== */
/*     MCHPAR  must define certain machine parameters as follows: */

/*     wmach(1)  = NBASE  = base of floating-point arithmetic. */
/*     wmach(2)  = NDIGIT = no. of base wmach(1) digits of precision. */
/*     wmach(3)  = EPS    = floating-point precision. */
/*     wmach(4)  = RTEPS  = sqrt(EPS). */
/*     wmach(5)  = RMIN   = smallest positive normalized floating-point */
/*                          number. */
/*     wmach(6)  = RTRMIN = sqrt(RMIN). */
/*     wmach(7)  = BIG    = a very large number that can be represented */
/*                          without overflow. BIG is equal to 1.0/SMALL, */
/*                          where SMALL is a small number for which the */
/*                          machine can evaluate  1.0/SMALL  without */
/*                          causing overflow. */
/*     wmach(8)  = RTBIG  = sqrt(BIG). */
/*     ================================================================== */
    if (first) {
	first = FALSE_;
/*        --------------------------------------------------------------- */
/*        Machine-dependent code. */
/*        1. Set HDWIRE as required. */
/*        2. If  HDWIRE = .TRUE.  set the machine constants */
/*               NBASE, NDIGIT, EPS, RMIN, BIG */
/*           in-line.  Otherwise, they will be computed by MCENVN. */
/*        --------------------------------------------------------------- */
	hdwire = TRUE_;
	if (hdwire) {
/*           IEEE standard floating-point arithmetic. */
/*           (Rounded arithmetic is mandated). */
	    nbase = 2;
	    ndigit = 53;
	    base = (doublereal) nbase;
	    i__1 = -ndigit;
	    eps = pow_di(&base, &i__1);
/* Computing 1021th power */
	    d__1 = 1 / base, d__2 = d__1, d__1 *= d__1, d__1 *= d__1, d__2 *= 
		    d__1, d__1 *= d__1, d__2 *= d__1, d__1 *= d__1, d__2 *= 
		    d__1, d__1 *= d__1, d__2 *= d__1, d__1 *= d__1, d__2 *= 
		    d__1, d__1 *= d__1, d__2 *= d__1, d__1 *= d__1, d__2 *= 
		    d__1;
	    rmin = d__2 * (d__1 * d__1);
/* Computing 1023th power */
	    d__1 = base, d__2 = d__1, d__1 *= d__1, d__2 *= d__1, d__1 *= 
		    d__1, d__2 *= d__1, d__1 *= d__1, d__2 *= d__1, d__1 *= 
		    d__1, d__2 *= d__1, d__1 *= d__1, d__2 *= d__1, d__1 *= 
		    d__1, d__2 *= d__1, d__1 *= d__1, d__2 *= d__1, d__1 *= 
		    d__1, d__2 *= d__1;
	    big = d__2 * (d__1 * d__1);
	} else {
	    mcenvn_(&nbase, &ndigit, &eps, &emin, &rmin);
/* Computing 4th power */
	    i__1 = nbase, i__1 *= i__1;
	    small = rmin * (i__1 * i__1);
	    big = 1. / small;
	}
	solmch_1.wmach[0] = (doublereal) nbase;
	solmch_1.wmach[1] = (doublereal) ndigit;
	solmch_1.wmach[2] = eps;
	solmch_1.wmach[3] = sqrt(eps);
	solmch_1.wmach[4] = rmin;
	solmch_1.wmach[5] = sqrt(rmin);
	solmch_1.wmach[6] = big;
	solmch_1.wmach[7] = sqrt(big);
    }
/*     end of mchpar */
    return 0;
} /* mchpar_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int mcout_(integer *iprint, integer *isumm)
{
/*     ================================================================== */
/*     mcout  defines the unit numbers for the default print files. */

/*     iPrint = unit number of the 132-column format print file. */
/*     iSumm  = unit number for the 80-column format summary file. */

/*     Beware: changing the defaults so that iPrint = iSumm  will cause */
/*             duplicate lines to be printed. */
/*     ================================================================== */
    *iprint = 9;
    *isumm = 6;
/*     end of mcout */
    return 0;
} /* mcout_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int mcenvn_(integer *beta, integer *t, doublereal *eps, 
	integer *emin, doublereal *rmin)
{
    /* Initialized data */

    static logical first = TRUE_;
    static logical iwarn = FALSE_;

    /* Format strings */
    static char fmt_99999[] = "(//\002 WARNING. The value Emin may be incorr"
	    "ect:-  Emin = \002,i8/\002 If, after inspection, the value Emin "
	    "looks\002,\002 acceptable please comment out \002/\002 the IF bl"
	    "ock\002,\002 as marked within the code of routine MCENVN,\002"
	    "/\002 otherwise contact UCSD. \002/)";

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    double pow_di(doublereal *, integer *);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    logical lrnd;
    static doublereal leps;
    doublereal zero, a, b;
    integer i__;
    static integer lbeta;
    doublereal rbase;
    extern /* Subroutine */ int mcmin_(integer *, doublereal *, integer *);
    static integer lemin;
    doublereal small;
    integer gnmin, gpmin;
    static doublereal lrmin;
    extern /* Subroutine */ int mcenv2_(integer *, integer *, logical *);
    static integer lt;
    integer ngnmin, ngpmin;
    extern doublereal mcstor_(doublereal *, doublereal *);
    doublereal one, two;

    /* Fortran I/O blocks */
    static cilist io___30 = { 0, 0, 0, fmt_99999, 0 };
    static cilist io___31 = { 0, 0, 0, fmt_99999, 0 };


/*     ------------------------------------------------------------------ */
/*     Based on NAG Mark 1.0 release ENVIRN. */

/*     MCENVN returns the machine parameters given by: */

/*        BETA - INTEGER. */
/*               The base of the machine. */

/*        T    - INTEGER. */
/*               The number of ( BETA ) digits in the mantissa. */

/*        EPS  - REAL. */
/*               The smallest positive number such that */

/*                  fl( 1.0 - EPS ) .lt. 1.0, */

/*               where fl denotes the computed value. */

/*        EMIN - INTEGER. */
/*               The minimum exponent before (gradual) underflow occurs. */

/*        RMIN - REAL. */
/*               The smallest normalized number for the machine given by */
/*               BASE**( EMIN - 1 ), where BASE is the floating point */
/*               value of BETA. */


/*     The computation of EPS, EMIN and RMIN is based on a routine, */
/*     PARANOIA by W. Kahan of the University of California at Berkeley. */


/*     Nag Fortran 77 O( 1 ) basic linear algebra routine (ENVIRN). */

/*     -- Written on 2-June-1987. */
/*     Sven Hammarling, Mick Pont and Janet Welding, Nag Central Office. */
/*     Modified by PEG, 7-Aug-1990. */
/*     ------------------------------------------------------------------ */
    if (first) {
	first = FALSE_;
	zero = 0.;
	one = 1.;
	two = 2.;
/*        LBETA, LT, LEPS, LEMIN and LRMIN are the local values of BETA, */
/*        T, EPS, EMIN and RMIN. */

/*        Throughout this routine we use the function MCSTOR to ensure */
/*        that relevant values are stored and not held in registers, or */
/*        are not affected by optimizers. */

/*        MCENV2 returns the parameters LBETA and LT. ( LRND is not used */
/*        here. ) */
	mcenv2_(&lbeta, &lt, &lrnd);
/*        Start to find EPS. */
	b = (doublereal) lbeta;
	if (lrnd) {
	    i__1 = 1 - lt;
	    leps = pow_di(&b, &i__1) / two;
	} else {
	    i__1 = 1 - lt;
	    leps = pow_di(&b, &i__1);
	}
/*        Computation of EPS complete. Now find EMIN. */
/*        Let a = + or - 1, and + or - (1 + base**(-3)). */
/*        Keep dividing a by BETA until (gradual) underflow occurs. */
/*        This is detected when we cannot recover the previous a. */
	rbase = one / lbeta;
	small = one;
	for (i__ = 1; i__ <= 3; ++i__) {
	    d__1 = small * rbase;
	    small = mcstor_(&d__1, &zero);
/* L20: */
	}
	a = mcstor_(&one, &small);
	mcmin_(&ngpmin, &one, &lbeta);
	d__1 = -one;
	mcmin_(&ngnmin, &d__1, &lbeta);
	mcmin_(&gpmin, &a, &lbeta);
	d__1 = -a;
	mcmin_(&gnmin, &d__1, &lbeta);
	if (ngpmin == ngnmin && gpmin == gnmin) {
	    if (ngpmin == gpmin) {
		lemin = ngpmin;
/*              ( Non twos-complement machines, no gradual underflow; */
/*              eg VAX ) */
	    } else if (gpmin - ngpmin == 3) {
		lemin = ngpmin - 1 + lt;
/*              ( Non twos-complement machines, with gradual underflow; */
/*              eg IEEE standard followers ) */
	    } else {
		lemin = min(ngpmin,gpmin);
/*              ( A guess; no known machine ) */
		iwarn = TRUE_;
	    }
	} else if (ngpmin == gpmin && ngnmin == gnmin) {
	    if ((i__1 = ngpmin - ngnmin, abs(i__1)) == 1) {
		lemin = max(ngpmin,ngnmin);
/*              ( Twos-complement machines, no gradual underflow; */
/*              eg Cyber 205 ) */
	    } else {
		lemin = min(ngpmin,ngnmin);
/*              ( A guess; no known machine ) */
		iwarn = TRUE_;
	    }
	} else if ((i__1 = ngpmin - ngnmin, abs(i__1)) == 1 && gpmin == gnmin)
		 {
	    if (gpmin - min(ngpmin,ngnmin) == 3) {
		lemin = max(ngpmin,ngnmin) - 1 + lt;
/*              ( Twos-complement machines with gradual underflow; */
/*              no known machine ) */
	    } else {
		lemin = max(ngpmin,ngnmin);
/*              ( A guess; no known machine ) */
		iwarn = TRUE_;
	    }
	} else {
/* Computing MIN */
	    i__1 = min(ngpmin,ngnmin), i__1 = min(i__1,gpmin);
	    lemin = min(i__1,gnmin);
/*           ( A guess; no known machine ) */
	    iwarn = TRUE_;
	}
/*        ** */
/*        Comment out this IF block if Emin is ok */
	if (iwarn) {
	    first = TRUE_;
	    if (sol1cm_1.iprint > 0) {
		io___30.ciunit = sol1cm_1.iprint;
		s_wsfe(&io___30);
		do_fio(&c__1, (char *)&lemin, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	    if (sol1cm_1.isumm > 0) {
		io___31.ciunit = sol1cm_1.isumm;
		s_wsfe(&io___31);
		do_fio(&c__1, (char *)&lemin, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
/*        ** */
/*        Finally compute RMIN by successive division by BETA. */
/*        We could compute RMIN as base**( EMIN - 1 ), but some machines */
/*        underflow during this computation. */
	lrmin = 1.;
	i__1 = 1 - lemin;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    lrmin /= lbeta;
/* L40: */
	}
    }
    *beta = lbeta;
    *t = lt;
    *eps = leps;
    *emin = lemin;
    *rmin = lrmin;
    return 0;
/*     End of mcenvn (envirn). */
} /* mcenvn_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int mcenv2_(integer *beta, integer *t, logical *rnd)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    doublereal sava;
    static logical lrnd;
    doublereal a, b, c__, f;
    static integer lbeta;
    doublereal c1, c2;
    static integer lt;
    extern doublereal mcstor_(doublereal *, doublereal *);
    doublereal one, qtr;

/*     ------------------------------------------------------------------ */
/*     Based on NAG Mark 1.0 release ENVRON. */

/*     MCENV2 returns the machine parameters given by: */

/*        BETA - INTEGER. */
/*               The base of the machine. */

/*        T    - INTEGER. */
/*               The number of ( BETA ) digits in the mantissa. */

/*        RND  - LOGICAL. */
/*               Whether proper rounding ( RND = .TRUE. ) or chopping */
/*               ( RND = .FALSE. ) occurs in addition. This may not be a */
/*               reliable guide to the way in which the machine perfoms */
/*               its arithmetic. */

/*     The routine is based on the routine of the same name by Malcolm */
/*     and incorporates suggestions by Gentleman and Marovich. See */

/*        Malcolm M. A. (1972) Algorithms to reveal properties of */
/*           floating-point arithmetic. Comms. of the ACM, 15, 949-951. */

/*        Gentleman W. M. and Marovich S. B. (1974) More on algorithms */
/*           that reveal properties of floating point arithmetic units. */
/*           Comms. of the ACM, 17, 276-277. */


/*     Nag Fortran 77 O( 1 ) basic linear algebra routine (envron). */

/*     -- Written on 26-November-1984. */
/*     Sven Hammarling and Mick Pont, Nag Central Office. */
/*     Modified by PEG, 7-Aug-1990. */
/*     ------------------------------------------------------------------ */
    if (first) {
	first = FALSE_;
	one = 1.;
/*        LBETA, LT and LRND are the local values of BETA, T and RND. */

/*        Throughout this routine we use the function MCSTOR to ensure */
/*        that relevant values are stored and not held in registers, or */
/*        are not affected by optimizers. */

/*        Compute  a = 2.0**m  with the smallest positive integer m such */
/*        that */

/*           fl( a + 1.0 ) = a. */
	a = 1.;
	c__ = 1.;
/*       +       while( c .eq. one )loop */
L20:
	if (c__ == one) {
	    a *= 2;
	    c__ = mcstor_(&a, &one);
	    d__1 = -a;
	    c__ = mcstor_(&c__, &d__1);
	    goto L20;
	}
/*       +       end while */
/*        Now compute  b = 2.0**m  with the smallest positive integer m */
/*        such that */

/*           fl( a + b ) .gt. a. */
	b = 1.;
	c__ = mcstor_(&a, &b);
/*       +       while( c .eq. a )loop */
L40:
	if (c__ == a) {
	    b *= 2;
	    c__ = mcstor_(&a, &b);
	    goto L40;
	}
/*       +       end while */
/*        Now compute the base. a and b are neighbouring floating point */
/*        numbers in the interval ( beta**t, beta**( t + 1 ) ) and so */
/*        their difference is beta. Adding 0.25 to c is to ensure that it */
/*        is truncated to beta and not ( beta - 1 ). */
	qtr = one / 4;
	d__1 = -a;
	c__ = mcstor_(&c__, &d__1);
	lbeta = (integer) (c__ + qtr);
/*        Now determine whether rounding or chopping occurs, by adding */
/*        a bit less than beta/2 and a bit more than beta/2 to a. */
	b = (doublereal) lbeta;
	d__1 = b / 2;
	d__2 = -b / 100;
	f = mcstor_(&d__1, &d__2);
	c1 = mcstor_(&f, &a);
	d__1 = b / 2;
	d__2 = b / 100;
	f = mcstor_(&d__1, &d__2);
	c2 = mcstor_(&f, &a);
	sava = a;
/*        Now find the mantissa, t. It should be the integer part of */
/*        log to the base beta of a, however it is safer to determine t */
/*        by powering. So we find t as the smallest positive integer */
/*        for which */

/*           fl( beta**t + 1.0 ) = 1.0. */
	lt = 0;
	a = 1.;
	c__ = 1.;
/*        +       while( c .eq. one )loop */
L60:
	if (c__ == one) {
	    ++lt;
	    a *= lbeta;
	    c__ = mcstor_(&a, &one);
	    d__1 = -a;
	    c__ = mcstor_(&c__, &d__1);
	    goto L60;
	}
/*        +       end while */
	lrnd = c1 == sava && c2 != sava;
    }
    *beta = lbeta;
    *t = lt;
    *rnd = lrnd;
    return 0;
/*     End of mcenv2 (envron). */
} /* mcenv2_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
doublereal mcstor_(doublereal *a, doublereal *b)
{
    /* System generated locals */
    doublereal ret_val;

/*     ------------------------------------------------------------------ */
/*     Based on NAG Mark 1.0 release. */

/*     MCSTOR is intended to force A and B to be stored prior to doing the */
/*     addition of A and B. For use in situations where optimizers might */
/*     hold one of these in a register. */


/*     Nag Fortran 77 O( 1 ) basic linear algebra routine (mcstor). */

/*     -- Written on 28-November-1984. */
/*     Sven Hammarling, Nag Central Office. */
/*     ------------------------------------------------------------------ */
    ret_val = *a + *b;
    return ret_val;
/*     end of mcstor. */
} /* mcstor_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int mcmin_(integer *emin, doublereal *start, integer *base)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    doublereal zero, a;
    integer i__;
    doublereal rbase, b1, b2, c1, c2, d1, d2;
    extern doublereal mcstor_(doublereal *, doublereal *);
    doublereal one;

/*     ------------------------------------------------------------------ */
/*     Based on NAG Mark 1.0 release. */

/*     Service routine for ENVIRN (mcenv2). */


/*     Nag Fortran 77 O( 1 ) basic linear algebra routine (getmin). */

/*     -- Written on 2-June-1987. */
/*     Mick Pont, Nag Central Office. */
/*     ------------------------------------------------------------------ */
    a = *start;
    one = 1.;
    rbase = one / *base;
    zero = 0.;
    *emin = 1;
    d__1 = a * rbase;
    b1 = mcstor_(&d__1, &zero);
    c1 = a;
    c2 = a;
    d1 = a;
    d2 = a;
L20:
    if (c1 == a && c2 == a && d1 == a && d2 == a) {
	--(*emin);
	a = b1;
	d__1 = a / *base;
	b1 = mcstor_(&d__1, &zero);
	d__1 = b1 * *base;
	c1 = mcstor_(&d__1, &zero);
	d1 = zero;
	i__1 = *base;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    d1 += b1;
/* L40: */
	}
	d__1 = a * rbase;
	b2 = mcstor_(&d__1, &zero);
	d__1 = b2 / rbase;
	c2 = mcstor_(&d__1, &zero);
	d2 = zero;
	i__1 = *base;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    d2 += b2;
/* L60: */
	}
	goto L20;
    }
    return 0;
/*     End of mcmin (getmin). */
} /* mcmin_ */


/* qpprmswrapper.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__55 = 55;

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*     qpprmswrapper   qpopenappend   qpclose */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int qpprmswrapper_(char *name__, integer *inform__, ftnlen
	name_len)
{
    /* System generated locals */
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer f_open(olist *), f_clos(cllist *);

    /* Local variables */
    integer iostat;
    extern /* Subroutine */ int qpprms_(integer *, integer *);

/*     ================================================================== */
/*     Read options for qpopt from the file named name. inform .eq 0 if */
/*     successful. */

/*     09 Jan 2000: First version of qpprmswrapper */
/*     ================================================================== */
/*     ------------------------------------------------------------------ */
/*     ------------------------------------------------------------------ */
    o__1.oerr = 1;
    o__1.ounit = 55;
    o__1.ofnmlen = name_len;
    o__1.ofnm = name__;
    o__1.orl = 0;
    o__1.osta = "old";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    iostat = f_open(&o__1);
    if (0 != iostat) {
	*inform__ = iostat + 2;
    } else {
	qpprms_(&c__55, inform__);
	cl__1.cerr = 0;
	cl__1.cunit = iostat;
	cl__1.csta = 0;
	f_clos(&cl__1);
    }
/*     end of qpprmswrapper */
    return 0;
} /* qpprmswrapper_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int qpopenappend_(integer *iunit, char *name__, integer *
	inform__, ftnlen name_len)
{
    /* System generated locals */
    olist o__1;

    /* Builtin functions */
    integer f_open(olist *);

/*     ================================================================== */
/*     Open file named name to FORTRAN unit iunit. inform .eq. 0 if */
/*     sucessful. Although opening for appending is not in the FORTRAN 77 */
/*     standard, it is understood by f2c. */

/*     09 Jan 2000: First version of qpopenappend */
/*     ================================================================== */
    o__1.oerr = 1;
    o__1.ounit = *iunit;
    o__1.ofnmlen = name_len;
    o__1.ofnm = name__;
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = "append";
    o__1.ofm = 0;
    o__1.oblnk = 0;
    *inform__ = f_open(&o__1);
/*     end of qpopenappend */
    return 0;
} /* qpopenappend_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int qpclose_(integer *iunit)
{
    /* System generated locals */
    cllist cl__1;

    /* Builtin functions */
    integer f_clos(cllist *);

/*     ================================================================== */
/*     Close unit iunit. */

/*     09 Jan 2000: First version of qpclose */
/*     ================================================================== */
    cl__1.cerr = 0;
    cl__1.cunit = *iunit;
    cl__1.csta = 0;
    f_clos(&cl__1);
/*     end of qpclose */
    return 0;
} /* qpclose_ */


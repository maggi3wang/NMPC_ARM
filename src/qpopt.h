/* Philip Gill  15 Jan 2000 */
/* Function prototypes for functions in the qpopt distribution */

#ifndef QPOPT
#define QPOPT

#pragma once

#ifndef F2C_INCLUDE
#include "f2c.h"
#endif

extern int qpopt_
  ( integer *n, integer *nclin, integer *ldA, integer *ldH,
    doublereal *a, doublereal *bl, doublereal *bu, doublereal *cvec,
    doublereal *h__, U_fp qphess, integer *istate, doublereal *x,
    integer *inform__, integer *iter, doublereal *Obj, doublereal *ax,
    doublereal *clamda, integer *iw, integer *leniw, doublereal *w,
    integer *lenw );

extern int qphess_
  ( integer *n, integer *ldH, integer *jthcol,
    doublereal *h__, doublereal *x, doublereal *hx, integer *iw,
    integer *leniw, doublereal *w, integer *lenw );

extern int qpprmswrapper_
  ( char *name__, integer *inform__, ftnlen name_len );
extern int qpprms_
  ( integer *ioptns, integer *inform__ );
extern int qpprm_
  ( char *string, ftnlen string_len );
extern int qpprmi_
  ( char *string, integer *ivalue, ftnlen string_len);

#endif


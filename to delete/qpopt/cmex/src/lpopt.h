/* Philip Gill  9-Jan-2000 */
/* Function prototypes for functions in the lpopt distribution */

#ifndef LPOPT
#define LPOPT

#pragma once

#ifndef F2C_INCLUDE
#include "f2c.h"
#endif

extern int lpopt_
  ( integer *n, integer *nclin, integer *lda,
    doublereal *a, doublereal *bl, doublereal *bu, doublereal *x,
    doublereal *ax, integer *inform__, integer *iter, integer *istate,
    doublereal *clamda, doublereal *obj, doublereal *cvec, integer *iw,
    integer *leniw, doublereal *w, integer *lenw );

extern int lpprmswrapper_
  ( char *name__, integer *inform__, ftnlen name_len );
extern int lpprms_
  ( integer *ioptns, integer *inform__ );
extern int lpprm_
  ( char *string, ftnlen string_len );
extern int lpprmi_
  ( char *string, integer *ivalue, ftnlen string_len);

#endif


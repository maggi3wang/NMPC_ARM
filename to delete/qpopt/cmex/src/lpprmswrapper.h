/* Philip Gill   09 Jan 2000 */

#ifndef LPPRMSWRAPPER
#define LPPRMSWRAPPER

#pragma once

extern int lpprmswrapper_
  ( char *name, integer *inform, ftnlen name_len );
extern int lpopenappend_
  ( integer *iunit, char *name, integer *inform, ftnlen name_len );
extern int lpclose_
  ( integer *iunit);

#endif

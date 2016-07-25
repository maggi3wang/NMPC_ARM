/* Philip Gill   09 Jan 2000 */

#ifndef QPPRMSWRAPPER
#define QPPRMSWRAPPER

#pragma once

extern int qpprmswrapper_
  ( char *name, integer *inform, ftnlen name_len );
extern int qpopenappend_
  ( integer *iunit, char *name, integer *inform, ftnlen name_len );
extern int qpclose_
  ( integer *iunit);

#endif

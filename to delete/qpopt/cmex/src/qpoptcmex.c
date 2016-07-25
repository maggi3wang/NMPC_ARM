/* ========================================================================= */
/* Mike Gertz and Philip Gill: 15 Jan 2000

   qpoptcmex
        A MATLAB interface to the qpopt optimization routine.

   mexFunction       - the C-MATLAB gateway routine.
   qpSingleStringArg - handles those functions in the qpopt package that
                       take a single string argument.
   qpoptmex          - calls qpopt_
   cqpprm            - a C interface to the  qpprm_  function
   cqpprmi           - a C interface to the  qpprmi_ function

   From MATLAB, a call to qpoptcmex takes the form

       [...] = qpoptcmex( what, ... )

   where the parameter what determines which function in the qpopt package is to
   be called. See the documentation included with the FORTRAN version of qpopt
   for descriptions of these routines.  Valid values of "what" are

   kqpopt              - call qpopt_
   kqpprms             - call qpprms.  Takes the file name as a single
                         string argument.
   kqpprm              - call qpoptn_. Takes the    option as a single
                         string argument.
   kqpSetPrintFile     - declare the location of the qpopt print file.
                         Takes the filename as a single string argument.
   kqpClosePrintFile   - close (and hence flush) the print file
   kqpSetSummaryFile   - set a file in which to print summmary information.
                         Takes a single string argument, the filename.
   kqpCloseSummaryFile - close the summary file.

   The numerical values of these constants are specified in the following enum

       enum { kqpopt = 0, kqpprms, kqpprm,
              kqpSetPrintFile, kqpClosePrintFile,
              kqpSetSummaryFile, kqpCloseSummaryFile};

   The number and meaning of the other input and output arguments to qpoptcmex
   depend on the value of "what". If no additional arguments are needed,
   mexFunction performs the operation. Otherwise, mexFunction passes control to
   a more specialized routine.

   It is unnecessary to call qpoptcmex directly. There are MATLAB m-files which
   will call qpoptcmex with the appropriate values of "what" and the
   appropriate parameters.                                                   */
/* ========================================================================= */
#include "mex.h"
#include "mexUtility.h"

#include "f2c.h"
#include "qpopt.h"
#include "qpprmswrapper.h"

enum    { kqpopt = 0, kqpprms, kqpprm,
	  kqpSetPrintFile, kqpClosePrintFile,
	  kqpSetSummaryFile, kqpCloseSummaryFile };

void     qpoptmex( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] );

void     qpSingleStringArg( int nlhs, mxArray *plhs[], int nrhs,
			    const mxArray *prhs[], int what );

void     cqpprm ( char option[] );
void     cqpprmi( char option[], integer i);

int      printFileIsOpen = 0, summaryFileIsOpen = 0;

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void mexFunction( int nlhs, mxArray *plhs[], int nrhs,
		  const mxArray *prhs[] ) {

  int      what;                    /* input argument 1 */
  static   int firstCall = 1;
  integer  iunit;

  if ( firstCall ) {
    /* Set some reasonable default options for the first time this
       mex file is called                                                      */
    cqpprm( "Nolist" );
    cqpprm( "Print   file = 0" );
    cqpprm( "Summary file = 0" );
    cqpprm( "List" );

    firstCall = 0;
  }
  if ( nrhs < 1 ) {
    mexErrMsgTxt( "Must have at least 1 input argument" );
  }
  /* what :input argument 1 */
  assertScalar( prhs[0], "what" );
  what = *mxGetPr( prhs[0] );

  switch( what ) {
  case kqpopt:
    qpoptmex( nlhs, plhs, nrhs, prhs );
    break;
    /* The next 4 cases require only one string argument for input,
       so we pass them all to the same routine.                    */
  case kqpprms:
  case kqpprm:
  case kqpSetPrintFile:
  case kqpSetSummaryFile:
    qpSingleStringArg( nlhs, plhs, nrhs, prhs, what );
    break;

  case kqpClosePrintFile:
    /* There are no additional input arguments, so we simply close
       the print file.                                             */
    if ( 1 != nrhs )
      mexErrMsgTxt( "Wrong number of input arguments" );
    if ( printFileIsOpen ) { /* Only close if it was open */
      iunit = 9; qpclose_( &iunit ); printFileIsOpen = 0;
    }
    /* Forget the old print file */
    cqpprm( "Nolist" );
    cqpprm( "Print file = 0" );
    cqpprm( "List" );
    break;

  case kqpCloseSummaryFile:
    /* There are no additional input arguments, so we simply close
       the summary file.                                           */
    if ( 1 != nrhs )
      mexErrMsgTxt( "Wrong number of input arguments" );
    if( summaryFileIsOpen ) { /* Only close if it was open */
      iunit = 56; qpclose_( &iunit ); summaryFileIsOpen = 0;
    }
    /* Forget the old summary file */
    cqpprm( "Nolist" );
    cqpprm( "Summary file = 0" );
    cqpprm( "List" );
    break;

  default:
    mexErrMsgTxt( "Unrecognized function name" );
    break;
  }
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void cqpprm
  ( char option[] )
{
  int lenOption;

  lenOption = strlen( option );
  qpprm_( option, lenOption );
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void cqpprmi
  ( char option[], integer i )
{
  int lenOption;

  lenOption = strlen( option );
  qpprmi_( option, &i, lenOption );
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void qpSingleStringArg
  ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], int what )
{
  char     *stringArg; /* input argument 2 */

  double   *dummyDoublePtr;
  integer  lenStringArg, inform, iunit;

  if ( 2 != nrhs ) {
    mexErrMsgTxt( "Wrong number of input arguments" );
  }

  /* Read the string argument */
  assertString( prhs[1], "stringArg" );
  lenStringArg = mxGetN( prhs[1] );
  stringArg    = mxCalloc( (lenStringArg+1), sizeof( char ) );
  mxGetString( prhs[1], stringArg, lenStringArg+1 );

  switch( what ) {
  case kqpprm:
    qpprm_( stringArg, lenStringArg );
    break;

  case kqpprms:
    qpprmswrapper_( stringArg, &inform, lenStringArg );
    break;

  case kqpSetPrintFile:
    iunit = 9;
    /* Close the current print file */
    if ( printFileIsOpen ) qpclose_( &iunit );
    qpopenappend_( &iunit, stringArg, &inform, lenStringArg );
    if ( 0 == inform ) {
      /* We were able to open the file. Set the unit number of the
	 print file to the newly opened file.                       */
      printFileIsOpen = 1;
      cqpprm( "Nolist" );
      cqpprm( "Print file = 9" );
      cqpprm( "List" );
    }
    break;

  case kqpSetSummaryFile:
    iunit = 56;
    /* Close the current summary file */
    if( summaryFileIsOpen ) qpclose_( &iunit );
    qpopenappend_( &iunit, stringArg, &inform, lenStringArg );
    if ( 0 == inform ) {
      /* We were able to open the file. Set the unit number of the
	 summary file to the newly opened file.                     */
      summaryFileIsOpen = 1;
      cqpprm( "Nolist" );
      cqpprm( "Summary file = 56" );
      cqpprm( "List" );
    }
    break;
  }

  mxFree( stringArg );

  switch(what) {
  case kqpprms:
  case kqpSetPrintFile:
  case kqpSetSummaryFile:
    /* These all return 0 if they were sucessful */
    plhs[0]         = mxCreateDoubleMatrix( 1, 1, mxREAL );
    dummyDoublePtr  = mxGetPr( plhs[0] );
    *dummyDoublePtr = inform;
    break;
  }
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void qpoptmex
  ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{

  double   *dummyDoublePtr;

  double   *A;                            /* input argument  2 */
  double   *l;                            /* input argument  3 */
  double   *u;                            /* input argument  4 */
  double   *c;                            /* input argument  5 */
  double   *H;                            /* input argument  6 */
  double   *x0;                           /* input argument  7 */
  integer  *istate0;                      /* input argument  8 */

  double   *x;                            /* output argument 1 */
  mxArray  *mexx;
  double   Obj;                           /* output argument 2 */
  double   *lambda;                       /* output argument 3 */
  mxArray  *mexlambda;
  integer  inform;                        /* output argument 4 */
  integer  iter;                          /* output argument 5 */
  integer  *istate;                       /* output argument 6 */
  mxArray  *mexistate;
  integer  n;                             /* array dimension */
  integer  nCon;                          /* array dimension */
  integer  nclin;                         /* array dimension */

  integer  ldA, ldH, nH;
  integer  leniw, lenw, *iw;
  double   *Ax, *w;

  if ( 7 > nrhs || 8 < nrhs ) {
    mexErrMsgTxt( "Wrong number of input arguments" );
  }

  /* x0 : input argument 7, read this first to get a value for n */
  n     = mxGetM( prhs[6] );
  assertColDim( prhs[6], 1, "x0" );
  x0    = mxGetPr( prhs[6] );

  /* A : input argument 2 */
  nclin = mxGetM( prhs[1] );
  if ( 0 != nclin ) {
    /* A is not empty */
    ldA = nclin;
    assertColDim( prhs[1], n, "A" );
  } else {
    ldA = 1; /* ldA cannot be 0, even if A is empty */
  }
  A = mxGetPr( prhs[1] );

  /* l : input argument 3 */
  nCon = mxGetM( prhs[2] );
  assertColDim( prhs[2], 1, "l" );
  l    = mxGetPr( prhs[2] );

  /* u : input argument 4 */
  assertColDim( prhs[3], 1, "u" );
  assertRowDim( prhs[3], nCon, "u" );
  u    = mxGetPr( prhs[3] );

  if ( nCon - n - nclin < 0 ) {
    mexErrMsgTxt( "'l' and 'u' are too small" );
  }

  /* c : input argument 5 */
  assertColDim( prhs[4], 1, "c" );
  assertRowDim( prhs[4], n, "c" );
  c    = mxGetPr( prhs[4] );

  /* H : input argument 6 */
  nH   = mxGetM( prhs[5] );
  if ( 0 != nH ) {
    /* H is not empty */
    ldH = n;
    assertColDim( prhs[5], n, "H" );
  } else {
    ldH = 1; /* ldH cannot be 0, even if H is empty */
  }
  H = mxGetPr( prhs[5] );

  /* istate0 : input argument 8, copied into output argument istate */

  /* x      : output argument 1 */
  mexx = mxCreateDoubleMatrix( n, 1, mxREAL );
  x    = mxGetPr( mexx );
  memmove( x, x0, n*sizeof( double ) );

  /* Obj    : output argument 2 */
  /* Obj    is scalar           */

  /* lambda : output argument 3 */
  mexlambda = mxCreateDoubleMatrix( nCon, 1, mxREAL );
  lambda = mxGetPr( mexlambda );

  /* inform : output argument 4 */
  /* inform is scalar           */

  /* iter   : output argument 5 */
  /* iter is scalar             */

  /* istate : output argument 6 */
  mexistate = mxCreateDoubleMatrix( nCon, 1, mxREAL );
  istate = (integer *) mxGetPr( mexistate );

  if ( 8 == nrhs ) {
    /* An initial state was given in input argument 8 */
    /* We will do a warm start                        */
    assertRowDim( prhs[7], nCon, "istate0" );
    assertColDim( prhs[7], 1   , "istate0" );
    /* copy istate0 into istate */
    dble2int( nCon, mxGetPr(prhs[7]), istate );
    cqpprm( "Nolist" ); cqpprm( "Warm start" ); cqpprm( "List" );
  } else {
    /* No initial state given */
    cqpprm( "Nolist" ); cqpprm( "Cold start" ); cqpprm( "List" );
  }

  Ax    = mxCalloc( ldA, sizeof(double));

  leniw = 2*n + 3;
  iw    = mxCalloc( leniw, sizeof( integer ));

  lenw  = 5*ldA + 8*n + 2*n*n;
  w     = mxCalloc( lenw , sizeof( double ) );

  qpopt_
    ( &n, &nclin, &ldA, &ldH, A, l, u, c, H, qphess_,
      istate, x, &inform, &iter, &Obj, Ax, lambda,
      iw, &leniw, w, &lenw );

  mxFree( Ax );
  /* x      : output argument 1 */
  plhs[0] = mexx;

  /* Obj    : output argument 2 */
  if ( 1 < nlhs ) {
    plhs[1]         = mxCreateDoubleMatrix(1, 1, mxREAL );
    dummyDoublePtr  = mxGetPr( plhs[1] );
    *dummyDoublePtr = Obj;
  }

  /* lambda : output argument 3 */
  if ( 2 < nlhs ) {
    plhs[2] = mexlambda;
  }

  /* inform : output argument 4 */
  if ( 3 < nlhs ) {
    plhs[3]         = mxCreateDoubleMatrix(1, 1, mxREAL );
    dummyDoublePtr  = mxGetPr( plhs[3] );
    *dummyDoublePtr = (double) inform;
  }

  /* iter   : output argument 5 */
  if ( 4 < nlhs ) {
    plhs[4]         = mxCreateDoubleMatrix(1, 1, mxREAL );
    dummyDoublePtr  = mxGetPr( plhs[4] );
    *dummyDoublePtr = (double) iter;
  }

  /* istate : output argument 6 */
  if ( 5 < nlhs ) {
    plhs[5] = mexistate;
    int2dble( nCon, istate, (double *) istate );
  }
}

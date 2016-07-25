/* ========================================================================= */
/* Mike Gertz and Philip Gill: 09 Jan 2000

   lpoptcmex
        A MATLAB interface to the lpopt optimization routine.

   mexFunction       - the C-MATLAB gateway routine.
   lpSingleStringArg - handles those functions in the lpopt package that
                       take a single string argument.
   lpoptmex          - calls lpopt_
   clpprm            - a C interface to the  lpprm_ function
   clpprmi           - a C interface to the  lpprmi_ function

   From MATLAB, a call to lpoptcmex takes the form

       [...] = lpoptcmex( what, ... )

   where the parameter what determines which function in the lpopt package is to
   be called. See the documentation included with the FORTRAN version of lpopt
   for descriptions of these routines.  Valid values of "what" are

   klpopt              - call lpopt_
   klpprms             - call lpprms.  Takes the file name as a single
                         string argument.
   klpprm              - call lpoptn_. Takes the    option as a single
                         string argument.
   klpSetPrintFile     - declare the location of the lpopt print file.
                         Takes the filename as a single string argument.
   klpClosePrintFile   - close (and hence flush) the print file
   klpSetSummaryFile   - set a file in which to print summmary information.
                         Takes a single string argument, the filename.
   klpCloseSummaryFile - close the summary file.

   The numerical values of these constants are specified in the following enum

       enum { klpopt = 0, klpprms, klpprm,
              klpSetPrintFile, klpClosePrintFile,
              klpSetSummaryFile, klpCloseSummaryFile};

   The number and meaning of the other input and output arguments to lpoptcmex
   depend on the value of "what". If no additional arguments are needed,
   mexFunction performs the operation. Otherwise, mexFunction passes control to
   a more specialized routine.

   It is unnecessary to call lpoptcmex directly. There are MATLAB m-files which
   will call lpoptcmex with the appropriate values of "what" and the
   appropriate parameters.                                                   */
/* ========================================================================= */
#include "mex.h"
#include "mexUtility.h"

#include "f2c.h"
#include "lpopt.h"
#include "lpprmswrapper.h"

enum    { klpopt = 0, klpprms, klpprm,
	  klpSetPrintFile, klpClosePrintFile,
	  klpSetSummaryFile, klpCloseSummaryFile };

void     lpoptmex( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] );

void     lpSingleStringArg( int nlhs, mxArray *plhs[], int nrhs,
			    const mxArray *prhs[], int what );

void     clpprm ( char option[] );
void     clpprmi( char option[], integer i);

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
    clpprm( "Nolist" );
    clpprm( "Print   file = 0" );
    clpprm( "Summary file = 0" );
    clpprm( "List" );

    firstCall = 0;
  }
  if ( nrhs < 1 ) {
    mexErrMsgTxt( "Must have at least 1 input argument" );
  }
  /* what :input argument 1 */
  assertScalar( prhs[0], "what" );
  what = *mxGetPr( prhs[0] );

  switch( what ) {
  case klpopt:
    lpoptmex( nlhs, plhs, nrhs, prhs );
    break;
    /* The next 4 cases require only one string argument for input,
       so we pass them all to the same routine.                    */
  case klpprms:
  case klpprm:
  case klpSetPrintFile:
  case klpSetSummaryFile:
    lpSingleStringArg( nlhs, plhs, nrhs, prhs, what );
    break;

  case klpClosePrintFile:
    /* There are no additional input arguments, so we simply close
       the print file.                                             */
    if ( 1 != nrhs )
      mexErrMsgTxt( "Wrong number of input arguments" );
    if ( printFileIsOpen ) { /* Only close if it was open */
      iunit = 9; lpclose_( &iunit ); printFileIsOpen = 0;
    }
    /* Forget the old print file */
    clpprm( "Nolist" );
    clpprm( "Print file = 0" );
    clpprm( "List" );
    break;

  case klpCloseSummaryFile:
    /* There are no additional input arguments, so we simply close
       the summary file.                                           */
    if ( 1 != nrhs )
      mexErrMsgTxt( "Wrong number of input arguments" );
    if( summaryFileIsOpen ) { /* Only close if it was open */
      iunit = 56; lpclose_( &iunit ); summaryFileIsOpen = 0;
    }
    /* Forget the old summary file */
    clpprm( "Nolist" );
    clpprm( "Summary file = 0" );
    clpprm( "List" );
    break;

  default:
    mexErrMsgTxt( "Unrecognized function name" );
    break;
  }
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void clpprm
  ( char option[] )
{
  int lenOption;

  lenOption = strlen( option );
  lpprm_( option, lenOption );
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void clpprmi
  ( char option[], integer i )
{
  int lenOption;

  lenOption = strlen( option );
  lpprmi_( option, &i, lenOption );
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void lpSingleStringArg
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
  case klpprm:
    lpprm_( stringArg, lenStringArg );
    break;

  case klpprms:
    lpprmswrapper_( stringArg, &inform, lenStringArg );
    break;

  case klpSetPrintFile:
    iunit = 9;
    /* Close the current print file */
    if ( printFileIsOpen ) lpclose_( &iunit );
    lpopenappend_( &iunit, stringArg, &inform, lenStringArg );
    if ( 0 == inform ) {
      /* We were able to open the file. Set the unit number of the
	 print file to the newly opened file.                       */
      printFileIsOpen = 1;
      clpprm( "Nolist" );
      clpprm( "Print file = 9" );
      clpprm( "List" );
    }
    break;

  case klpSetSummaryFile:
    iunit = 56;
    /* Close the current summary file */
    if( summaryFileIsOpen ) lpclose_( &iunit );
    lpopenappend_( &iunit, stringArg, &inform, lenStringArg );
    if ( 0 == inform ) {
      /* We were able to open the file. Set the unit number of the
	 summary file to the newly opened file.                     */
      summaryFileIsOpen = 1;
      clpprm( "Nolist" );
      clpprm( "Summary file = 56" );
      clpprm( "List" );
    }
    break;
  }

  mxFree( stringArg );

  switch(what) {
  case klpprms:
  case klpSetPrintFile:
  case klpSetSummaryFile:
    /* These all return 0 if they were sucessful */
    plhs[0]         = mxCreateDoubleMatrix( 1, 1, mxREAL );
    dummyDoublePtr  = mxGetPr( plhs[0] );
    *dummyDoublePtr = inform;
    break;
  }
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void lpoptmex
  ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{

  double   *dummyDoublePtr;

  double   *A;                            /* input argument  2 */
  double   *l;                            /* input argument  3 */
  double   *u;                            /* input argument  4 */
  double   *c;                            /* input argument  5 */
  double   *x0;                           /* input argument  6 */
  integer  *istate0;                      /* input argument  7 */

  double   *x;                            /* output argument 1 */
  mxArray  *mexx;
  double   *lambda;                       /* output argument 2 */
  mxArray  *mexlambda;
  integer  inform;                        /* output argument 3 */
  integer  iter;                          /* output argument 4 */
  integer  *istate;                       /* output argument 5 */
  mxArray  *mexistate;
  integer  n;                             /* array dimension */
  integer  nCon;                          /* array dimension */
  integer  nclin;                         /* array dimension */

  integer  ldA;
  integer  leniw, lenw, *iw, nQ;
  double   obj;
  double   *Ax;
  double   *w;

  if ( 6 > nrhs || 7 < nrhs ) {
    mexErrMsgTxt( "Wrong number of input arguments" );
  }

  /* x0 : input argument 6, read this first to get a value for n */
  n     = mxGetM( prhs[5] );
  assertColDim( prhs[5], 1, "x0" );
  x0    = mxGetPr( prhs[5] );

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

  /* istate0 : input argument 7, copied into output argument istate */

  /* x      : output argument 1 */
  mexx = mxCreateDoubleMatrix( n, 1, mxREAL );
  x    = mxGetPr( mexx );
  memmove( x, x0, n*sizeof( double ) );

  /* lambda : output argument 2 */
  mexlambda = mxCreateDoubleMatrix( nCon, 1, mxREAL );
  lambda = mxGetPr( mexlambda );

  /* inform : output argument 3 */
  /* inform is scalar           */

  /* iter   : output argument 4 */
  /* iter is scalar             */

  /* istate : output argument 5 */
  mexistate = mxCreateDoubleMatrix( nCon, 1, mxREAL );
  istate = (integer *) mxGetPr( mexistate );

  if ( 7 == nrhs ) {
    /* An initial state was given in input argument 7 */
    /* We will do a warm start                        */
    assertRowDim( prhs[6], nCon, "istate0" );
    assertColDim( prhs[6], 1   , "istate0" );
    /* copy istate0 into istate */
    dble2int( nCon, mxGetPr(prhs[6]), istate );
    clpprm( "Nolist" ); clpprm( "Warm start" ); clpprm( "List" );
  } else {
    /* No initial state given */
    clpprm( "Nolist" ); clpprm( "Cold start" ); clpprm( "List" );
  }

  Ax    = mxCalloc( ldA, sizeof(double));

  leniw = 2*n + 3;
  iw    = mxCalloc( leniw, sizeof( integer ));

  nQ    = nclin + 1;   /* row dimension of Q for the TQ factorization */
  if ( nQ > n ) nQ = n;

  lenw  = 5*ldA + 7*n + 2*nQ*nQ;
  w     = mxCalloc( lenw , sizeof( double ) );

  lpopt_
    ( &n, &nclin, &ldA, A, l, u,
      x, Ax, &inform, &iter, istate, lambda, &obj, c,
      iw, &leniw, w, &lenw );

  /* x      : output argument 1 */
  plhs[0] = mexx;

  /* lambda : output argument 2 */
  if ( 1 < nlhs ) {
    plhs[1] = mexlambda;
  }

  /* inform : output argument 3 */
  if ( 2 < nlhs ) {
    plhs[2]         = mxCreateDoubleMatrix(1, 1, mxREAL );
    dummyDoublePtr  = mxGetPr( plhs[2] );
    *dummyDoublePtr = (double) inform;
  }

  /* iter   : output argument 4 */
  if ( 3 < nlhs ) {
    plhs[3]         = mxCreateDoubleMatrix(1, 1, mxREAL );
    dummyDoublePtr  = mxGetPr( plhs[3] );
    *dummyDoublePtr = (double) iter;
  }

  /* istate : output argument 5 */
  if ( 4 < nlhs ) {
    plhs[4] = mexistate;
    int2dble( nCon, istate, (double *) istate );
  }
}

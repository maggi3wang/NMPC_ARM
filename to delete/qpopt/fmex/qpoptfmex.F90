#include "fintrf.h"
#if 0
!
!     .F90 file needs to be preprocessed to generate .f90 equivalent
!
#endif
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine mexfunction ( nlhs, plhs, nrhs, prhs )

    use precisionModule, only :  ip, rp

    implicit               none

    integer*4           :: nlhs, nrhs
    mwPointer           :: prhs(*), plhs(*)

    ! ====================================================================
    ! qpoptmex.f - Gateway function for qpopt.
    !
    ! To be run from the Matlab file qpopt.m.
    !
    ! [x,obj,lambda,istate,iter,inform]
    !                      = qpoptfmex( full(A),cvec,H,x,bl,bu,msglvl );
    !
    ! 21 Dec 2008
    ! Philip Gill and Michael Saunders
    ! ====================================================================
    mwPointer                :: mxGetPr, mxCreateDoubleMatrix
    mwSize                   :: mxGetM, mxGetN
    integer*4                :: mxIsNumeric
!   integer*4                :: mxIsClass ! Does not work on Mac OS X

    ! Interfaces here
    external                    qpHess  ! Default subroutine in qpoptsubs.f

    real(rp),    allocatable :: A(:,:), H(:,:), cvec(:), x(:), bl(:), bu(:)
    real(rp),    allocatable :: Ax(:), lambda(:), rstate(:), rw(:)
    integer(ip), allocatable :: istate(:), iw(:)

    ! Local vars

    integer(ip)              :: nclin, n, ldA, ldH, lenA, lenBnd, lenH, lenrw, leniw
    integer(ip)              ::  iter,  inform,  msglvl
    real(rp)                 :: riter, rinform, rmsglvl, obj

    !---------------------------------------------------------------------

    if ( nrhs < 6 ) then
       call mexErrMsgTxt( 'Must have at least 6 input arguments' )
    end if

    ! Check that the inputs are numeric.
    if (mxIsNumeric(prhs(1)) .ne. 1) then
       call mexErrMsgTxt( 'Input #1 is not a numeric array.' )
    else if (mxIsNumeric(prhs(2)) .ne. 1) then
       call mexErrMsgTxt( 'Input #2 is not a numeric array.' )
    else if (mxIsNumeric(prhs(3)) .ne. 1) then
       call mexErrMsgTxt( 'Input #3 is not a numeric array.' )
    else if (mxIsNumeric(prhs(4)) .ne. 1) then
       call mexErrMsgTxt( 'Input #4 is not a numeric array.' )
    else if (mxIsNumeric(prhs(5)) .ne. 1) then
       call mexErrMsgTxt( 'Input #5 is not a numeric array.' )
    else if (mxIsNumeric(prhs(6)) .ne. 1) then
       call mexErrMsgTxt( 'Input #6 is not a numeric array.' )
    else if (nrhs >= 7) then
       if (mxIsNumeric(prhs(7)) .ne. 1) then
          call mexErrMsgTxt( 'Input #7 is not a numeric array.' )
       endif
    end if

    !The following does not work on Mac OS X
    ! if (mxIsClass(mxGetPr(prhs(3)),'function_handle') .eq. 1) then
    !   call mexErrMsgTxt( 'Function handle recognised' )
    ! end if

    nclin   = mxGetM ( prhs(1) )
    n       = mxGetN ( prhs(1) )

    ldA     = nclin
    lenA    = ldA*n
    ldH     = n
    lenH    = ldH*n
    lenBnd  = n + nclin

    leniw   = 2*n + 3
    lenrw   = 5*max(nclin,1) + 8*n + 2*n**2

    allocate( A(ldA,n), cvec(n), H(ldH,n), x(n), bl(lenBnd), bu(lenBnd) )
    allocate( Ax(nclin), lambda(lenBnd), iw(leniw), rw(lenrw) )
    allocate( istate(lenBnd), rstate(lenBnd) )

    call mxCopyPtrToReal8( mxGetPr(prhs(1)),       A,   lenA )
    call mxCopyPtrToReal8( mxGetPr(prhs(2)),    cvec,      n )
    call mxCopyPtrToReal8( mxGetPr(prhs(3)),       H,   lenH )
    call mxCopyPtrToReal8( mxGetPr(prhs(4)),       x,      n )
    call mxCopyPtrToReal8( mxGetPr(prhs(5)),      bl, lenBnd )
    call mxCopyPtrToReal8( mxGetPr(prhs(6)),      bu, lenBnd )
    call mxCopyPtrToReal8( mxGetPr(prhs(7)), rmsglvl,      1 )


    msglvl = rmsglvl

    call qpprm ( 'Nolist          ' )
    call qpprm ( 'Print file = 0  ' )

    if (msglvl .eq. 0) then
       call qpprm( 'Summary file = 0' )
    else
       call qpprm( 'Print level  = 5' )
    endif

    call qpopt( n, nclin, ldA, ldH,                    &
                A, bl, bu, cvec, H, qpHess, istate, x, &
                inform, iter, obj, Ax, lambda,         &
                iw, leniw, rw, lenrw  )

    plhs(1) = mxCreateDoubleMatrix( n     , 1_ip, 0)  ! x
    plhs(2) = mxCreateDoubleMatrix( 1_ip  , 1_ip, 0)  ! obj
    plhs(3) = mxCreateDoubleMatrix( lenBnd, 1_ip, 0)  ! lambda
    plhs(4) = mxCreateDoubleMatrix( lenBnd, 1_ip, 0)  ! istate
    plhs(5) = mxCreateDoubleMatrix( 1_ip  , 1_ip, 0)  ! iter
    plhs(6) = mxCreateDoubleMatrix( 1_ip  , 1_ip, 0)  ! inform

    rstate  = istate ! copy integer_ip array into real_rp array
    riter   = iter
    rinform = inform

    call mxCopyReal8ToPtr( x      , mxGetPr(plhs(1)), n      )
    call mxCopyReal8ToPtr( obj    , mxGetPr(plhs(2)), 1_ip   )
    call mxCopyReal8ToPtr( lambda , mxGetPr(plhs(3)), lenBnd )
    call mxCopyReal8ToPtr( rstate , mxGetPr(plhs(4)), lenBnd )
    call mxCopyReal8ToPtr( riter  , mxGetPr(plhs(5)), 1_ip   )
    call mxCopyReal8ToPtr( rinform, mxGetPr(plhs(6)), 1_ip   )

    deallocate( A , cvec, H, x, bl, bu )
    deallocate( Ax, lambda, iw, rw )
    deallocate( istate, rstate )

  end subroutine mexFunction

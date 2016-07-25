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

    !=====================================================================
    ! lpoptmex.f - Gateway function for lpopt.
    !
    ! To be run from the Matlab file lpopt.m.
    !
    ! [x,obj,lambda,istate,iter,inform]
    !                      = lpoptfmex( full(A),cvec,x,bl,bu,msglvl );
    ! 21 Dec 2008
    ! Philip Gill and Michael Saunders
    !=====================================================================
    mwPointer                :: mxGetPr, mxCreateDoubleMatrix
    mwSize                   :: mxGetM, mxGetN
    integer*4                :: mxIsNumeric

    ! Interfaces here

    integer(ip), allocatable :: istate(:), iw(:)
    real(rp),    allocatable :: A(:,:), cvec(:), x(:), bl(:), bu(:)
    real(rp),    allocatable :: Ax(:), lambda(:), rstate(:), rw(:)

    ! Local vars

    integer(ip)              :: nclin, n, ldA, lenA, lenBnd, lenrw, leniw
    integer(ip)              ::  iter,  inform,  msglvl
    real(rp)                 :: riter, rinform, rmsglvl, obj

    !---------------------------------------------------------------------

    if ( nrhs < 5 ) then
        call mexErrMsgTxt( 'Must have at least 5 input arguments' )
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
    else if (nrhs >= 6) then
       if (mxIsNumeric(prhs(6)) .ne. 1) then
          call mexErrMsgTxt( 'Input #6 is not a numeric array.' )
       endif
    endif

    nclin   = mxGetM ( prhs(1) )
    n       = mxGetN ( prhs(1) )

    ldA     = nclin
    lenA    = ldA*n
    lenBnd  = n + nclin

    leniw   = 2*n + 3
    lenrw   = 5*max(nclin,1) + 7*n + 2*(min(nclin+1,n))**2

    allocate( A(ldA,n), cvec(n), x(n), bl(lenBnd), bu(lenBnd) )
    allocate( Ax(nclin), lambda(lenBnd), iw(leniw), rw(lenrw) )
    allocate( istate(lenBnd), rstate(lenBnd) )

    call mxCopyPtrToReal8( mxGetPr(prhs(1)),       A,   lenA )
    call mxCopyPtrToReal8( mxGetPr(prhs(2)),    cvec,      n )
    call mxCopyPtrToReal8( mxGetPr(prhs(3)),       x,      n )
    call mxCopyPtrToReal8( mxGetPr(prhs(4)),      bl, lenBnd )
    call mxCopyPtrToReal8( mxGetPr(prhs(5)),      bu, lenBnd )
    call mxCopyPtrToReal8( mxGetPr(prhs(6)), rmsglvl,      1 )


    msglvl = rmsglvl

    call lpprm ( 'Nolist          ' )
    call lpprm ( 'Print file = 0  ' )

    if (msglvl == 0) then
       call lpprm( 'Summary file = 0' )
    else
       call lpprm( 'Print level  = 5' )
    endif

    call lpopt( n, nclin, ldA,                   &
                A, bl, bu, x, Ax, inform,        &
                iter, istate, lambda, obj, cvec, &
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

    deallocate( A , cvec, x, bl, bu )
    deallocate( Ax, lambda, iw, rw )
    deallocate( istate, rstate )

  end subroutine mexFunction


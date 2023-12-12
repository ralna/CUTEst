! THIS VERSION: CUTEST 2.2 - 2023-11-07 AT 11:20 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      PROGRAM UNCMIN_main

      USE CUTEST_KINDS_precision

      IMPLICIT NONE

!  UNCMIN test driver for problems derived from SIF files.

!  Nick Gould, for CGT Productions, October 1991.
!  Ph. Toint, December 2000.
!  Revised for CUTEst, Nick Gould, January 2013


      INTEGER ( KIND = ip_ ) :: n, method, itrmcd, iexp, msg, ndigit
      INTEGER ( KIND = ip_ ) :: i, ilim, iagflg, iahflg, status
      INTEGER ( KIND = ip_ ), PARAMETER :: input = 55, out = 6, inspec = 46
      INTEGER ( KIND = ip_ ), PARAMETER :: io_buffer = 11
      REAL ( KIND = rp_ ) :: dlt, gradtl, stp, steptl, fscale, fpls
      REAL ( KIND = rp_ ) :: xmax, gnorm, typx
      REAL ( KIND = rp_ ), PARAMETER :: one = 1.0D+0, zero = 0.0D+0
      REAL ( KIND = rp_ ), PARAMETER :: biginf = REAL( 9.0D+19, KIND = rp_ )
      LOGICAL :: bounds
      CHARACTER ( LEN = 10 ) :: pname
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 4 )
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, BL, BU, TYPSIZ
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: XPLS, GPLS
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : , :) :: A, WRK
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: XNAMES
      EXTERNAL :: UNCMIN_evalf, UNCMIN_evalg, UNCMIN_evalh

!  Open the spec file

      OPEN( inspec, FILE = 'UNCMIN.SPC', FORM = 'FORMATTED', STATUS = 'OLD' )

!  read input Spec data

! TYPX        <--  typical size for each component of X
! FSCALE      <--  estimate of scale of minimization function
! METHOD      <--  algorithm to use to solve minimization problem
!                  ( 1 = linesearch, 2 = double dogleg, 3 = Hebden-More )
! IEXP        <--  = 0  if minimization function not expensive to evaluate
! MSG         <--  message to inhibit certain automatic checks + output
! NDIGIT      <--  number of good digits in minimization function
! ILIM        <--  maximum number of allowable iterations
! IAGFLG      <--  =0 if analytic gradient not supplied
! IAHFLG      <--  =0 if analytic hessian not supplied
! DLT         <--  initial trust region radius
! GRADTL      <--  tolerance at which gradient considered close enough
!                  to zero to terminate algorithm
! STEPTL      <--  tolerance at which successive iterates considered
!                  close enough to terminate algorithm

      READ ( inspec, 1000 ) typx, fscale, method, iexp, msg, ndigit,           &
                            ilim, iagflg, iahflg, dlt, gradtl, steptl

!  close input file

      CLOSE ( inspec )

!  open the input data file

      OPEN ( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED', STATUS = 'OLD' )
      REWIND input

!  find the problem dimension

      CALL CUTEST_udimen_r( status, input, n )
      IF ( status /= 0 ) GO TO 910

!  allocate workspace

      ALLOCATE( X( n ), BL( n ), BU( n ), TYPSIZ( n ),                         &
                XPLS( n ), GPLS( n ), A( n, n ), WRK( n, 8 ),                  &
                XNAMES( n ), STAT = status )
      IF ( status /= 0 ) GO TO 990

!  set up SIF data

      CALL CUTEST_usetup_r( status, input, out, io_buffer, n, X, BL, BU )
      CLOSE( input )

!  obtain variable names

      CALL CUTEST_unames_r( status, n, pname, XNAMES )
      IF ( status /= 0 ) GO TO 910

!  set up algorithmic input data

      xmax = zero
      bounds = .FALSE.
      DO i = 1, n
        TYPSIZ( i ) = typx
        xmax = MAX( xmax, ABS( X( i ) ) )
        IF ( BL( i ) .GT. - biginf .OR. BU( i ) .LT. biginf ) BOUNDS = .TRUE.
      END DO
      IF ( bounds ) WRITE( out, 2030 )
      stp = 1000.0 * MAX( one, xmax )

!  call the optimizer

      CALL OPTIF9( n, n, X, UNCMIN_evalf, UNCMIN_evalg, UNCMIN_evalh,          &
                   TYPSIZ, fscale, method, iexp, msg, ndigit, ilim,            &
                   iagflg, iahflg, out, dlt, gradtl, stp,                      &
                   steptl, XPLS, fpls, GPLS, itrmcd, A, WRK )

!  output solution

      CALL CUTEST_ureport_r( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910

      gnorm = zero
      DO i  = 1, n
        gnorm = MAX( gnorm, ABS( GPLS( i ) ) )
      END DO
      WRITE ( out, 2010 )
      DO i = 1, n
        WRITE( out, 2020 ) XNAMES( i ), XPLS( i ), GPLS( i )
      END DO
      WRITE ( out, 2000 ) pname, n, ( CALLS( i ), i = 1, 3 ),                  &
                          itrmcd, fpls, CPU( 1 ), CPU( 2 )

      CALL CUTEST_uterminate_r( status )
      STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )" ) status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP

!  Non-executable statements

 1000 FORMAT( 2( D10.3, /), 7( I10, /), 2( D10.3, / ), D10.3 )
 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //                    &
          ,' Code used               :  UNCMIN',   /                           &
          ,' Problem                 :  ', A10,    /                           &
          ,' # variables             =      ', I10 /                           &
          ,' # objective functions   =        ', F8.2 /                        &
          ,' # objective gradients   =        ', F8.2 /                        &
          ,' # objective Hessians    =        ', F8.2 /                        &
           ' Exit code               =      ', I10 /                           &
          ,' Final f                 = ', ES15.7 /                             &
          ,' Set up time             =      ', 0P, F10.2, ' seconds' /         &
           ' Solve time              =      ', 0P, F10.2, ' seconds' //        &
           66('*') / )
 2010 FORMAT( /, '                 X         G ' )
 2020 FORMAT(  A10, 2ES12.4 )
 2030 FORMAT(  /, ' ** Warning from UNCMIN_main. The problem as stated',       &
                  ' includes simple bounds. ', /,                              &
                  '    These bounds will be ignored. ' )
      END PROGRAM UNCMIN_main

      SUBROUTINE UNCMIN_evalf( n, X, f )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n
      REAL ( KIND = rp_ ) :: f, X( n )

!  Interface for UNCMIN (Schnabel, Koontz and Weiss,
!  ACM Trans. Math. Software, 1982).

      INTEGER ( KIND = ip_ ) :: status
      INTEGER ( KIND = ip_ ), PARAMETER :: out = 6
      CALL CUTEST_ufn_r( status, n, X, f )
      IF ( status .NE. 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )")        &
           status
        STOP
      END IF
      RETURN
      END

      SUBROUTINE UNCMIN_evalg( n, X, G )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n
      REAL ( KIND = rp_ ) :: X( n ), G( n )
      INTEGER ( KIND = ip_ ) :: status
      INTEGER ( KIND = ip_ ), PARAMETER :: out = 6
      CALL CUTEST_ugr_r( status, n, X, G )
      IF ( status .NE. 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )" )       &
           status
        STOP
      END IF
      RETURN
      END

      SUBROUTINE UNCMIN_evalh( nr, n, X, H )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n, nr
      REAL ( KIND = rp_ ) ::X( n ), H( nr, n )
      INTEGER ( KIND = ip_ ) :: status
      INTEGER ( KIND = ip_ ), PARAMETER :: out = 6
      CALL CUTEST_udh_r( status, n, X, nr, H )
      IF ( status .NE. 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )" )       &
           status
        STOP
      END IF
      RETURN
      END

C THIS VERSION: CUTEST 2.2 - 2023-11-26 AT 15:10 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      PROGRAM TENMIN_main

      USE CUTEST_KINDS_precision

C  TENSOR test driver for problems derived from SIF files.

C  Ali Bouaricha, CERFACS (April, 1993), updated by Ph. Toint (March 2001)
C  Revised for CUTEst, Nick Gould, January 2013

      INTEGER ( KIND = ip_ ) :: n, method, itnno, msg, ndigit
      INTEGER ( KIND = ip_ ) :: i, ilim, iagflg, iahflg, status
      INTEGER ( KIND = ip_ ),  PARAMETER :: input = 55, out = 6
      INTEGER ( KIND = ip_ ),  PARAMETER :: io_buffer = 11, inspec = 46
      REAL ( KIND = rp_ ) :: gradtl, steptl, fscale, fpls, stepmx, typx
      REAL ( KIND = rp_ ) :: gnorm
      REAL ( KIND = rp_ ), PARAMETER :: biginf = REAL( 9.0D+19, rp_ )
      REAL ( KIND = rp_ ), PARAMETER :: zero = 0.0_rp_
      LOGICAL :: bounds
      CHARACTER ( LEN = 10 ) :: pname
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 4 )
      INTEGER ( KIND = ip_ ),  ALLOCATABLE, DIMENSION( : ) :: IWRK
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, TYPSIZ
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: XPLS, GPLS
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : , :) :: H, WRK
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: XNAMES
      EXTERNAL :: TENMIN_evalf, TENMIN_evalg, TENMIN_evalh

C  open the Spec file for the method.

      OPEN ( inspec, FILE = 'TENMIN.SPC', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND( inspec )

C  read input Spec data

C ILIM        <--  MAXIMUM NUMBER OF ALLOWABLE ITERATIONS
C GRADTL      <--  TOLERANCE AT WHICH GRADIENT CONSIDERED CLOSE ENOUGH
C                  TO ZERO TO TERMINATE ALGORITHM
C IAGFLG      <--  =0 IF ANALYTIC GRADIENT NOT SUPPLIED
C IAHFLG      <--  =0 IF ANALYTIC HESSIAN NOT SUPPLIED
C FSCALE      <--  ESTIMATE OF SCALE OF MINIMIZATION FUNCTION
C TYPX        <--  TYPICAL SIZE FOR EACH COMPONENT OF X
C METHOD      <--  ALGORITHM TO USE TO SOLVE MINIMIZATION PROBLEM
C                  ( 1 = LINESEARCH, 2 = DOUBLE DOGLEG, 3 = HEBDEN-MORE )
C MSG         <--  MESSAGE TO INHIBIT CERTAIN AUTOMATIC CHECKS + OUTPUT

      READ ( inspec, 1000 ) ilim, gradtl, iagflg, iahflg, fscale,
     *                      typx, method, msg

C  close input file

      CLOSE ( inspec )

C  open the input data file

      OPEN ( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND( input )

C  find the problem dimension

      CALL CUTEST_udimen_r( status, input, n )
      IF ( status /= 0 ) GO TO 910

C  allocate workspace

      ALLOCATE( IWRK( n ), X( n ), TYPSIZ( n ), 
     *          XPLS( n ), GPLS( n ), H( n, n ), WRK( n, 8 ), 
     *          XNAMES( n ), STAT = status )
      IF ( status /= 0 ) GO TO 990

C  set up SIF data

      CALL CUTEST_usetup_r( status, INPUT, out, io_buffer, 
     *                      n, X, XPLS, GPLS )

C  obtain variable names

      CALL CUTEST_unames_r( status, n, pname, XNAMES )
      IF ( status /= 0 ) GO TO 910

C  set up algorithmic input data

      ndigit = 15
      steptl = REAL( 1.0D-5, KIND = rp_ )
      stepmx = BIGINF

      bounds = .FALSE.
      DO 10 i = 1, n
        TYPSIZ( i ) = typx
        IF ( XPLS( i ) .GT. - biginf .OR. GPLS( i ) .LT. biginf )
     *    BOUNDS = .TRUE.
   10 CONTINUE
      IF ( bounds ) WRITE( out, 2030 )

C  call the optimizer

      CALL TENSOR( n, n, X, TENMIN_evalf, TENMIN_evalg, TENMIN_evalh, 
     *             TYPSIZ, fscale, gradtl, steptl, ilim, stepmx, out, 
     *             method, iagflg, iahflg, ndigit, msg, XPLS, 
     *             fpls, GPLS, H, itnno, WRK, IWRK  )

C  output solution

      CALL CUTEST_ureport_r( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910

      gnorm = zero
      DO 20 i  = 1, n
        gnorm = MAX( gnorm, ABS( GPLS( i ) ) )
   20 CONTINUE
      WRITE ( out, 2010 )
      DO 30 i = 1, n
        WRITE( out, 2020 ) XNAMES( i ), XPLS( i ), GPLS( i )
   30 CONTINUE
      WRITE ( out, 2001 ) gnorm
      WRITE ( out, 2000 ) pname, n, ( CALLS( i ), i = 1, 3 ),
     *                    itnno, fpls, CPU( 1 ), CPU( 2 )
      CLOSE( input  )
      CALL CUTEST_uterminate_r( status )
      STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP

C  Non-executable statements

 1000 FORMAT( I10, /, D10.3, 2(/,I10), 2(/,D10.3), 2(/,I10) )
 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *    ,' Package used            :  TENMIN',   /
     *    ,' Problem                 :  ', A10,    /
     *    ,' # variables             =      ', I10 /
     *    ,' # objective functions   =        ', F8.2 /
     *    ,' # objective gradients   =        ', F8.2 / 
     *    ,' # objective Hessians    =        ', F8.2 /
     *     ' Exit code               =      ', I10 /
     *    ,' Final f                 = ', E15.7 /
     *    ,' Set up time             =      ', 0P, F10.2, ' seconds' /
     *     ' Solve time              =      ', 0P, F10.2, ' seconds' //
     *     66('*') / )
 2001 FORMAT( / ' Final ||g|| =', 1P, D12.4 )
 2010 FORMAT( /, '                 X         G ' )
 2020 FORMAT(  A10, 1P, 2D12.4 )
 2030 FORMAT(  /, ' ** Warning from TENMIN_main. The problem as stated',
     *            ' includes simple bounds. ', /,
     *            '    These bounds will be ignored. ' )
      END

      SUBROUTINE TENMIN_evalf( n, X, f )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n
      REAL ( KIND = rp_ ) :: f, X( n )

C  Interface for TENMIN (Chow, Schnabel, Eskow, 1993)

      INTEGER ( KIND = ip_ ) :: status
      INTEGER ( KIND = ip_ ), PARAMETER :: out = 6
      CALL CUTEST_ufn_r( status, n, X, f )
      IF ( status /= 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *     status
        STOP
      END IF
      RETURN
      END

      SUBROUTINE TENMIN_evalg( n, X, G )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n
      REAL ( KIND = rp_ ) :: X( n ), G( n )
      INTEGER ( KIND = ip_ ) :: status
      INTEGER ( KIND = ip_ ), PARAMETER :: out = 6
      CALL CUTEST_ugr_r( status, n, X, G )
      IF ( status /= 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *     status
        STOP
      END IF
      RETURN
      END

      SUBROUTINE TENMIN_evalh( nr, n, X, H )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n, nr
      REAL ( KIND = rp_ ) ::X( n ), H( nr, n )
      INTEGER ( KIND = ip_ ) :: status
      INTEGER ( KIND = ip_ ), PARAMETER :: out = 6
      CALL CUTEST_udh_r( status, n, X, nr, H )
      IF ( status /= 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *     status
        STOP
      END IF
      RETURN
      END

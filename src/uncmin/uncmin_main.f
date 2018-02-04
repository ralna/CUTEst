C     ( Last modified on 5 Jan 2013 at 12:40:00 )

      PROGRAM          UNCMIN_main

C  UNCMIN test driver for problems derived from SIF files.

C  Nick Gould, for CGT Productions, October 1991.
C  Ph. Toint, December 2000.
C  Revised for CUTEst, Nick Gould, January 2013

      INTEGER :: n, method, itrmcd, iexp, msg, ndigit
      INTEGER :: i, ilim, iagflg, iahflg, status
      INTEGER, PARAMETER :: input = 55, out = 6, inspec = 46
      INTEGER, PARAMETER :: io_buffer = 11
      DOUBLE PRECISION :: dlt, gradtl, stp, steptl, fscale, fpls
      DOUBLE PRECISION :: xmax, gnorm, typx
      DOUBLE PRECISION, PARAMETER :: one = 1.0D+0, zero = 0.0D+0
      DOUBLE PRECISION, PARAMETER :: biginf = 9.0D+19
      LOGICAL :: bounds
      CHARACTER ( LEN = 10 ) :: pname
      DOUBLE PRECISION :: CPU( 2 ), CALLS( 4 )
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: X, BL, BU, TYPSIZ
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: XPLS, GPLS
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : , :) :: A, WRK
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: XNAMES
      EXTERNAL :: UNCMIN_evalf, UNCMIN_evalg, UNCMIN_evalh

C  Open the spec file

      OPEN( inspec, FILE = 'UNCMIN.SPC', FORM = 'FORMATTED',
     *      STATUS = 'OLD' )

C  read input Spec data

C TYPX        <--  TYPICAL SIZE FOR EACH COMPONENT OF X
C FSCALE      <--  ESTIMATE OF SCALE OF MINIMIZATION FUNCTION
C METHOD      <--  ALGORITHM TO USE TO SOLVE MINIMIZATION PROBLEM
C                  ( 1 = LINESEARCH, 2 = DOUBLE DOGLEG, 3 = HEBDEN-MORE )
C IEXP        <--  =0 IF MINIMIZATION FUNCTION NOT EXPENSIVE TO EVALUATE
C MSG         <--  MESSAGE TO INHIBIT CERTAIN AUTOMATIC CHECKS + OUTPUT
C NDIGIT      <--  NUMBER OF GOOD DIGITS IN MINIMIZATION FUNCTION
C ILIM        <--  MAXIMUM NUMBER OF ALLOWABLE ITERATIONS
C IAGFLG      <--  =0 IF ANALYTIC GRADIENT NOT SUPPLIED
C IAHFLG      <--  =0 IF ANALYTIC HESSIAN NOT SUPPLIED
C DLT         <--  INITIAL TRUST REGION RADIUS
C GRADTL      <--  TOLERANCE AT WHICH GRADIENT CONSIDERED CLOSE ENOUGH
C                  TO ZERO TO TERMINATE ALGORITHM
C STEPTL      <--  TOLERANCE AT WHICH SUCCESSIVE ITERATES CONSIDERED
C                  CLOSE ENOUGH TO TERMINATE ALGORITHM
C
      READ ( inspec, 1000 ) typx, fscale, method, iexp, msg, ndigit,
     *                      ilim, iagflg, iahflg, dlt, gradtl, steptl

C  close input file

      CLOSE ( inspec )

C  open the input data file

      OPEN ( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND input

C  find the problem dimension

      CALL CUTEST_udimen( status, input, n )
      IF ( status /= 0 ) GO TO 910

C  allocate workspace

      ALLOCATE( X( n ), BL( n ), BU( n ), TYPSIZ( n ), 
     *          XPLS( n ), GPLS( n ), A( n, n ), WRK( n, 8 ), 
     *          XNAMES( n ), STAT = status )
      IF ( status /= 0 ) GO TO 990

C  set up SIF data

      CALL CUTEST_usetup( status, input, out, io_buffer, n, X, BL, BU )

C  obtain variable names

      CALL CUTEST_unames( status, n, pname, XNAMES )
      IF ( status /= 0 ) GO TO 910

C  set up algorithmic input data

      xmax = zero
      bounds = .FALSE.
      DO 10 i = 1, n
        TYPSIZ( i ) = typx
        xmax = MAX( xmax, ABS( X( i ) ) )
        IF ( BL( i ) .GT. - biginf .OR. BU( i ) .LT. biginf )
     *    BOUNDS = .TRUE.
   10 CONTINUE
      IF ( bounds ) WRITE( out, 2030 )
      stp = 1000.0 * MAX( one, xmax )

C  call the optimizer

      CALL OPTIF9( n, n, X, UNCMIN_evalf, UNCMIN_evalg, UNCMIN_evalh, 
     *             TYPSIZ, fscale, method, iexp, msg, ndigit, ilim,
     *             iagflg, iahflg, out, dlt, gradtl, stp,
     *             steptl, XPLS, fpls, GPLS, itrmcd, A, WRK )

C  output solution

      CALL CUTEST_ureport( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910

      gnorm = zero
      DO 20 i  = 1, n
        gnorm = MAX( gnorm, ABS( GPLS( i ) ) )
   20 CONTINUE
      WRITE ( out, 2010 )
      DO 30 i = 1, n
        WRITE( out, 2020 ) XNAMES( i ), XPLS( i ), GPLS( i )
   30 CONTINUE
      WRITE ( out, 2000 ) pname, n, ( CALLS( i ), i = 1, 3 ),
     *                    itrmcd, fpls, CPU( 1 ), CPU( 2 )
      CLOSE( input  )
      STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP

C  Non-executable statements

 1000 FORMAT( 2( D10.3, /), 7( I10, /), 2( D10.3, / ), D10.3 )
 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *    ,' Code used               :  UNCMIN',   /
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
 2010 FORMAT( /, '                 X         G ' )
 2020 FORMAT(  A10, 1P, 2D12.4 )
 2030 FORMAT(  /, ' ** Warning from UNCMIN_main. The problem as stated',
     *            ' includes simple bounds. ', /,
     *            '    These bounds will be ignored. ' )
      END

      SUBROUTINE UNCMIN_evalf( n, X, f )
      INTEGER :: n
      DOUBLE PRECISION :: f, X( n )

C  Interface for UNCMIN (Schnabel, Koontz and Weiss,
C  ACM Trans. Math. Software, 1982).

      INTEGER :: status
      INTEGER, PARAMETER :: out = 6
      CALL CUTEST_ufn( status, n, X, f )
      IF ( status .NE. 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *     status
        STOP
      END IF
      RETURN
      END

      SUBROUTINE UNCMIN_evalg( n, X, G )
      INTEGER :: n
      DOUBLE PRECISION :: X( n ), G( n )
      INTEGER :: status
      INTEGER, PARAMETER :: out = 6
      CALL CUTEST_ugr( status, n, X, G )
      IF ( status .NE. 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *     status
        STOP
      END IF
      RETURN
      END

      SUBROUTINE UNCMIN_evalh( nr, n, X, H )
      INTEGER :: n, nr
      DOUBLE PRECISION ::X( n ), H( nr, n )
      INTEGER :: status
      INTEGER, PARAMETER :: out = 6
      CALL CUTEST_udh( status, n, X, nr, H )
      IF ( status .NE. 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *     status
        STOP
      END IF
      RETURN
      END

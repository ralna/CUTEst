C     ( Last modified on 5 Jan 2013 at 14:40:00 )

      PROGRAM          TENMIN_main

C  TENSOR test driver for problems derived from SIF files.

C  Ali Bouaricha, CERFACS (April, 1993), updated by Ph. Toint (March 2001)
C  Revised for CUTEst, Nick Gould, January 2013

      INTEGER :: n, method, itnno, msg, ndigit
      INTEGER :: i, ilim, iagflg, iahflg, status
      INTEGER, PARAMETER :: input = 55, out = 6, inspec = 46
      INTEGER, PARAMETER :: io_buffer = 11
      DOUBLE PRECISION :: gradtl, steptl, fscale, fpls, stepmx, typx
      DOUBLE PRECISION :: gnorm
      DOUBLE PRECISION, PARAMETER :: biginf = 9.0D+19, zero = 0.0D+0
      LOGICAL :: bounds
      CHARACTER ( LEN = 10 ) :: pname
      DOUBLE PRECISION :: CPU( 4 ), CALLS( 4 )
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: IWRK
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: X, TYPSIZ
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: XPLS, GPLS
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : , :) :: H, WRK
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

      CALL CUTEST_udimen( status, input, n )
      IF ( status /= 0 ) GO TO 910

C  allocate workspace

      ALLOCATE( IWRK( n ), X( n ), TYPSIZ( n ), 
     *          XPLS( n ), GPLS( n ), H( n, n ), WRK( n, 8 ), 
     *          XNAMES( n ), STAT = status )
      IF ( status /= 0 ) GO TO 990

C  set up SIF data

      CALL CUTEST_usetup( status, INPUT, out, io_buffer, 
     *                    n, X, XPLS, GPLS )

C  obtain variable names

      CALL CUTEST_unames( status, n, pname, XNAMES )
      IF ( status /= 0 ) GO TO 910

C  set up algorithmic input data

      ndigit = 15
      steptl = 1.0D-5
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
      WRITE ( out, 2001 ) gnorm
      WRITE ( out, 2000 ) pname, n, ( CALLS( i ), i = 1, 3 ),
     *                    itnno, fpls, CPU( 1 ), CPU( 2 )
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
      INTEGER :: n
      DOUBLE PRECISION :: f, X( n )

C  Interface for TENMIN (Chow, Schnabel, Eskow, 1993)

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

      SUBROUTINE TENMIN_evalg( n, X, G )
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

      SUBROUTINE TENMIN_evalh( nr, n, X, H )
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

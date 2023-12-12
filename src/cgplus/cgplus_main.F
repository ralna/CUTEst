C  THIS VERSION: CUTEST 2.2 - 2023-11-29 AT 14:40 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      PROGRAM CGPLUS_main

C  CG+ test driver for problems derived from SIF files.

C  Nick Gould, for CGT Productions.
C  July 2004
C  Revised for CUTEst, January 2013

      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n, maxit, status, iflag, IPRINT( 2 )
      INTEGER ( KIND = ip_ ) :: lp, mp, i, method, iter, nfun, irest
      INTEGER ( KIND = ip_ ) :: io_buffer = 11
      INTEGER ( KIND = ip_ ), PARAMETER :: out = 6, input = 55
      INTEGER ( KIND = ip_ ), PARAMETER :: inspec = 56
      REAL ( KIND = rp_ ) :: f, eps, gnorm, tlev
      REAL ( KIND = rp_ ), PARAMETER :: biginf = 9.0E+19_rp_
      LOGICAL :: bounds, finish
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, W, D
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: G, GOLD
      CHARACTER ( LEN = 10 ) :: pname, spcdat
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: XNAMES
      COMMON / CGDD /  mp, lp
      COMMON / RUNINF / iter, nfun
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 4 )
C
C  Open the Spec file for the method.

      spcdat = 'CGPLUS.SPC'
      OPEN ( inspec, FILE = spcdat, FORM = 'FORMATTED',
     *      STATUS = 'OLD' )
      REWIND( inspec )

C  Read input Spec data.

C     IPRINT(1): specifies the frequency of output
C     IPRINT(2): specifies the amount of output
C     METHOD   : method used (1=Fletcher-Reeves,2=Polak-Ribiere,3=P-R+)
C     IREST    : no restart (0) or restart every n iterations (1)
C     MAXIT    : maximum number of iterations
C     EPS      : the required norm of the gradient

      READ ( inspec, 1000 ) IPRINT( 1 ), IPRINT( 2 ), method, irest,
     *                      maxit, eps

C  Close input file.

      CLOSE ( inspec )

C  Open the relevant file.

      OPEN ( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )

C  Check to see if there is sufficient room

      CALL CUTEST_udimen_r( status, input, n )
      IF ( status /= 0 ) GO TO 910

      ALLOCATE( X( n ), G( n ), D( n ), GOLD( n ), W( n ), XNAMES( n ),
     *          STAT = status )
      IF ( status /= 0 ) GO TO 990

C  Set up SIF data.

      CALL CUTEST_usetup_r( status, input, out, io_buffer, n, X, 
     *                      W, GOLD )
      IF ( status /= 0 ) GO TO 910

C  Obtain variable names.

      CALL CUTEST_unames_r( status, N, PNAME, XNAMES )
      IF ( status /= 0 ) GO TO 910

C  Set up algorithmic input data.

      bounds = .FALSE.
      DO 10 i = 1, n
        IF ( W( I ) .GT. - biginf .OR. GOLD( I ) .LT. biginf )
     *    bounds = .TRUE.
   10 CONTINUE
      IF ( bounds ) WRITE( out, 2030 )
      lp = out
      mp = out
      iter = - 1
      iflag = 0
      finish = .FALSE.

C  Optimization loop

   20 CONTINUE
        iter = iter + 1

C  Evaluate the function and gradient

        CALL CUTEST_uofg_r( status, n, X, f, G, .TRUE. )
        IF ( status /= 0 ) GO TO 910

C  Call the optimizer.

   30   CONTINUE
        CALL CGFAM( n, X, f, G, D, GOLD, iprint, EPS, W,
     *              iflag, irest, method, finish )

C  Test for termination

        IF ( iflag .LE. 0 .OR. iter .GT. maxit ) GO TO 50
        IF ( iflag .EQ. 1 ) GO TO 20

C Termination Test.  The user may replace it by some other test. However,
C the parameter 'FINISH' must be set to 'TRUE' when the test is satisfied.

        IF ( iflag .EQ. 2 ) THEN
          tlev = eps * ( 1.0E+0_rp_ + ABS( f ) )
          DO 40 i = 1, n
            IF( ABS( G( i ) ) .GT. tlev ) GO TO 30
  40      CONTINUE
          finish = .TRUE.
          GO TO 30
        ENDIF
   50 CONTINUE

C  Terminal exit.

      CALL CUTEST_ureport_r( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910
      gnorm = 0.0_rp_
      DO 110 i = 1, n
         gnorm = MAX( gnorm, ABS( G( i ) ) )
  110 CONTINUE
      WRITE ( out, 2010 ) f, gnorm
C     WRITE ( out, 2040 )
C      DO 120 I = 1, N
C         WRITE( out, 2020 ) XNAMES( I ), X( I ), G( I )
C  120 CONTINUE
      WRITE ( out, 2000 ) pname, n, INT( CALLS(1) ), INT( CALLS(2) ),
     *                    iflag, F, CPU(1), CPU(2)
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

C  Non-executable statements.

 1000 FORMAT( 5( I10, / ), D10.3 )
 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *    ,' Package used            :  CG+',     /
     *    ,' Problem                 :  ', A10,    /
     *    ,' # variables             =      ', I10 /
     *    ,' # objective functions   =      ', I10 /
     *    ,' # objective gradients   =      ', I10 /
     *     ' Exit code               =      ', I10 /
     *    ,' Final f                 = ', E15.7 /
     *    ,' Set up time             =      ', 0P, F10.2, ' seconds' /
     *     ' Solve time              =      ', 0P, F10.2, ' seconds' //
     *     66('*') / )
 2010 FORMAT( ' Final objective function value  = ', 1P, D12.4,
     *        /, ' Final norm of gradient          = ', 1P, D12.4 )
C2020 FORMAT(  1X, A10, 1P, 2D12.4 )
 2030 FORMAT(  /, ' ** Warning from CGPMA. The problem as stated',
     *            ' includes simple bounds. ', /,
     *            '    These bounds will be ignored. ' )
C2040 FORMAT( /, '                 X         G ' )
      END

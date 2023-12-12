! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      PROGRAM CG_DESCENT_main

C  CG_DESCENT test driver for problems derived from SIF files.

C  Nick Gould, for CGT Productions.
C  July 2004
C  Revised for CUTEst, January 2013

C  A number of updates to the structure of the SPEC file
C  to accomodate version 1.4 of CG_DESCENT.
C  Dominique Orban, July 2007

      USE CUTEST_KINDS_precision
      IMPLICIT NONE

      INTEGER ( KIND = ip_ ) :: i, iter, n, nf, ng, nxpand, nsecnt
      INTEGER ( KIND = ip_ ) :: status, stat
      INTEGER ( KIND = ip_ ) :: io_buffer = 11
      INTEGER ( KIND = ip_ ), PARAMETER :: out  = 6, outcp = 57
      INTEGER ( KIND = ip_ ), PARAMETER :: input = 55, inspec = 56
      REAL ( KIND = rp_ ) :: f, tol, gnorm
      LOGICAL :: bounds
      REAL ( KIND = rp_ ) :: delta, sigma, epsilon, gamma,stopfa
      REAL ( KIND = rp_ ) :: rho, eta, psi0, psi1, psi2, quadcu, rstrtf
      REAL ( KIND = rp_ ) :: maxitf, feps, awlffct, qdecay
      LOGICAL :: quadst, prntlv, prntfi, strule, awolfe
      LOGICAL :: step, prtrul, debug
      REAL ( KIND = rp_ ), PARAMETER :: biginf = 9.0D+19, zero = 0.0D0
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, G, D
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: XTEMP, GTEMP
      CHARACTER ( LEN = 10 ) :: pname
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: XNAMES
      CHARACTER ( LEN = 17 ), PARAMETER :: cgparm = 'cg_descent_f.parm'
      CHARACTER ( LEN = 15 ) :: spcdat
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 4 )
      EXTERNAL :: CG_DESCENT_EVALF, CG_DESCENT_EVALG

C  Open the Spec file for the method.

      SPCDAT = 'CG_DESCENT_FORTRAN.SPC'
      OPEN ( inspec, FILE = SPCDAT, FORM = 'FORMATTED',
     *      STATUS = 'OLD' )
      REWIND( inspec )

C  Read input Spec data.

      READ( inspec, 1000 ) tol, gnorm, delta, sigma, epsilon, gamma,
     *                     rho, eta, psi0, psi1, psi2, quadcu, stopfa, 
     *                     awlffct, rstrtf, maxitf, feps, qdecay, 
     *                     nxpand, nsecnt, prtrul, quadst, prntlv, 
     *                     prntfi, strule, awolfe, step, debug

C  Close input file.

      CLOSE ( inspec )

C  Create the required data input file

      OPEN( outcp, FILE = CGPARM, FORM = 'FORMATTED',
     *       STATUS = 'UNKNOWN' )
      REWIND( outcp )
      WRITE( outcp, 1001 ) delta, sigma, epsilon, gamma, rho, eta,
     *                     psi0, psi1, psi2, quadcu, stopfa, awlffct,
     *                     rstrtf, maxitf, feps, qdecay, nxpand,
     *                     nsecnt, prtrul, quadst, prntlv, prntfi,
     *                     strule, awolfe, step, debug
      CLOSE( outcp )

C  Open the relevant file.

      OPEN ( INPUT, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )

C  Check to see if there is sufficient room

      CALL CUTEST_udimen_r( status, input, n )
      IF ( status /= 0 ) GO TO 910

      ALLOCATE( X( n ), G( n ), D( n ), XTEMP( n ), GTEMP( n ), 
     *          XNAMES( n ), STAT = status )
      IF ( status /= 0 ) GO TO 990

C  Set up SIF data.

      CALL CUTEST_usetup_r( status, input, out, io_buffer, n, X, 
     *                    XTEMP, GTEMP )
      IF ( status /= 0 ) GO TO 910

C  Obtain variable names.

      CALL CUTEST_unames_r( status, N, PNAME, XNAMES )
      IF ( status /= 0 ) GO TO 910

C  Set up algorithmic input data.

      bounds = .FALSE.
      DO 10 i = 1, n
        IF ( XTEMP( i ) .GT. - biginf .OR. GTEMP( i ) .LT. biginf )
     *    bounds = .TRUE.
   10 CONTINUE
      IF ( bounds ) WRITE( out, 2030 )

C  Set up initial step length if requested

      IF ( step ) THEN
        IF ( gnorm .LE. 0.0D+0 ) THEN
          CALL CUTEST_ugr_r( status, N, X, G )
          IF ( status /= 0 ) GO TO 910
          gnorm = 0.0D+0
          DO 11 i = 1, n
            gnorm = MAX( gnorm, DABS( G( i ) ) )
   11     CONTINUE
        ENDIF
      ENDIF

C  Call the optimizer.

      CALL CG_DESCENT( tol, X, n, CG_DESCENT_evalf, CG_DESCENT_evalg, 
     &                 stat, gnorm, f, iter, nf, ng, D, G, 
     &                 XTEMP, GTEMP )

C  Terminal exit.

      CALL CUTEST_ureport_r( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910
      WRITE ( out, 2010 ) f, gnorm
C      DO 120 i = 1, n
C         WRITE( out, 2020 ) XNAMES( i ), X( i ), G( i )
C  120 CONTINUE
      WRITE ( out, 2000 ) pname, n, INT( CALLS(1) ), INT( CALLS(2) ),
     *                    stat, f, CPU(1), CPU(2) 
      CLOSE( INPUT  )
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

 1000 FORMAT( 18( D10.3, / ), 2( I10, / ), 7( L10, / ), L10 )
 1001 FORMAT( 16( D10.3, / ), 2( I10, / ), 7( L10, / ), L10 )
 2000 FORMAT( /, 24('*'), ' CUTEr statistics ', 24('*') //
     *    ,' Code used               :  CG_DESCENT',     /
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
     *        /, ' Final norm of gradient          = ', 1P, D12.4,
     *        //, '                 X         G ' )
 2030 FORMAT(  /, ' ** Warning from CGDMA. The problem as stated',
     *            ' includes simple bounds. ', /,
     *            '    These bounds will be ignored. ' )
      END

      SUBROUTINE CG_DESCENT_evalf( f, X, n )

C  Evaluate the objective function

      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n
      REAL ( KIND = rp_ ) :: f
      REAL ( KIND = rp_ ) :: X( n )
      EXTERNAL CUTEST_ufn

      INTEGER ( KIND = ip_ ) :: status

      CALL CUTEST_ufn_r( status, n, X, f )
      IF ( status /= 0 ) STOP

      RETURN
      END
      
      SUBROUTINE CG_DESCENT_evalg( G, X, n )

C  Evaluate the gradiuent of the objective function

      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n
      REAL ( KIND = rp_ ) :: X( n ), G( n )
      EXTERNAL CUTEST_ugr
      INTEGER ( KIND = ip_ ) :: status

      CALL CUTEST_ugr_r( status, N, X, G )
      IF ( status /= 0 ) STOP

      RETURN
      END

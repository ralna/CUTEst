C     ( Last modified on 2 Jan 2013 at 13:40:00 )
      PROGRAM          LBFGS_main
C
C  LBFGS test driver for problems derived from SIF files.
C
C  Nick Gould and Ph. Toint, for CGT Productions.
C  Revised for CUTEst, January 2013
C
      INTEGER :: N, M, status, LP, MP, LW, I, MAXIT, iflag
      INTEGER :: icall, IPRINT( 2 )
      INTEGER, PARAMETER :: input = 55, out = 6 , inspec = 46
      INTEGER, PARAMETER :: io_buffer = 11
      DOUBLE PRECISION F, EPS, XTOL, GTOL, GNORM, BIGINF
      DOUBLE PRECISION one, ZERO, STPMIN, STPMAX
      LOGICAL DIAGCO, BOUNDS
      PARAMETER ( BIGINF = 9.0D+19, ZERO = 0.0D0, one = 1.0D0 )
      CHARACTER ( LEN = 10 ) :: PNAME, SPCDAT
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: X, G, DIAG, W
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: XNAMES
      EXTERNAL LB2
      COMMON / LB3 / MP, LP, GTOL, STPMIN, STPMAX
      DOUBLE PRECISION :: CPU( 4 ), CALLS( 4 )
C     
C  Open the Spec file for the method.
C
      SPCDAT = 'LBFGS.SPC'
      OPEN ( INSPEC, FILE = SPCDAT, FORM = 'FORMATTED',
     *      STATUS = 'OLD' )
      REWIND INSPEC
C
C  Read input Spec data.
C
C     M        : the number of iterations in the memory
C     IPRINT(1): specifies the frequency of output
C     IPRINT(2): specifies the amount of output
C     MAXIT    : the maximum number of iterations,
C     EPS      : the required norm of the gradient
C
      READ ( INSPEC, 1000 ) M, IPRINT( 1 ), IPRINT( 2 ), MAXIT, 
     *                      EPS
C
C  Close input file.
C
      CLOSE ( INSPEC )
C
C  Open the relevant file.
C
      OPEN ( INPUT, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
C
C  Find the problem dimension
C
      CALL CUTEST_udimen( status, INPUT, n )
      IF ( status /= 0 ) GO TO 910

C  Allocate workspace

      lw  = n * ( 2 * m + 1 ) + 2 * m
      ALLOCATE( X( n ), G( n ), DIAG( n ), W( lw ), XNAMES( n ),
     *          STAT = status )
      IF ( status /= 0 ) GO TO 990
C
C  Set up SIF data.
C
      CALL CUTEST_usetup( status, INPUT, out, io_buffer, N, X, W, 
     *                    W( n + 1 ) )
      IF ( status /= 0 ) GO TO 910
C
C  Obtain variable names.
C
      CALL CUTEST_unames( status, N, PNAME, XNAMES )
C
C  Set up algorithmic input data.
C
      BOUNDS  = .FALSE.
      DO 10 I = 1, N
         IF ( W( I ) > - BIGINF .OR. W( n + i ) < BIGINF )
     *      BOUNDS = .TRUE.
   10 CONTINUE
      IF ( BOUNDS ) WRITE( out, 2030 )
      LP     = out
      MP     = out
      ICALL  = 0
      IFLAG  = 0
      DIAGCO = .FALSE.
      XTOL = EPSILON( one )
   20 CONTINUE
C
C  Evaluate the function and gradient.
C
      CALL CUTEST_uofg( status, N, X, F, G, .TRUE. )
      IF ( status /= 0 ) GO TO 910
C
C  Call the optimizer.
C
      CALL LBFGS( N, M, X, F, G, DIAGCO, DIAG, IPRINT, EPS, XTOL,
     *            W, IFLAG )
C
C  Check exit conditions and prepare for re-entry.
C
      IF ( IFLAG. GT. 0 ) THEN
         ICALL = ICALL + 1
         IF ( ICALL .LE. MAXIT ) GO TO 20
      END IF
C
C  Terminal exit.
C
      CALL CUTEST_ureport( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910
      GNORM    = ZERO
      DO 30 I  = 1, N
         GNORM = MAX( GNORM, ABS( G( I ) ) )
   30 CONTINUE
      WRITE ( out, 2010 ) F, GNORM
      DO 40 I = 1, N
         WRITE( out, 2020 ) XNAMES( I ), X( I ), G( I )
   40 CONTINUE
      WRITE ( out, 2000 ) PNAME, N, INT( CALLS(1) ), INT( CALLS(2) ),
     *                     IFLAG, F, CPU(1), CPU(2) 
      CLOSE( INPUT  )
      CALL CUTEST_uterminate( status )
      STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP
C
C  Non-executable statements.
C
 1000 FORMAT( 4( I10, / ), D10.3 )
 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *    ,' Package used            :  L-BFGS',    /
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
 2020 FORMAT(  1X, A10, 1P, 2D12.4 )
 2030 FORMAT(  /, ' ** Warning from LBFGS_main. The problem as stated',
     *            ' includes simple bounds. ', /,
     *            '    These bounds will be ignored. ' )
      END PROGRAM LBFGS_main


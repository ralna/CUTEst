C     ( Last modified on 3 Jan 2013 at 13:40:00 )
      PROGRAM COBMA
C
C  COBYLA test driver for problems derived from SIF files.
C
C  A. R. Conn and Ph. Toint (based upon Nick Gould's vf13ma.f)
C  January 1995.
C
      INTEGER          M, N, MAXFUN, MCON, LW, LIW
      INTEGER          IPRINT, I, MGEQ, INDR, INPUT, IOUT, NFIX
      INTEGER :: io_buffer = 11
      DOUBLE PRECISION RHOBEG, RHOEND, F
    
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: IW
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: X, BL, BU, C
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: CL, CU, W
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATN, LINEAR

      DOUBLE PRECISION CPU( 4 ), CALLS( 4 )

      CHARACTER ( LEN = 10 ) :: PNAME
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: VNAME
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: CNAME

      PARAMETER      ( INPUT = 55, INDR = 46, IOUT = 6 )
      DOUBLE PRECISION BIGINF
      PARAMETER      ( BIGINF = 9.0D+19 )
      COMMON / FCOBFN /  BL, BU, CL, CU, MGEQ, MCON
C
C  Open the relevant file.
C

      OPEN ( INPUT, FILE = 'OUTSDIF.d', FORM = 'FORMATTED', 
     *       STATUS = 'OLD' )
      REWIND INPUT

C  allocate space

      CALL CUTEST_udimen( status, INPUT, N, MCON )
      IF ( status /= 0 ) GO TO 910

      lw = n * ( 3 * n + 2 * mcon + 11 ) + 4 * mcon + 6
      liw  = n + 1
      ALLOCATE( X( n ), BL( n ), BU( n ), C( mcon ), CL( mcon ), 
     *          CU( mcon ), IW( liw ), W( lw ), EQUATN( MCON ), 
     *          LINEAR( MCON ), VNAME( N ), CNAME( MCON ), 
     *          STAT = status )
      IF ( status /= 0 ) GO TO 990
C
C  Set up the data structures necessary to hold the problem functions.
C
      CALL CUTEST_csetup( status, INPUT, IOUT, io_buffer, n, mcon, X,
     *                     BL, BU, C, CL, CU, EQUATN, LINEAR, 1, 0, 0 )
      IF ( status /= 0 ) GO TO 910
      CLOSE ( INPUT )
C
C  Count the number of general equality constraints and ignore them by
C  shifting the remaining constraints at the beginning of the constraints' list
C
      MGEQ = 0
      DO 20 I = 1, MCON
         IF ( EQUATN( I ) ) MGEQ = MGEQ + 1
   20 CONTINUE
      IF ( MGEQ .GT. 0 ) THEN
         WRITE( 6, 3090 ) MGEQ
         DO 30 I = MCON, MGEQ + 1, -1
            CU( I - MGEQ ) = CU( I )
            CL( I - MGEQ ) = CL( I )
   30    CONTINUE
      END IF
      M = MCON - MGEQ
C
C  If constraints have both lower and upper bounds, they have to be
C  included twice!
C
      DO 40 I = 1, MCON - MGEQ
         IF ( CL( I ) .GT. - BIGINF .AND.
     *        CU( I ) .LT.   BIGINF ) M = M + 1
   40 CONTINUE
C
C  Include any simple bounds.
C
      NFIX = 0
      DO 50 I = 1, N
         IF ( BL( I ) .EQ.  BU( I ) ) THEN
            NFIX = NFIX + 1
         ELSE
            IF ( BL( I ) .GT. - BIGINF ) M = M + 1
            IF ( BU( I ) .LT.   BIGINF ) M = M + 1
         END IF
   50 CONTINUE
      IF ( NFIX .GT. 0 ) WRITE( 6, 3020 ) NFIX
C
C  Open the Spec file for the method.
C
      OPEN( INDR, FILE = 'COBYLA.SPC', FORM = 'FORMATTED', 
     *      STATUS = 'OLD')
      REWIND INDR
C
C  Read input Spec data.
C
C   RHOBEG = the size of the simplex initially
C   RHOEND = the size of the simplex at termination
C   MAXFUN = the maximum number of function calls allowed.
C   IPRINT   should be set to 0, 1, 2 or 3, it controls the amount of printing.
C
C  Set up algorithmic input data.
C
      READ ( INDR, 1000 ) RHOBEG, RHOEND, MAXFUN, IPRINT
      CLOSE ( INDR )
C
C  Evaluate the objective function and constraints.
C
      CALL CALCFC( N, X, F, C )
C
C  Perform the minimization.
C
      CALL COBYLA( N, M, X, RHOBEG, RHOEND, IPRINT, MAXFUN, W, IW)
      CALL CUTEST_creport( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910
C
      CALL CUTEST_cnames( status, N, MCON, PNAME, VNAME, CNAME )
      IF ( status /= 0 ) GO TO 910
      CALL CALCFC( N, X, F, C )
      WRITE( 6, 2110 ) ( I, VNAME( I ), X( I ), BL( I ), BU( I ),
     *                      I = 1, N )
      IF ( MCON .GT. 0 ) WRITE( 6, 2120 ) ( I, CNAME( I ), C( I ),
     *     CL( I ), CU( I ), LINEAR( I ), I = 1, MCON )
      WRITE( 6, 2000 ) PNAME, N, MCON, CALLS(1), CALLS(5), F,
     *     CPU(1), CPU(2)
      STOP

  910 CONTINUE
      WRITE( 6, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *  status
      STOP

  990 CONTINUE
      WRITE( 6, "( ' Allocation error, status = ', I0 )" ) status
      STOP
C
C  Non-executable statements
C
 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *    ,' Code used               :  COBYLA ',  /
     *    ,' Problem                 :  ', A10,    /
     *    ,' # variables             =      ', I10 /
     *    ,' # constraints           =      ', I10 /
     *    ,' # objective functions   =        ', F8.2 /
     *    ,' # constraints functions =        ', F8.2 /
     *    ,' Final f                 = ', E15.7 /
     *    ,' Set up time             =      ', 0P, F10.2, ' seconds' /
     *     ' Solve time              =      ', 0P, F10.2, ' seconds' //
     *     66('*') / )
 1000 FORMAT( D12.4, /, D12.4, /,I6, /, I6 )
 2110 FORMAT( /, ' The variables:', /,
     *        '     I name          value    lower bound upper bound',
     *        /, ( I6, 1X, A10, 1P, 3D12.4 ) )
 2120 FORMAT( /, ' The constraints:', /,
     *        '     I name          value    lower bound upper bound',
     *        ' linear? ',
     *        /, ( I6, 1X, A10, 1P, 3D12.4, 5X, L1 ) )
 3000 FORMAT( /,'  ** Program CSETUP: array length ', A6, ' too small.',
     *        /,'  -- Miminimization abandoned.',
     *        /,'  -- Increase the parameter ', A6, ' by at least ', I8,
     *          ' and restart.'  )
 3020 FORMAT( /,'  ** Warning from COBMA. **',
     *        /,'     In the problem as stated , ', I6,
     *          ' variables are fixed: they are changed to free.' )
 3090 FORMAT( /,'  ** Warning from COBMA. **',
     *        /,'     The problem as stated includes ', I6,
     *          ' equality constraints: they are ignored ' )
C
C  End of COBMA.
C
      END
      SUBROUTINE CALCFC( N, X, F, C )
C
C  Evaluates the objective function value in a format compatible with COBYLA,
C  but using the CUTEst tools.
C
C  A. R. Conn and Ph. Toint
C  January 1995.
C
      INTEGER            N, MGEQ, MCON, I, MT
      DOUBLE PRECISION   F, X( N ), C( MCON ), BL( n ), BU( n )
      DOUBLE PRECISION   CL( MCON ), CU( MCON )
      COMMON /FCOBFN/    BL, BU, CL, CU, MGEQ, MCON
C
      DOUBLE PRECISION   BIGINF
      PARAMETER        ( BIGINF = 9.0D+19 )
C
C  Evaluate the objective function and constraints.
C
      CALL CUTEST_cfn( status, N, MCON, X, F, C )
      IF ( status /= 0 ) GO TO 910
C
C  If there are equality constraints, ignore them
C  and shift all the inequality constraint values.
C
      IF ( MGEQ .GT. 0 ) THEN
         DO 10 I = MCON, MGEQ + 1, - 1
            C( I - MGEQ ) = C( I )
   10    CONTINUE
      END IF
C
C  If constraints have both lower and upper bounds, they have to
C  be included twice! Reverse the signs of less-than-or-equal-to
C  constraints.
C
      MT = MCON - MGEQ + 1
      DO 40 I = 1, MCON - MGEQ
         IF ( CL( I ) .GT. - BIGINF .AND.
     *        CU( I ) .LT.   BIGINF ) THEN
            C( I )  = CU( I ) - C( I )
            C( MT ) = C( I ) - CL( I )
            MT      = MT + 1
         ELSE IF ( CL( I ) .GT. - BIGINF ) THEN
            C( I )  = C( I ) - CL( I )
         ELSE IF ( CU( I ) .LT.   BIGINF ) THEN
            C( I )  = CU( I ) - C( I )
         END IF
   40 CONTINUE
C
C  Include any simple bounds, including fixed variables.
C
      DO 50 I = 1, N
         IF ( BL( I ) .NE.  BU( I ) ) THEN
            IF ( BL( I ) .GT. - BIGINF ) THEN
               C( MT ) = X( I ) - BL( I )
               MT      = MT + 1
            END IF
            IF ( BU( I ) .LT.   BIGINF ) THEN
               C( MT ) = BU( I ) - X( I )
               MT      = MT + 1
            END IF
         END IF
   50 CONTINUE
      RETURN

  910 CONTINUE
      WRITE( 6, "( ' CUTEst error, status = ', i0, ', stopping' )") status
      STOP

C
C  End of CALCFC.
C
      END

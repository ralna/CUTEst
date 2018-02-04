C     ( Last modified on 6 Jan 2013 at 16:20:00 )

      PROGRAM NITSOL_main

C  ------------------------------------------------------
C
C  Solve a system of nonlinear equations using 
C  Homer Walker's package NITSOL version 0.3 
C  ( ftp://ephraim.wpi.edu/pub/nitsol/nitsol.tar.gz )
C
C  CUTEst interface: Nick Gould
C  June 2003
C  CUTEst evolution January 2013
C
C  ------------------------------------------------------

      INTEGER :: lipar, lrpar, nwork, i, nn, mm, m, n, iterm, status
      INTEGER :: nfree, nnimax, ijacv, ikrysl, kdmax, irpre
      INTEGER :: iksmax, iresup, ifdord, ibtmax, ieta, ipsol
      INTEGER, PARAMETER :: input = 55, out = 6, inspec = 46
      INTEGER, PARAMETER :: io_buffer = 11
      INTEGER :: NINPUT( 10 ), INFO( 6 )
      DOUBLE PRECISION ftol, stptol, final, f
      DOUBLE PRECISION, PARAMETER :: zero = 0.0D+0
      DOUBLE PRECISION, PARAMETER :: biginf = 9.0D+19
      DOUBLE PRECISION :: CPU( 2 ), CALLS( 7 )
      CHARACTER ( LEN = 10 ) :: pname
      LOGICAL :: bound, inequality
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: IPAR
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: X, WORK
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: RPAR, XFREE
      DOUBLE PRECISION NITSOL_dot, NITSOL_norm2
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: VNAME
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: CNAME
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATN, LINEAR
      EXTERNAL :: NITSOL_evalf, NITSOL_evalj
      EXTERNAL :: NITSOL_evalfn, nitsol_evaljn
      EXTERNAL :: NITSOL_dot, NITSOL_norm2

C  NITSOL printing common block (see nitprint.h)

      INTEGER IPLVL, IPUNIT
      COMMON / NITPRINT / IPLVL, IPUNIT

C  NITSOL info common block (see nitprint.h)

      INTEGER INSTEP, NEWSTEP, KRYSTAT
      DOUBLE PRECISION AVRATE, FCURNRM, ETA
      COMMON / NITINFO / AVRATE, FCURNRM, ETA, INSTEP, NEWSTEP, KRYSTAT

C  common block to tell when to evluate Jacobian again

      LOGICAL JKNOWN
      COMMON / NITJEV / JKNOWN
      JKNOWN = .FALSE.

C  open the Spec file for the method.

      OPEN ( INSPEC, FILE = 'NITSOL.SPC', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND INSPEC

C  Read input Spec data.
C
C  NNIMAX = maximum number of nonlinear iterations 
C  IJACV  = the method of J*v evaluation 
C          (0 => finite differences, 1 => analytic)
C  IKRYSL = the Krylov solver: 
C          (0 => GMRES 1 => BiCGSTAB, 2 => TFQMR)
C  KDMAX  = max Krylov subspace dimension when GMRES is used 
C  IRPRE = flag for right preconditioning: 
C          ( 0 => no right preconditioning, 1 => right preconditioning)
C  IKSMAX = max allowable number iterations per call to Krylov solver
C  IRESUP = residual update flag when GMRES is used
C           ( 0 => linear combination, 1 => direct evaluation)
C  IFDORD = order of the finite-difference formula (sometimes) used 
C  IBTMAX = maximum allowable number of backtracks per linesearch
C  IETA   = flag determining the forcing term eta
C           ( 0 => abs( ||fcur|| - ||fprev+Jprev*sprev|| )/||fprev||,
C             1 => (||fcur||/||fprev||)**2,
C             2 => gamma*(||fcur||/||fprev||)**alpha 
C             3 => fixed (constant) eta in (0,1) )
C  IPLVL  = printlevel 
C           ( 0 => no printout, 
C             1 => iteration numbers and F-norms,
C             2 => ... + some stats, step norms, and linear model norms
C             3 => ... + some Krylov solver and backtrack information
C             4 => ... + more Krylov solver and backtrack information)
C  IPUNIT = printout unit number
C  IPSOL  = print solution to output channel
C           ( 0 => no, 1=> yes )
C  FTOL   = stopping tolerance on the f-norm
C  STPTOL = stopping tolerance on the steplength

      READ( INSPEC, "( I10, 12( /, I10 ), 2( /, 1P, D10.3 ) )" ) 
     *   NNIMAX, IJACV, IKRYSL, KDMAX, IRPRE, IKSMAX, IRESUP, 
     *   IFDORD, IBTMAX, IETA, IPLVL, IPUNIT, IPSOL, FTOL, STPTOL

C  assign values

      NINPUT(  1 ) = NNIMAX
      NINPUT(  2 ) = IJACV
      NINPUT(  3 ) = IKRYSL
      NINPUT(  4 ) = KDMAX
      NINPUT(  5 ) = IRPRE
      NINPUT(  6 ) = IKSMAX
      NINPUT(  7 ) = IRESUP
      NINPUT(  8 ) = IFDORD
      NINPUT(  9 ) = IBTMAX
      NINPUT( 10 ) = IETA

C  close input file

      CLOSE ( INSPEC )

C  open the relevant file.

      OPEN ( INPUT, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND INPUT

C  determine the number of variables and constraints

      CALL CUTEST_cdimen( status, input, n, m )
      IF ( status /= 0 ) GO TO 910

C  allocate space 

      IF ( NINPUT( 3 ) .EQ. 1 ) THEN
         nwork = N * ( NINPUT( 4 ) + 5 ) + 
     *        NINPUT( 4 ) * ( NINPUT( 4 ) + 3  )
      ELSE IF ( NINPUT( 3 ) .EQ. 2 ) THEN
         nwork = 11 * n
      ELSE
         nwork = 14 * n
      END IF
      lipar = n + 2
      lrpar = 2 * n
      ALLOCATE( IPAR( lipar ), X( n ), WORK( nwork ), RPAR( lrpar ), 
     *          XFREE( n ), EQUATN( m ), LINEAR( m ), VNAME( n ), 
     *          CNAME( m ), STAT = status )
      IF ( status /= 0 ) GO TO 990

C  set up the data structures necessary to hold the group partially
C  separable function.

      nn = n
      mm = m
      CALL CUTEST_csetup( status, INPUT, out, io_buffer, n, m, 
     *             X, WORK( 1 ), WORK( nn + 1 ), 
     *             WORK( 2 * nn + 2 * mm + 1 ), WORK( 2 * nn + 1 ),
     *             WORK( 2 * nn + mm + 1 ), EQUATN, LINEAR, 0, 0, 0 )
      IF ( status /= 0 ) GO TO 910

C  determine the names of the problem, variables and constraints

      CALL CUTEST_cnames( status, N, M, PNAME, VNAME, CNAME )
      IF ( status /= 0 ) GO TO 910

C  check that there are no variable bounds

      bound = .FALSE.
      nfree = 0
      DO 10 i = 1, n
        IF ( WORK( i ) .GT. - biginf .OR. 
     *    WORK( n + i ) .LT. biginf ) THEN
          IF ( WORK( i ) .NE. WORK( n + i ) ) THEN
             bound = .TRUE.
C            WRITE( out, "( ' Variable ', A10, ' is bounded ',
C    *                    /, ' so NITSOL is not appropriate ' )" )  
C    *          VNAME( i )
C           STOP
          END IF
        ELSE
          nfree = nfree + 1
        END IF
   10 CONTINUE
      IF ( bound ) WRITE( out, "( ' Warning: there are bounded',
     *  ' variables. Bounds ignored.' )" )

C  check that all constraints are equalities

      inequality = .FALSE.
      DO 20 i = 1, m
        IF ( WORK( 2 * n + i ) .NE.  WORK( 2 * n + m + i ) ) THEN
          inequality = .TRUE.
C         WRITE( out, "( ' Constraint ', A10, ' is an inequality ',
C    *                   /, ' so NITSOL is not appropriate ' )" )  
C    *       CNAME( i )
C         STOP
        END IF
   20 CONTINUE
      IF ( inequality ) WRITE( out, "( ' Warning: there are inequality',
     *  ' constraints. Inequalities ignored.' )" )

C  check that the system is "square"

      IF ( nfree .NE. m ) THEN
         WRITE( out, "( ' n = ', I10, ' /= ', ' m = ', I10,
     *      /, ' so NITSOL is not appropriate ' )" ) nfree, m    
         STOP
      END IF

C  solve the problem - no fixed variable case

      IF ( nfree .EQ. n ) THEN
        CALL NITSOL( n, X, NITSOL_evalf, NITSOL_evalj, ftol, stptol, 
     *               NINPUT, INFO, WORK, RPAR, IPAR, iterm, NITSOL_dot, 
     *               NITSOL_norm2 )

C  fixed variable case

      ELSE
        IPAR( 1 ) = n
        IPAR( 2 ) = m
        nfree = 0
        DO 30 I = 1, N
          IF ( WORK( I ) .NE. WORK( N + I ) ) THEN
            nfree = nfree + 1
            IPAR( nfree + 2 ) = i
            XFREE( nfree ) = X( i )
          ELSE
            X( i ) = WORK( i )
            RPAR( i ) = X( i )
            RPAR( n + i ) = zero
          END IF
   30   CONTINUE
        CALL NITSOL( nfree, XFREE, NITSOL_evalfn, NITSOL_evaljn, FTOL, 
     *               STPTOL, NINPUT, INFO, WORK, RPAR, IPAR, ITERM, 
     *               NITSOL_dot, NITSOL_norm2 )
        DO 40 i = 1, nfree
          X( IPAR( i + 2 ) ) = XFREE( i )
   40   CONTINUE
      END IF

C  write results

      CALL CUTEST_creport( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910
      CALL CUTEST_cfn( status, n, m, X, F, WORK )
      IF ( status /= 0 ) GO TO 910
      final = NITSOL_norm2( M, WORK, 1 )

      WRITE( out, "( /, ' Termination flag iterm:       ', I9,
     *             /, ' Final f-norm:                 ', 1P, E9.3,
     *             /, ' No. function evaluations:     ', I9, 
     *             /, ' No. J*v evaluations:          ', I9,
     *             /, ' No. P(inverse)*v evaluations: ', I9, 
     *             /, ' No. linear iterations:        ', I9, 
     *             /, ' No. nonlinear iterations:     ', i9,
     *             /, ' No. backtracks:               ', i9 )" )
     *      iterm, final, INFO( 1 ), INFO( 2 ), INFO( 3 ), 
     *      INFO( 4 ), INFO( 5 ), INFO( 6 )
      IF ( ipsol .GT. 0 ) THEN
        WRITE( out, "( /, ' the variables:', /,
     *          '     I name          value',
     *          /, ( I6, 1X, A10, 1P, D12.4 ) )" )
     *       ( i, VNAME( i ), X( i ), i = 1, n )
        WRITE( out, "( /, ' the constraints:', /,
     *          '     I name          value',
     *          /, ( I6, 1X, A10, 1P, D12.4 ) )" )
     *     (   i, CNAME( i ), WORK( i ), i = 1, m )
      END IF
      WRITE( 6, "( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *    ,' Code used               :  NITSOL',     /
     *    ,' Problem                 :  ', A10,    /
     *    ,' # variables (inc.fixed) =      ', I10 /
     *    ,' # equations             =      ', I10 /
     *    ,' # objective functions   =        ', F8.2 /
     *    ,' # objective gradients   =        ', F8.2 / 
     *    ,' # constraints functions =        ', F8.2 /
     *    ,' # constraints gradients =        ', F8.2 /
     *    ,' Final f                 = ', E15.7 /
     *    ,' Set up time             =      ', 0P, F10.2, ' seconds' /
     *     ' Solve time              =      ', 0P, F10.2, ' seconds' //
     *     65('*') / )" )
     *  pname, n, m, CALLS( 1 ), CALLS( 2 ), 
     *  CALLS( 5 ), CALLS( 6 ), FINAL, CPU( 1 ), CPU( 2 )
      CLOSE( INPUT  )
      STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP

      END

C  function and Jacobian-vector product  evaluation subroutines

      SUBROUTINE NITSOL_evalf( n, X, C, RPAR, IPAR, ITRMF )
      INTEGER N, ITRMF
      INTEGER IPAR( * )
      DOUBLE PRECISION f, X( n ), C( n ), RPAR( * )
      LOGICAL jknown
      COMMON / NITJEV / jknown
      INTEGER :: status
      INTEGER, PARAMETER :: out = 6
      CALL CUTEST_cfn( status, n, n, X, f, C )
      IF ( status .NE. 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *     status
        STOP
      END IF
      jknown = .FALSE.
      itrmf = 0
      RETURN
      END

      SUBROUTINE NITSOL_evalj( n, X, C, ijob, V, Z, RPAR, IPAR, itrmjv )
      INTEGER n, ijob, itrmjv
      INTEGER IPAR( * )
      DOUBLE PRECISION X( N ), C( N ), V( N ), Z( N ), RPAR( * )
      LOGICAL jknown
      COMMON / NITJEV / jknown
      INTEGER :: status
      INTEGER, PARAMETER :: out = 6
      IF ( ijob .EQ. 0 ) THEN
        CALL CUTEST_cjprod( status, N, N, JKNOWN, .FALSE., 
     *                      X, V, n, Z, n )
        IF ( status .NE. 0 ) THEN
          WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping')")
     *       status
          STOP
        END IF
        jknown = .TRUE.
        itrmjv = 0
      ELSE
        itrmjv = 2
      END IF
      RETURN
      END

      SUBROUTINE NITSOL_evalfn( nfree, XFREE, C, RPAR, IPAR, itrmf )
      INTEGER nfree, itrmf
      INTEGER IPAR( * )
      DOUBLE PRECISION f, XFREE( nfree ), C( nfree ), RPAR( * )
      INTEGER n, m, i, status
      LOGICAL jknown
      COMMON / NITJEV / jknown
      INTEGER, PARAMETER :: out = 6
      n = IPAR( 1 )
      m = IPAR( 2 )
      DO 10 i = 1, nfree
        RPAR( IPAR( i + 2 ) ) = XFREE( i )
   10 CONTINUE
      CALL CUTEST_cfn( status, n, m, RPAR( 1 ), f, C )
      IF ( status .NE. 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping')") 
     *     status
        STOP
      END IF
      jknown = .FALSE.
      itrmf = 0
      RETURN
      END

      SUBROUTINE NITSOL_evaljn( nfree, XFREE, C, ijob, VFREE, Z, RPAR, 
     *                   IPAR, itrmjv )
      INTEGER nfree, ijob, itrmjv
      INTEGER IPAR( * )
      DOUBLE PRECISION XFREE( nfree ), C( nfree ), VFREE( nfree ), 
     *                 Z( nfree ), RPAR( * )
      INTEGER i, n, m, status
      INTEGER, PARAMETER :: out = 6
      LOGICAL jknown
      COMMON / NITJEV / jknown
      IF ( ijob .EQ. 0 ) THEN
        n = IPAR( 1 )
        m = IPAR( 2 )
        DO 10 i = 1, nfree
          RPAR( IPAR( i + 2 ) ) = XFREE( i )
          RPAR( n + IPAR( i + 2 ) ) = VFREE( i )
   10   CONTINUE
        CALL CUTEST_cjprod( status, n, m, jknown, .FALSE., RPAR( 1 ), 
     *                      RPAR( n + 1 ), n, Z, m )
        IF ( status .NE. 0 ) THEN
          WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping')") 
     *       status
          STOP
        END IF
        jknown = .TRUE.
        itrmjv = 0
      ELSE
        itrmjv = 2
      END IF
      RETURN
      END


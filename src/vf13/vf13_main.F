! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      PROGRAM VF13MA

C  VF13 test driver for problems derived from SIF files.
C
C  Nick Gould, for CGT Productions
C  CUTE version July 1991
C  CUTEst evolution January 2013

      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: inf, m, n, maxfun, mcon, lcn, meq, lw
      INTEGER ( KIND = ip_ ) :: i, j, mgeq, mmax, status, liw, iprint
      INTEGER ( KIND = ip_ ), PARAMETER :: input = 55, out = 6
      LOGICAL :: firstg, debug
      CHARACTER ( LEN = 10 ) :: pname
      REAL ( KIND = rp_ ) :: f, acc
      REAL ( KIND = rp_ ), PARAMETER :: accreq = 1.0E-7_rp_
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 7 )
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IW
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, BL, BU, G
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: C, CL, CU, W
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( :, : ) :: CN
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: VNAME
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: CNAME
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATN, LINEAR

C     DEBUG = .TRUE.
      DEBUG = .FALSE.

C  open the relevant data input file

      OPEN( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *      STATUS = 'OLD' )
      REWIND( input )

C  compute problem dimensions

      CALL CUTEST_cdimen_r( status, input, n, m )
      IF ( status /= 0 ) GO TO 910

C  allocate space

      mmax = 2 * ( n + m )
      lcn = n + 1
      lw = 5 * n * n / 2 + 43 * n / 2 + 16 * mmax + 14
      liw  = n + 1
      ALLOCATE( IW( liw ), X( n ), BL( n ), BU( n ), G( n ), C( mmax ),
     *          CL( mmax ), CU( mmax ), CN( lcn, mmax ), W ( lw ),
     *          EQUATN( mmax ), LINEAR( mmax ), VNAME( n ),
     *          CNAME( mmax ), STAT = status )
      IF ( status /= 0 ) GO TO 990

C  set up the data structures necessary to hold the problem functions

      CALL VF13SE( input, out, n, m, mgeq, meq, mcon, X, BL, BU, n,
     *             EQUATN, LINEAR, C, CL, CU, mmax )
      IF ( DEBUG ) THEN
         WRITE( 6, 2030 ) ( i, X( i ), BL( i ), BU( i ), i = 1, n )
         IF ( mcon .GT. 0 ) WRITE( 6, 2060 ) ( i, C( i ), CL( i ),
     *        CU( i ), EQUATN( i ), LINEAR( i ), i = 1, mcon )
      END IF

C  set up algorithmic input data

      maxfun = 1000
      iprint = 0
      inf = - 1
      acc = accreq
      firstg = .TRUE.

C  start of main iteration loop

   10 CONTINUE

C  evaluate the objective function and constraints

      CALL VF13FN( n, mgeq, meq, mcon, X, f, m, C, BL, BU, CL, CU )
      IF ( debug ) THEN
         WRITE( 6, 2010 ) f
         IF ( m .GT. 0 ) WRITE( 6, 2070 ) ( i, C( i ), i = 1, m )
      END IF

c  Evaluate the gradient of the objective and constraint functions

      CALL VF13GR( n, mgeq, meq, mcon, X, m, C,
     *             G, lcn, m, CN, BL, BU, CL, CU, firstg )
      IF ( debug ) THEN
         WRITE( 6, 2080 )
         WRITE( 6, 2020 ) ( i, G( i ), i = 1, n )
         DO 11 j = 1, m
            WRITE( 6, 2090 ) j
            WRITE( 6, 2020 ) ( i, CN( i, j ), i = 1, n )
   11    CONTINUE
      END IF

C  perform another iteration of the minimization

      CALL VF13AD( n, m, meq, X, f, G, C, CN, lcn, maxfun,
     *             acc, iprint, inf, W, lw, IW )
      IF ( INF .EQ. 0 ) GO TO 10
      CALL CUTEST_creport_r( status, CALLS, CPU )
      CALL CUTEST_cnames_r( status, N, M, PNAME, VNAME, CNAME )
      WRITE( 6, 2110 ) f, ( i, VNAME( i ), X( i ), BL( i ), BU( i ),
     *                      i = 1, n )
      IF ( mcon .GT. 0 ) WRITE( 6, 2120 ) ( i, CNAME( i ), C( i ),
     *     CL( i ), CU( i ), EQUATN( i ), LINEAR( i ), i = 1, mcon )
      WRITE ( 6, 2000 ) pname, n, mcon, CALLS( 1 ), CALLS( 2 ),
     *                  CALLS( 5 ), CALLS( 6 ), F, CPU( 1 ), CPU( 2 )
      STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )")
     *   status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP

 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *,' Package used            :  VF13',     /
     *,' Problem                 :  ', A10,    /
     *,' # variables             =      ', I10 /
     *,' # constraints           =      ', I10 /
     *,' # objective functions   =        ', F8.2 /
     *,' # objective gradients   =        ', F8.2 /
     *,' # constraints functions =        ', F8.2 /
     *,' # constraints gradients =        ', F8.2 /
     *,' Final f                 = ', E15.7 /
     *,' Set up time             =      ', 0P, F10.2, ' seconds' /
     *     ' Solve time              =      ', 0P, F10.2, ' seconds' //
     *     65('*') / )
 2010 FORMAT( /, ' Objective function value is ', 1P, D22.14 )
 2020 FORMAT( /, '     i      GRAD', /, ( I6, 1P, D12.4 ) )
 2030 FORMAT( /, ' After projection, the starting point:',
     *        /, '     i      X          BL          BU', /,
     *        ( I6, 1P, 3D12.4 ) )
 2060 FORMAT( /, ' the constraints:', /,
     *         '     i  MULTIPLIER     CL          CU      equality? ',
     *       '  linear? ', /, ( I6, 1P, 3D12.4, 5X, L1, 10X, L1 ) )
 2070 FORMAT( /, ' the constraint values are:',
     *        /, '     I      C ', /, ( I6, 1P, D12.4 ) )
 2080 FORMAT( /, ' Objective function ' )
 2090 FORMAT( /, ' Constraint ', I6 )
 2110 FORMAT( /, ' the objective function value: ', 1P, D12.4, /,
     *        /, ' the variables:', /,
     *        '     I name          value    lower bound upper bound',
     *        /, ( I6, 1X, A10, 1P, 3D12.4 ) )
 2120 FORMAT( /, ' the constraints:', /,
     *        '     I name          value    lower bound upper bound',
     *        ' equality?   linear? ',
     *        /, ( I6, 1X, A10, 1P, 3D12.4, 5X, L1, 10X, L1 ) )

C  End of VF13MA

      END

      SUBROUTINE VF13SE( input, out, n, m, mgeq, meq,
     *                   mcon, X, BL, BU, nmax, EQUATN,
     *                   LINEAR, V, CL, CU, mmax  )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: input, out, n, m, mgeq, meq, mcon
      INTEGER ( KIND = ip_ ) :: nmax, mmax
      REAL ( KIND = rp_ ) :: X( nmax ), BL( nmax ), BU( nmax )
      REAL ( KIND = rp_ ) :: V( mmax ), CL( mmax ), CU( mmax )
      LOGICAL :: EQUATN( mmax ), LINEAR( mmax )

C  Set up the input data for the the VF13 minimizer.

C  Nick Gould, for CGT productions,
C  7th November, 1991.

      INTEGER ( KIND = ip_ ) :: i, status
      INTEGER ( KIND = ip_ ), PARAMETER :: io_buffer = 11
      REAL ( KIND = rp_ ), PARAMETER :: biginf = 9.0E+19_rp_

C  Set up the data structures necessary to hold the problem functions.

      CALL CUTEST_csetup_r( status, input, out, io_buffer, n, m,
     *                    X, BL, BU, V, CL, CU, EQUATN, LINEAR,
     *                    1, 0, 0 )
      IF ( status /= 0 ) GO TO 910

C  Count the number of general equality constraints.

      mcon = m
      mgeq = 0
      DO 20 i = 1, m
         IF ( EQUATN( i ) ) mgeq = mgeq + 1
   20 CONTINUE
C     IF ( m .GT. 0 ) WRITE( 6, 2010 ) ( i, V( i ), CL( i ), CU( i ),
C    *                             EQUATN( i ), LINEAR( i ), i = 1, m )
      meq = mgeq
C
C  If constraints have both lower and upper bounds, they have to be
C  included twice!
C
      DO 40 i = mgeq + 1, mcon
         IF ( CL( i ) .GT. - biginf .AND.
     *        CU( i ) .LT. biginf ) m = m + 1
   40 CONTINUE
C
C  Include any simple bounds.
C
      DO 50 i = 1, n
        IF ( BL( i ) .EQ.  BU( i ) ) THEN
          meq = meq + 1
          m = m + 1
        ELSE
          IF ( BL( I ) .GT. - biginf ) m = m + 1
          IF ( BU( I ) .LT.   biginf ) m = m + 1
        END IF
   50 CONTINUE
      IF ( m .GT. mmax ) THEN
         IF ( out .GT. 0 )
     *      WRITE( out, 2000 ) 'EQUATN', 'MMAX  ', m - mmax
         STOP
      END IF
      RETURN

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )")
     *   status
      STOP
C
C  Non-executable statements.
C
 2000 FORMAT( /, ' ** Program CUTEst_csetup: array length ', A6,
     *           ' too small.', /, ' -- Miminimization abandoned.',
     *        /, ' -- Increase the parameter ', A6, ' by at least ', I8,
     *           ' and restart.'  )
C2010 FORMAT( /, ' the constraints:', /,
C    *         '     I  MULTIPLIER     CL          CU      EQUALITY? ',
C    *       '  LINEAR? ', /, ( I6, 1P, 3D12.4, 5X, L1, 10X, L1 ) )
C
C  End of VF13SE.
C
      END
C
      SUBROUTINE VF13FN( n, mgeq, meq, mcon, X, f, lc,
     *                   C, BL, BU, CL, CU )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n, mgeq, meq, mcon, lc
      REAL ( KIND = rp_ ) ::  f
      REAL ( KIND = rp_ ) ::  X( n ), BL( n ), BU( n )
      REAL ( KIND = rp_ ) ::  C( lc ), CL( lc ), CU( lc )
C
C  Evaluate the objective function and constraints.
C
C  Nick Gould, for CGT productions.
C  November 1991.
C
      INTEGER ( KIND = ip_ ) :: i, mt, mfixed, mfixva, status
      INTEGER ( KIND = ip_ ), PARAMETER :: out = 6
      REAL ( KIND = rp_ ), PARAMETER :: biginf = 9.0E+19_rp_

      CALL CUTEST_cfn_r( status, n, mcon, X, f, C )
      IF ( status /= 0 ) GO TO 910
C
C  If there are fixed variables, shift all the inequality constraint values.
C
      mfixed = meq - mgeq
      mfixva = mgeq
      IF ( mfixed .GT. 0 ) THEN
         DO 10 i = mcon, mgeq + 1, - 1
            C( i + mfixed ) = C( i )
   10    CONTINUE
      END IF
C
C  If constraints have both lower and upper bounds, they have to
C  be included twice! Reverse the signs of less-than-or-equal-to
C  constraints.
C
      mt = mcon + mfixed
      DO 40 i = mgeq + 1, mcon
         IF ( CL( i ) .GT. - biginf .AND.
     *        CU( i ) .LT.   biginf ) THEN
            mt = mt + 1
            C( mt ) = CU( i ) - C( i + mfixed )
            C( i  + mfixed ) = C( i + mfixed ) - CL( i )
         ELSE IF ( CL( i ) .GT. - biginf ) THEN
            C( i + mfixed )  = C( i + mfixed ) - CL( i )
         ELSE IF ( CU( i ) .LT.   biginf ) THEN
            C( i + mfixed )  = CU( i ) - C( i + mfixed )
         END IF
   40 CONTINUE
C
C  Include any simple bounds, including fixed variables.
C
      DO 50 i = 1, N
         IF ( BL( i ) .EQ.  BU( i ) ) THEN
            mfixva = mfixva + 1
            C( mfixva ) = X( i ) - BL( i )
         ELSE
            IF ( BL( i ) .GT. - biginf ) THEN
               mt = mt + 1
               C( mt  ) = X( i ) - BL( i )
            END IF
            IF ( BU( i ) .LT.   biginf ) THEN
               mt = mt + 1
               C( mt ) = BU( i ) - X( i )
            END IF
         END IF
   50 CONTINUE
      RETURN

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )")
     *   status
      STOP

C  End of VF13FN.

      END

      SUBROUTINE VF13GR( n, mgeq, meq, mcon, X, lv, V, G,
     *                   lcn, mmax, CN, BL, BU, CL, CU, firstg )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n, mgeq, meq, mcon, lv, lcn, mmax
      LOGICAL :: firstg
      REAL ( KIND = rp_ )   X( n ), G( n ), V( lv ), CN( lcn, mmax )
      REAL ( KIND = rp_ )   BL( n ), BU( n)
      REAL ( KIND = rp_ )   CL( mmax ), CU( mmax )
C
C  Evaluate the gradient of the objective and constraint functions.
C
C  Nick Gould, for CGT productions,
C  November 1991.
C
      INTEGER ( KIND = ip_ ) :: i, j, mt, mfixed, mfixva, status
      INTEGER ( KIND = ip_ ), PARAMETER :: out = 6
      REAL ( KIND = rp_ ), PARAMETER :: biginf = 9.0E+19_rp_

C  Evaluate the gradient of the objective and constraint functions
C  at the initial point in a dense format.

      CALL CUTEST_cgr_r( status, N, mcon, X, V, .FALSE., G,
     *                 .TRUE., lcn, mmax, CN )
      IF ( status /= 0 ) GO TO 910

C  If there are fixed variables, shift all the  gradients of the
C  inequality constraints.

      mfixed = meq - mgeq
      mfixva = mgeq
      IF ( mfixed .GT. 0 ) THEN
         DO 20 i = mcon, mgeq + 1, - 1
            DO 10 j = 1, n
               CN( j, i + mfixed ) = CN( j, i )
   10       CONTINUE
   20    CONTINUE
      END IF

C  If constraints have both lower and upper bounds, their gradients
C  have to be included twice! Reverse the signs of less-than-or-equal-
C  -to constraints.

      mt      = mcon + mfixed
      DO 50 i = mgeq + 1, mcon
         IF ( CL( i ) .GT. - biginf .AND.
     *        CU( i ) .LT.   biginf ) THEN
            mt = mt + 1
            DO 30 j = 1, n
               CN( j, mt ) = - CN( j, i + mfixed )
   30       CONTINUE
         ELSE IF ( CU( i ) .LT.   biginf ) THEN
           DO 40 j = 1, n
               CN( j, i + mfixed ) = - CN( j, i + mfixed )
   40       CONTINUE
         END IF
   50 CONTINUE

C  Include the gradients of any simple bounds, including fixed variables

      IF ( firstg .OR. mfixed .GT. 0 ) THEN
         DO 90 i = 1, n
            IF ( BL( i ) .EQ. BU( i ) ) THEN
               mfixva  = mfixva + 1
               DO 60 j = 1, n
                  CN( j, mfixva ) = 0.0_rp_
   60          CONTINUE
               CN( i, mfixva ) = 1.0_rp_
            ELSE
               IF ( FIRSTG ) THEN
                  IF ( BL( i ) .GT. - biginf ) THEN
                     mt = mt + 1
                     DO 70 J = 1, N
                        CN( j, mt ) = 0.0_rp_
   70                CONTINUE
                     CN( i, mt ) = 1.0_rp_
                  END IF
                  IF ( BU( i ) .LT. biginf ) THEN
                     mt = mt + 1
                     DO 80 J = 1, N
                        CN( j, mt ) = 0.0_rp_
   80                CONTINUE
                     CN( i, mt ) = - 1.0_rp_
                  END IF
               END IF
            END IF
   90    CONTINUE
         FIRSTG = .FALSE.
      END IF
      RETURN

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )")
     *   status
      STOP

C  End of VF13GR.

      END

! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      PROGRAM NPSOL_main

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     Driver for running NPSOL Version 4.06 on CUTEst problems.
C
C     May 1993. Peihuang Chen
C     modified September 1993. Ingrid Bongartz
C     CUTEst evolution January 2013, Nick Gould
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      USE CUTEST_KINDS_precision

C  Set up parameters, variables and arrays required by constrained tools

      INTEGER ( KIND = ip_ ), PARAMETER :: input = 55, out = 6
      INTEGER ( KIND = ip_ ), PARAMETER :: io_buffer = 11
      INTEGER ( KIND = ip_ ) :: n, m, ldcj, ldr, liwork, lwork, npm
      INTEGER ( KIND = ip_ ) :: i, ib, ic, j, iter, status, lda
      INTEGER ( KIND = ip_ ) :: ioptns, iprint, nclin, ncnln, inform
      REAL ( KIND = rp_ ) :: f
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 7 )
      LOGICAL :: debug
      CHARACTER ( LEN= 10 ) :: cbgbnd
      CHARACTER * 10   pname
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IWORK
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ISTATE
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, BL, BU, G
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: Y, CL, CU, C
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: BLOWER, BUPPER
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: CLAMBDA, WORK
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : , : ) :: A, R, CJAC
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATN, LINEAR
      external :: NPSOL_evalfg, NPSOL_evalcj
      LOGICAL :: fdgrad
      COMMON / FDG   / fdgrad

C  and dimension IPADNP and IPSVNP only once

      INTEGER ( KIND = ip_ ), PARAMETER :: mxparm = 30
      INTEGER ( KIND = ip_ ) :: idbgnp, ldbgnp, itmxnp, nlnf, nlnj, nlnx
      INTEGER ( KIND = ip_ ) :: jvrfy1, jvrfy2, jvrfy3, jvrfy4
      INTEGER ( KIND = ip_ ) :: lformh, lvlder, lverfy, msgnp
      INTEGER ( KIND = ip_ ) :: nncnln, nsave, nload, ksave
      INTEGER ( KIND = ip_ ) :: ipadnp( 12 ), IPSVNP( mxparm )
      COMMON / NPPAR1/ IPSVNP, idbgnp, itmxnp, jvrfy1, jvrfy2, jvrfy3,
     *                 jvrfy4, ldbgnp, lformh, lvlder, lverfy, msgnp,
     *                 nlnf, nlnj, nlnx, nncnln, nsave, nload, ksave,
     *                 IPADNP
      COMMON / NPSOL_m/ m
C     debug = .TRUE.
      debug = .FALSE.

C  Open the relevant file

      OPEN ( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND( input )

C  compute problem dimensions

      CALL CUTEST_cdimen_r( status, input, n, m )
      IF ( status /= 0 ) GO TO 910

C  Set up parameters, variables and arrays required by NPSOL.
C
*  =====================================================================
*  Set the declared array dimensions for NPSOL
*
*  npm    = number of variables + linear & nonlinear constrnts
*  lda    = the declared leading dimension of A
*  ldcj   = the declared leading dimension of CJAC
*  ldr    = the declared leading dimension of R
*  liwork = the length of the integer work array
*  lwork  = the length of the double precision work array
*
*  Lengths of working arrays
*  according to User's Guide for NPSOL (Version 4.0):
*     liwork >= 3*n + nclin + 2*ncnln
*     lwork  >= 2*n*n + n*nclin + 2*n*ncnln + 20*n + 11*nclin + 21*ncnln
*
*  =====================================================================

      npm = n + m
      lda = m
      ldcj = m
      ldr = npm
      liwork = 3 * npm
      lwork = 2 * n * n + 3 * n * m + 20 * n + 32 * m

C  allocate space

      ALLOCATE( X( n ), BL( n ), BU( n ), Y( m ), CL( m ), CU( m ),
     *          G( n ), C( m ), EQUATN( m ), LINEAR( m ),
     *          WORK( lwork), IWORK( liwork ),
     *          ISTATE( npm ), CJAC( ldcj, n ), A( lda, n ),
     *          R( LDR, n ), BLOWER( npm ), BUPPER( npm ),
     *          CLAMBDA( npm ), STAT = status )
      IF ( status /= 0 ) GO TO 990

C  input problem data using CSETUP - reorder the constraints so that the
C  nonlinear constraints occur before the linear ones.  The constraints
C  are ordered in this way so that CCFG need evaluate the Jacobian for
C  only the first NCNLN constraints

      CALL CUTEST_csetup_r( status, input, out, io_buffer, n, m, X, BL,
     *                    BU, Y, CL, CU,EQUATN, LINEAR, 0, 2, 0 )
      CLOSE( input )
      IF ( status /= 0 ) GO TO 910

C  determine the number of linear and nonlinear constraints

      nclin = COUNT( LINEAR( 1 : m ) )
      ncnln = m - nclin

C  set up the lower bound vector BLOWER and upper bound vector BUPPER
C  in the order required by NPSOL. For i=1 to n, set BLOWER (BUPPER)
C  to the lower (upper) bound on the variables.  (CSETUP put these bounds
C  in BL and BU.). For i=n+1 to n+nclin, set BLOWER (BUPPER) to the lower
C  (upper) bounds on the linear constraints. For i=n+nclin+1 to n+nclin+
C   ncnln, set BLOWER (BUPPER) to the lower (upper) bounds on the nonlinear
C  constraints. At the same time, copy the multiplier estimates from Y to
C  CLAMBDA. CLAMBDA has the same ordering as BLOWER and BUPPER.

      DO 150 i = 1, n
        BLOWER( i ) = BL( i )
        BUPPER( i ) = BU( i )
        CLAMBDA( i ) = 0.0_rp_
        G( i ) = 0.0_rp_
  150 CONTINUE
      DO 160 i = 1, nclin
        ib = n + i
        ic = ncnln + i
        BLOWER( ib ) = CL( ic )
        BUPPER( ib ) = CU( ic )
        CLAMBDA( ib ) = Y( ic )
  160 CONTINUE
      DO 170 i = 1, ncnln
        ib = n + nclin + i
        BLOWER( ib ) = CL( i )
        BUPPER( ib ) = CU( i )
        CLAMBDA( ib ) = Y( i )
  170 CONTINUE

C  compute the constraint values and Jacobian at X = G = 0

      CALL CUTEST_ccfg_r( status, n, m, G, C, .FALSE.,
     *                  ldcj, n, CJAC, .TRUE. )
      IF ( status .NE. 0 ) THEN
        WRITE( 6, "( ' CUTEst error, status = ', i0, ', stopping' )")
     *   status
        STOP
      END IF

C  set A, the coefficients of the linear constraints

      DO 230 J = 1, n
        DO 210 i = 1, nclin
          ic = ncnln + i
          A( i, j ) = CJAC( ic, j )
  210   CONTINUE
  230 CONTINUE

C  Incorporate nonzero RHS constants of linear constraints into the
C  lower and upper bounds

      DO 250 i = 1, nclin
        ic = ncnln + i
        ib = n + i
        BLOWER( ib ) = BLOWER( ib ) + C( ic )
        BUPPER( ib ) = BUPPER( ib ) + C( ic )
  250 CONTINUE

C  Incorporate nonzero RHS constants of nonlinear constraints into
C  the lower and upper bounds.

C    DO 260 i = 1, ncnln
C       ib = n + nclin + i
C       BLOWER( ib ) = BLOWER( ib ) + C( i )
C       BUPPER( ib ) = BUPPER( ib ) + C( i )
C 260 CONTINUE

C  Get the problem name and write some debug messages.

C     m = nclin + ncnln
      CALL CUTEST_probname_r( status, pname )
      WRITE ( out, 2080 ) pname, n, nclin, ncnln
      IF ( debug ) THEN
        WRITE( out, 2030 ) ( i, X( i ), BLOWER( i ),
     *    BUPPER( i ), i = 1, n )
        IF ( nclin .GT. 0 ) WRITE( out, 2060 ) ( i, CLAMBDA( n + i ),
     *    BLOWER( N + i ), BUPPER( n + i ), EQUATN( ncnln + i ),
     *    i = 1, nclin )
        IF ( ncnln .GT. 0 ) WRITE( out, 2070 ) ( i, CLAMBDA( n + nclin
     *    + i ), BLOWER( n + nclin + i ), BUPPER( n + nclin + i ),
     *    EQUATN( i ), i = 1, ncnln )
      END IF
      cbgbnd = '1.0D+15'

C  ioptns = the unit number for reading the options file.
C  iprint = the unit number for writing the output file.

      ioptns = 4
      iprint = 9

C  Open and then read the options file

      OPEN ( UNIT = ioptns, FILE= 'NPSOL.SPC', STATUS='UNKNOWN' )
      CALL NPFILE( ioptns, inform )

C  gradients are used if requested

      fdgrad = lvlder .EQ. 0
      IF ( inform .NE. 0 .AND. out .GT. 0 ) THEN
        WRITE ( out, 3000 ) inform
        IF ( inform .EQ. 1 ) THEN
          WRITE ( out, 3001 )
        ELSE IF ( inform .EQ. 2 ) THEN
          WRITE ( out, 3002 )
        ELSE IF ( inform .EQ. 3 ) THEN
          WRITE ( out, 3003 )
        ELSE IF ( inform .EQ. 4 ) THEN
          WRITE ( out, 3004 )
        END IF
        STOP
      END IF
      CALL NPOPTN( 'Infinite Bound size =' // CBGBND )

C  Solve the problem

      CALL NPSOL( n, nclin, ncnln, lda, ldcj, ldr, A, BLOWER, BUPPER,
     *             NPSOL_evalcj, NPSOL_evalfg, inform, ITER, ISTATE,
     *             C, CJAC, CLAMBDA, f, G, R, X, IWORK, liwork, WORK,
     *             lwork )

      CALL CUTEST_creport_r( status, CALLS, CPU )

C  Print messages about abnormal NPSOL inform codes

      IF ( inform .GT. 0 .AND. out .GT. 0 ) THEN
        WRITE ( out, 4000 ) inform
        IF ( inform .EQ. 1 ) THEN
          WRITE ( out, 4001 )
        ELSE IF ( inform .EQ. 2 ) THEN
          WRITE ( out, 4002 )
        ELSE IF ( inform .EQ. 3 ) THEN
          WRITE ( out, 4003 )
        ELSE IF ( inform .EQ. 4 ) THEN
          WRITE ( out, 4004 )
        ELSE IF ( inform .EQ. 6 ) THEN
          WRITE ( out, 4006 )
        ELSE IF ( inform .EQ. 7 ) THEN
          WRITE ( out, 4007 )
        ELSE IF ( inform .EQ. 9 ) THEN
          WRITE ( out, 4009 )
        END IF
      END IF

C  Output final objective function value and timing information

      IF ( iprint .GT. 0 )
     *  WRITE ( iprint, 2000 ) pname, n, m, CALLS( 1 ), CALLS( 2 ),
     *   CALLS( 5 ), CALLS( 6 ), inform, f, CPU( 1 ), CPU( 2 )
      IF ( out .GT. 0 )
     *  WRITE ( out, 2000 ) pname, n, m, CALLS( 1 ), CALLS( 2 ),
     *   CALLS( 5 ), CALLS( 6 ), inform, f, CPU( 1 ), CPU( 2 )

      DEALLOCATE( X, BL, BU, Y, CL, CU, G, C, EQUATN, LINEAR, WORK,
     *            IWORK, ISTATE, CJAC, A, R, BLOWER, BUPPER, CLAMBDA,
     *            STAT = status )
      CALL CUTEST_cterminate_r( status )
      STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )")
     *   status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP

C  Non-executable statements

 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *    ,' Package used            :  NPSOL',    /
     *    ,' Problem                 :  ', A10,    /
     *    ,' # variables             =      ', I10 /
     *    ,' # constraints           =      ', I10 /
     *    ,' # objective functions   =        ', F8.2 /
     *    ,' # objective gradients   =        ', F8.2 /
     *    ,' # constraints functions =        ', F8.2 /
     *    ,' # constraints gradients =        ', F8.2 /
     *    ,' Exit code               =      ', I10 /
     *    ,' Final f                 = ', E15.7 /
     *    ,' Set up time             =      ', 0P, F10.2, ' seconds' /
     *    ,' Solve time              =      ', 0P, F10.2, ' seconds' //
     *     66('*') / )
 2030 FORMAT( /, ' The starting point:',
     *        /, '     i      X        BLOWER      BUPPER',
     *        /, ( I6, 1P, 3D12.4 ) )
 2060 FORMAT( /, ' The linear constraints:',
     *        /, '     i  MULTIPLIER   BLOWER      BUPPER    EQUALITY?',
     *        /, ( I6, 1P, 3D12.4, 5X, L1 ) )
 2070 FORMAT( /, ' The nonlinear constraints:',
     *        /, '     i  MULTIPLIER   BLOWER      BUPPER    EQUALITY?',
     *        /, ( I6, 1P, 3D12.4, 5X, L1 ) )
 2080 FORMAT( /, ' Run NPSOL on Problem ', A10,
     *        /, ' n = ', I5, ' nclin = ', I5, ' ncnln = ', I5 )
 3000 FORMAT( /, ' NPFILE terminated with inform =', I3 )
 3001 FORMAT(    ' IOPTNS .LT. 0 or IOPTNS .GT. 99 ' )
 3002 FORMAT(    ' BEGIN was found, but end-of-file occurred before',
     *           ' END was found.' )
 3003 FORMAT(    ' End-of-file occurred before BEGIN or ENDRUN were',
     *           ' found.' )
 3004 FORMAT(    ' ENDRUN was found before BEGIN.' )
 4000 FORMAT( /, ' NPSOL  terminated with inform =', I3 )
 4001 FORMAT(    ' Final iterate satisfies first-order Kuhn-Tucker',
     *           ' conditions',
     *        /, ' to accuracy requested, but iterates have not yet',
     *           ' converged.',
     *        /, ' No improvement could be made in merit function.' )
 4002 FORMAT(    ' No feasible point found for linear constraints and',
     *        /, ' bounds.  The problem is infeasible.' )
 4003 FORMAT(    ' No feasible point found for nonlinear constraints.',
     *        /, ' The problem may be infeasible.' )
 4004 FORMAT(    ' Maximum number of iterations reached.' )
 4006 FORMAT(    ' Final iterate does not satisfy Kuhn-Tucker',
     *           ' conditions',
     *        /, ' and no improved point could be found.')
 4007 FORMAT(    ' The provided derivatives of the objective function',
     *        /, ' or nonlinear constraints appear to be incorrect.' )
 4009 FORMAT(    ' An input parameter is invalid.' )
      END

      SUBROUTINE NPSOL_evalfg( mode, n, X, f, G, nstate )

C  evaluate the objective and its gradient

      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: mode, n, nstate
      REAL ( KIND = rp_ ) :: f
      REAL ( KIND = rp_ ) :: X( n ), G( n )

      INTEGER ( KIND = ip_ ) :: j, status
      LOGICAL :: grad, fdgrad
      COMMON / FDG / fdgrad

      IF ( mode .EQ. 0 ) THEN
        grad = .FALSE.
      ELSE
        grad = .TRUE.
      END IF
      CALL CUTEST_cofg_r( status, n, X, f, G, grad )
      IF ( status .NE. 0 ) THEN
        WRITE( 6, "( ' CUTEst error, status = ', i0, ', stopping' )")
     *   status
        STOP
      END IF

C    gradients are used if requested by setting the gradient to
C    appropriate NPSOL values.

      IF ( grad .AND. fdgrad ) THEN
        DO 20 j = 1, n
          G( j ) = -11111.0D+0
   20   CONTINUE
      END IF
      RETURN
      END

      SUBROUTINE NPSOL_evalcj( mode, ncnln, n, ldcj,
     *                         NEEDC, X, C, CJAC, nstate )

C  evaluate the constraints and their gradients (Jacobian)

      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: mode, ncnln, n, ldcj, nstate
      INTEGER ( KIND = ip_ ) :: NEEDC( * )
      REAL ( KIND = rp_ ) :: X( n ), C( ldcj ), CJAC( ldcj, n )

      INTEGER ( KIND = ip_ ) :: i, j, m, status
      LOGICAL :: grad, fdgrad
      COMMON / FDG / fdgrad
      COMMON / NPSOL_m/ m

      IF ( mode .EQ. 0 ) THEN
        grad = .FALSE.
      ELSE
        grad = .TRUE.
      END IF
      CALL CUTEST_ccfg_r( status, n, m, X, C, .FALSE.,
     *                  ldcj, n, CJAC, grad )
      IF ( status .NE. 0 ) THEN
        WRITE( 6, "( ' CUTEst error, status = ', i0, ', stopping' )")
     *   status
        STOP
      END IF

C   gradients are used if requested by setting the Jacobian to
C   appropriate NPSOL values.

      IF ( grad .AND. fdgrad ) THEN
        DO 20 j = 1, n
          DO 10 i = 1, ncnln
            CJAC( i, j ) = -11111.0_rp_
   10     CONTINUE
   20   CONTINUE
      END IF
      RETURN
      END


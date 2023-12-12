! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C
C     Main program for MINOS using CUTEst
C
C     Ingrid Bongartz, August 1992
C     CUTEst evolution, Nick Gould, January 2013
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      PROGRAM MINOS_main
      USE CUTEST_KINDS_precision
      IMPLICIT NONE
C  ------- these may need to be altered - see also funcon below --------
      INTEGER ( KIND = ip_ ), PARAMETER :: n_guess = 100000
      INTEGER ( KIND = ip_ ), PARAMETER :: ne_guess = 1000000
      INTEGER ( KIND = ip_ ), PARAMETER :: nwcore = 10000000
      INTEGER ( KIND = ip_ ), PARAMETER :: 
     *                          licwk = 2 * ne_guess + n_guess + 1
      INTEGER ( KIND = ip_ ), PARAMETER :: lcwk = ne_guess + n_guess
      INTEGER ( KIND = ip_ ) :: ICWK( licwk )
      REAL ( KIND = rp_ ) :: CWK( lcwk )
      REAL ( KIND = rp_ ) :: Z( nwcore )
C  ---------------------------------------------------------------------
      INTEGER ( KIND = ip_ ) :: ispecs, iprint, isumm, ns, l_j, status
      INTEGER ( KIND = ip_ ) :: m, n, ne, nb, nncon, nnjac, nnobj, iobj
      INTEGER ( KIND = ip_ ) :: inform, mincor, nlc
      INTEGER ( KIND = ip_ ) :: i, ii, j, k, jslack, njac, ninf, neq
      INTEGER ( KIND = ip_ ), PARAMETER :: input = 55, out = 6
      INTEGER ( KIND = ip_ ), PARAMETER :: io_buffer = 11
      INTEGER ( KIND = ip_ ) :: NAME1( 1 ), NAME2( 1 )
      REAL ( KIND = rp_ ) :: objadd, sinf, obj, atemp, f
      REAL ( KIND = rp_ ), PARAMETER :: big = 1.0E+20_rp_
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 7 )
      CHARACTER ( LEN = 8 ) :: start, NAMES( 5 )
      CHARACTER ( LEN = 10 ) :: pname
      INTEGER * 4, ALLOCATABLE, DIMENSION( : ) :: HA, HS
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: KA
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, BL, BU
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: AA, Y, C, RC
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATN, LINEAR
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: VNAME
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: CNAME

C  MINOS common block

      INTEGER ( KIND = ip_ ) ::  ncom, nden, nlag, nmajor, nminor
      REAL ( KIND = rp_ )  :: penpar, rowtol
      COMMON   /M8AL1 / penpar, rowtol, ncom, nden, nlag, nmajor, nminor

C  Sparse Jacobian common block 

      INTEGER ( KIND = ip_ ) :: jstrt, indv, indf 
      COMMON / SPJAC / CWK, ICWK, jstrt, indv, indf
      SAVE / SPJAC /

C  Open the problem input file

      OPEN ( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND( input )

C  compute problem dimensions

      CALL CUTEST_cdimen_r( status, input, n, m )
      IF ( status /= 0 ) GO TO 910

C  allocate space 

      nb = n + m + 1
      ALLOCATE( HS( nb ), KA( n + 1 ), X( nb ), BL( nb ), BU( nb ), 
     *          Y( m + 1 ), C( m + 1 ), RC( nb ), EQUATN( m + 1 ), 
     *          LINEAR( m + 1 ), VNAME( n ), CNAME( m + 1 ), 
     *          STAT = status )
      IF ( status /= 0 ) GO TO 990

C  Set up the unit numbers for the MINOS files:
C  ispecs is the Specifications file
C  iprint is the Print file
C  isumm is the Summary file

      ispecs = 4
      iprint = 9
      isumm  = 0
      CALL m1open( ispecs, 1, 'IN ' )
      CALL m1open( iprint, 2, 'OUT' )

C  Set options to default values and read Specs file

      CALL MISPEC( ispecs, iprint, isumm, nwcore, inform )
      IF ( inform .GE. 2 ) THEN
        IF ( out .GT. 0 ) WRITE( out, 2010 )
        STOP
      END IF

C  input problem data using csetup

      CALL CUTEST_csetup_r( status, input, out, io_buffer, n, m, 
     *                    X, BL, BU, Y, BL( n + 1 ), BU( n + 1 ), 
     *                    EQUATN, LINEAR, 0, 1, 1 )
      CLOSE( input )
      IF ( status /= 0 ) GO TO 910

C  compute the numbers of nonlinear variables, and linear/equatity constraints

      CALL CUTEST_cstats_r( status, nnobj, nnjac, neq, nlc )
      IF ( status /= 0 ) GO TO 910

C  Ensure there is sufficient room in CWK

      IF ( lcwk .LT. n ) THEN
        IF ( out .GT. 0 ) WRITE( out, 2000 ) 'CWK   ','LCWK  ', n - lcwk
        STOP
      END IF

!  compute the objective and constraints at X = 0

      DO 90 i = 1, n
        CWK( i ) = 0.0_rp_
   90 CONTINUE
      CALL CUTEST_cfn_r( status, n, m, CWK, f, C )
      IF ( status /= 0 ) GO TO 910

C  Determine the number of nonlinear constraints

      nncon = m - nlc

C  Use the constraint bounds to set the bounds on the slack variables

      DO 100 i = 1, m
        IF ( EQUATN( i ) ) THEN
          BL( n + i ) = 0.0_rp_
          BU( n + i ) = 0.0_rp_
        ELSE
          atemp = - BU( n + i )
          BU( n + i ) = - BL( n + i )
          BL( n + i ) = atemp
        END IF
  100 CONTINUE

C  Add one to m for linear objective row. Also set BL and BU for the objective 
C  row. If the objective function has a linear part, set iobj to m

      m = m + 1
      BL( n + m ) = - big
      BU( n + m ) = big
      C( m ) = 0.0_rp_
      X( n + m ) = 0.0_rp_
      IF ( nnobj .LT. n ) THEN
        iobj = m
      ELSE
        iobj = 0
      END IF

C  Set up AA(i), KA(j) and HA(i). AA(i) gives the i-th element in the Jacobian.
C  KA(j) gives starting address in AA of entries for variable j. HA(i) gives 
C  constraint index for i-th element in AA

C  Jacobian is to be stored in dense format

      IF ( nden .EQ. 1 ) THEN

C  Allocate space for AA and HA

        ne = m * n
        ALLOCATE( HA( ne ), AA( ne ), STAT = status )
        IF ( status /= 0 ) GO TO 990

C  find the entries in the dense Jacobian

        CALL CUTEST_cgr_r( status, n, m, X, Y, .FALSE., 
     *                   CWK, .FALSE., m, n, AA )
        IF ( status /= 0 ) GO TO 910

C  Set KA(j) and HA(j)

        IF ( ne .GT. 0 ) THEN
          KA( 1 ) = 1
          DO 220 j = 1, n 
            KA( j + 1 ) = KA( j ) + m
            k = KA( j ) - 1
            DO 210 i = 1, m
              HA( k + i ) = i
  210       CONTINUE

C  Copy gradient of linear part of objective function into row iobj of Jacobian

            IF ( j .GT. nnobj ) AA( k + iobj ) = CWK( j )
  220     CONTINUE
        END IF

C  Jacobian is to be stored in sparse format

      ELSE

C  compute the number of nonzeros in the Jacobian

        CALL CUTEST_cdimsj_r( status, ne )
        IF ( status /= 0 ) GO TO 910

C  Partition the integer sparse work vector ICWK

        jstrt = 0
        indv = jstrt + n + 1
        indf = indv + ne

C  Ensure there is sufficient room in ICWK

        IF ( licwk .LT. indf + ne ) THEN
          IF ( out .GT. 0 ) WRITE( out, 2000 )
     *       'ICWK  ','LICWK', indf + ne - licwk
          STOP
        END IF

C  Ensure there is sufficient room in CWK

        IF ( lcwk .LT. ne ) THEN
           IF ( out .GT. 0 ) WRITE( out, 2000 )
     *          'CWK   ','LCWK  ', ne - lcwk
           STOP
        END IF

C  Allocate space for AA and HA

        l_j = ne
        ALLOCATE( HA( ne ), AA( ne ), STAT = status )
        IF ( status /= 0 ) GO TO 990

C  Use CSGR to find entries in sparse Jacobian. Since CSGR and MINOS use 
C  different sparse formats, store Jacobian temporarily in CWK and ICWK.

        CALL CUTEST_csgr_r( status, n, m, X, Y, .FALSE., ne, l_j,
     *                    CWK, ICWK( indv + 1 ), ICWK( indf + 1 ) )
        IF ( status /= 0 ) GO TO 910
        k = ne

C  Initialize KA

        DO 250 j = 1, n
          KA( j ) = 0
  250   CONTINUE

C  Count Jacobian entries for each variable j. Store counts in KA(j).
C  Don't include nonlinear objective function entries

        DO 300 ii = 1, ne
          j = ICWK( indv + ii )
          i = ICWK( indf + ii )
          IF ( i .GT. 0 .OR. j .GT. nnobj ) THEN
            KA( j ) = KA( j ) + 1
          ELSE
            k = k - 1
          END IF
  300   CONTINUE
        KA( n + 1 ) = k + 1 

C  Now set KA(j) to starting address for variable j

        DO 310 j = n, 1, - 1
          KA ( j ) = KA( j + 1 ) - KA( j )
          ICWK( jstrt + j ) = 0
  310   CONTINUE

C  Loop through nonlinear Jacobian entries. Put correct entries in AA and HA.
C  Use KA to keep track of position for each variable j. Also count nonlinear 
C  Jacobian entries for each variable j. Store count in ICWK(jstrt+j)

        njac = 0
        DO 320 k = 1, ne
          j = ICWK( indv + k )
          i = ICWK( indf + k )
          IF ( i .GT. 0 .AND. i .LE. nncon .AND. j .LE. nnjac ) THEN
            ii = KA( j )
            AA( ii ) = CWK( k )
            HA( ii ) = i
            KA( j ) = ii + 1
            ICWK( jstrt + j ) = ICWK( jstrt + j ) + 1
            njac = njac + 1
          END IF
  320   CONTINUE

C  Now loop through linear Jacobian entries, including linear objective 
C  function entries. Put correct entries in AA and HA. Use KA to keep track 
C  of position for each variable J.

        DO 330 k = 1, ne
          j = ICWK( indv + k )
          i = ICWK( indf + k )
          IF ( i .EQ. 0 .AND. j .GT. nnobj ) THEN
            ii = KA ( j )
            AA( ii ) = CWK( k )
            HA( ii ) = iobj
            KA( j ) = ii + 1
          ELSE IF ( ( i .GT. 0 .AND. j .GT. nnjac ) .OR.
     *                i .GT. nncon ) THEN
            ii = KA ( j )
            AA( ii ) = CWK( k )
            HA( ii ) = i
            KA( j ) = ii + 1
          END IF
  330   CONTINUE

C  Reset KA(j) and set ICWK(jstrt+j). ICWK(jstrt+j) now gives starting address 
C  for variable j in nonlinear Jacobian. These addresses are needed in FUNCON

        ICWK( jstrt + n + 1 ) = njac + 1
        DO 340 j = n, 2, - 1
          KA ( j ) = KA( j - 1 )
          ICWK( jstrt + j ) = ICWK( jstrt + j + 1 ) - ICWK( jstrt + j )
  340   CONTINUE
        KA( 1 ) = 1 
        ICWK( jstrt + 1 ) = 1
        ne = KA( n + 1 ) - 1
      END IF

C  Incorporate nonzero constants from linear constraints as bounds on slack 
C  variables.  (Constants for nonlinear constraints are added in CCFG or 
C  CCFSG, which are called by FUNCON.)

      DO 400 i = 1, m
        jslack = n + i
        IF ( i .GT. nncon ) THEN
          BU( jslack ) = BU( jslack ) - C( i )
          BL( jslack ) = BL( jslack ) - C( i )
        END IF

C  If possible, set slack variables to be nonbasic at zero.

        X( jslack ) = MAX( 0.0_rp_, BL( jslack ) )
        X( jslack ) = MIN( X( jslack ), BU( jslack ) )
  400 CONTINUE

C  Incorporate nonzero constants from objective function groups only if 
C  objective function is completely linear. (If the objective function has 
C  nonlinear part, constants are added in COFG, which is called by FUNOBJ.)

      objadd = 0.0_rp_
      IF ( nnobj .EQ. 0 ) objadd = objadd - f

C  determine the names for problem quantities

      CALL CUTEST_cnames_r( status, n, m, pname, VNAME, CNAME )
      IF ( status /= 0 ) GO TO 910

C  Assign names to problem, constraints, and objective function.

      NAMES( 1 ) = pname( 1 : 8 )
      NAMES( 2 )  = 'Obj     '
      NAMES( 3 )  = 'RHS     '
      NAMES( 4 )  = 'Ranges  '
      NAMES( 5 )  = 'Bounds  '

      nb = n + m
      DO 410 I = 1, nb
        HS( i ) = 0
        RC( i ) = 0.0_rp_
  410 CONTINUE

C  Call MINOS as a subroutine

      start = 'COLD'
      CALL MINOSS( start, m, n, nb, ne, 1, nncon, nnobj, nnjac,
     *             iobj, objadd, NAMES, AA, HA, KA, BL, BU,
     *             NAME1, NAME2, HS, X, Y, RC, inform, mincor, 
     *             ns, ninf, sinf, obj, Z, nwcore )
      CALL CUTEST_creport_r( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910

C  Try to handle abnormal MINOS inform codes gracefully

      IF ( inform .GE. 20 .AND.
     *  ( iprint .GT. 0 .OR. isumm .GT. 0 ) ) THEN
         IF ( iprint .GT. 0 ) WRITE ( iprint, 3000 ) inform
         IF ( isumm  .GT. 0 ) WRITE ( isumm, 3000 ) inform
         IF ( inform .EQ. 20 ) THEN
            IF ( iprint .GT. 0 ) WRITE ( iprint, 3020 )
            IF ( isumm  .GT. 0 ) WRITE ( isumm, 3020 )
         ELSE IF ( inform .EQ. 21 ) THEN
            IF ( iprint .GT. 0 ) WRITE ( iprint, 3021 )
            IF ( isumm  .GT. 0 ) WRITE ( isumm, 3021 )
         ELSE IF ( inform .EQ. 22 ) THEN
            IF ( iprint .GT. 0 ) WRITE ( iprint, 3022 )
            IF ( isumm  .GT. 0 ) WRITE ( isumm, 3022 )
         ELSE IF ( inform .EQ. 32 ) THEN
            IF ( iprint .GT. 0 ) WRITE ( iprint, 3032 )
            IF ( isumm  .GT. 0 ) WRITE ( isumm, 3032 )
         ELSE IF ( inform .EQ. 42 ) THEN
            IF ( iprint .GT. 0 ) WRITE ( iprint, 3042 )
            IF ( isumm  .GT. 0 ) WRITE ( isumm, 3042 )
         END IF
      END IF
      IF ( out .GT. 0 ) THEN
         WRITE( out, 2110 ) obj, ( i, VNAME( i ), X( i ), BL( i ), 
     *                    BU( i ), RC( i ), i = 1, n )
         IF ( m .GT. 1 ) WRITE( out, 2120 ) ( i, CNAME( i ), 
     *        X( n + i ), BL( n + i ), BU( n + i ), Y( i ), 
     *        EQUATN( i ), LINEAR( i ), i = 1, m - 1 )
      END IF
      IF ( isumm .GT. 0 ) 
     *  WRITE ( isumm, 2020 ) NAMES( 1 ), n, m, CALLS( 1 ), CALLS( 2 ), 
     *    CALLS( 5 ), CALLS( 6 ), inform, obj, CPU( 1 ), CPU( 2 )
      CLOSE( ispecs )
      CLOSE( iprint )
      DEALLOCATE( HS, KA, X, BL, BU, Y, C, RC, EQUATN, 
     *          LINEAR, VNAME, CNAME, HA, AA, STAT = status )
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

 2000 FORMAT( /, ' ** SUBROUTINE MINOS_main: array length ', A, 
     *        ' too small.', /, ' -- Minimization abandoned.',
     *        /, ' -- Increase the parameter ', A, ' by at least ', I0,
     *           ' and restart.'  )
 2010 FORMAT( /, ' ** PROGRAM MINOS_main: No Specs file found.' )
 2020 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *,' Code used               :  MINOS',    /
     *,' Problem                 :  ', A10,    /
     *,' # variables             =    ', I10 /
     *,' # constraints           =    ', I10 /
     *,' # objective functions   =      ', F8.2 /
     *,' # objective gradients   =      ', F8.2 / 
     *,' # constraints functions =      ', F8.2 /
     *,' # constraints gradients =      ', F8.2 /
     *,' Exit code               =    ', I10 /
     *,' Final f                 = ', E15.7 /
     *,' Set up time             =    ', 0P, F10.2, ' seconds' /
     *,' Solve time              =    ', 0P, F10.2, ' seconds' //
     *     66('*') / )
 2110 FORMAT( /, ' the objective function value: ', 1P, D12.4, /,
     *        /, ' the variables:', //,
     *        '     # name          value    lower bound upper bound', 
     *        '  dual value', 
     *        /, ( I6, 1X, A10, 1P, 4D12.4 ) )
 2120 FORMAT( /, ' the constraints:', //,
     *        '     # name          value    lower bound upper bound', 
     *        '  multiplier equlty linear', 
     *        /, ( I6, 1X, A10, 1P, 4D12.4, 4X, L1, 5X, L1 ) )
 3000 FORMAT( /, ' WARNING!  Abnormal MINOS termination code:',
     *           ' inform = ', I2 )
 3020 FORMAT(    ' Not enough storage for the basis factorization.',
     *        /, ' Reduce parameters in MINOS.SPC file or increase',
     *           ' NWCORE in MINOS_main.' )
 3021 FORMAT(    ' Error in basis package.' )
 3022 FORMAT(    ' The basis is singular after several attempts to',
     *        /, ' factorize it (and add slacks where necessary).' )
 3032 FORMAT(    ' System error.  Wrong number of basic variables.' )
 3042 FORMAT(    ' Not enough storage to solve the problem.',
     *        /, ' Reduce parameters in MINOS.SPC file or increase',
     *           ' NWCORE in MINOS_main.' )
      END

      SUBROUTINE FUNOBJ( mode, n, X, f, G, nstate, nprob, Z, nwcore )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) ::  mode, n, nstate, nprob, nwcore
      REAL ( KIND = rp_ ) :: f
      REAL ( KIND = rp_ ) :: X( n ), G( n ), Z( nwcore )

C  Local variables

      INTEGER ( KIND = ip_ ) :: status
      LOGICAL :: grad
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
      RETURN
      END

      SUBROUTINE FUNCON( mode, m, n, njac, X, C, JAC, nstate, nprob,
     *                   Z, nwcore )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: mode, m, n, njac, nstate, nprob, nwcore
      REAL ( KIND = rp_ ) :: X( n ), C( m ), JAC( njac ), Z( nwcore )
      INTEGER ( KIND = ip_ ) :: ncom, nden, nlag, nmajor, nminor
      REAL ( KIND = rp_ ) :: penpar, rowtol
      COMMON / M8AL1 / penpar, rowtol, ncom, nden, nlag, nmajor, nminor

C  Sparse Jacobian common block 

C  ----- these may need to be altered - see also MINOS_main above ------
      INTEGER ( KIND = ip_ ), PARAMETER :: n_guess = 100000
      INTEGER ( KIND = ip_ ), PARAMETER :: ne_guess = 1000000
      INTEGER ( KIND = ip_ ), PARAMETER :: 
     *                          licwk = 2 * ne_guess + n_guess + 1
      INTEGER ( KIND = ip_ ), PARAMETER :: lcwk = ne_guess + n_guess
      INTEGER ( KIND = ip_ ) :: ICWK( licwk )
      REAL ( KIND = rp_ ) :: CWK( lcwk )
C  ---------------------------------------------------------------------
      INTEGER ( KIND = ip_ ) :: jstrt, indv, indf
      COMMON / SPJAC / CWK, ICWK, jstrt, indv, indf
      SAVE / SPJAC /

C  Local variables

      INTEGER ( KIND = ip_ ) :: i, j, k, nnzj, status
      LOGICAL :: grad

      IF ( mode .EQ. 0 ) THEN
        grad = .FALSE.
      ELSE
        grad = .TRUE.
      END IF

C  Jacobian is stored in dense format

      IF ( nden .EQ. 1 ) THEN
        CALL CUTEST_ccfg_r( status, n, m, X, C, .FALSE., m, n, 
     *                      JAC, grad )
        IF ( status .NE. 0 ) GO TO 910

C  Jacobian is stored in sparse format

      ELSE
        CALL CUTEST_ccfsg_r( status, n, m, X, C, nnzj, njac, CWK, 
     *                     ICWK( indv + 1 ), ICWK( indf + 1 ), grad )
        IF ( status .NE. 0 ) GO TO 910

C  Copy Jacobian from CCFSG, contained in CWK, into MINOS Jacobian JAC in 
C  correct order. Use ICWK(jstrt+j) to keep track of position for variable j

        IF ( grad ) THEN
          DO 130 i = 1, nnzj
            j = ICWK( indv + i )
            k = ICWK( jstrt + j )
            JAC( k ) = CWK ( i )
            ICWK( jstrt + j ) = K + 1
  130     CONTINUE

C  Reset ICWK(jstrt+j)

          DO 140 j = n, 2, - 1
            ICWK( jstrt + j ) = ICWK( jstrt + j - 1 )
  140     CONTINUE
          ICWK( jstrt + 1 ) = 1
        END IF
      END IF
      RETURN

  910 CONTINUE
      WRITE( 6, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
        STOP
      END

      SUBROUTINE MATMOD( NCYCLE, NPROB, FINISH, M, N, NB, NE, NKA, NS, 
     *                   NSCL, A, HA, KA, BL, BU, ASCALE, HS, ID1, ID2,
     *                   X, PI, Z, NWCORE )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: ncycle, nprob, m, n, nb, ne, nka
      INTEGER ( KIND = ip_ ) :: ns, nscl, nwcore
      INTEGER ( KIND = ip_ ) :: HA( ne ), HS( nb )
      INTEGER ( KIND = ip_ ) :: KA( nka ), ID1( nb ), ID2( nb )
      REAL ( KIND = rp_ ) :: A( ne ), ASCALE( nscl ), BL( nb ), BU( nb )
      REAL ( KIND = rp_ ) :: X( nb ), PI( m ), Z( nwcore )
      LOGICAL :: finish
      INTEGER ( KIND = ip_ ) :: iread, iprint, isumm
      COMMON / M1FILE / iread, iprint, isumm
      IF ( iprint .GT. 0 ) WRITE( iprint, 2000 )
      IF ( isumm .GT. 0 ) WRITE( isumm, 2000 )
      finish = .TRUE.
      RETURN
C
C  Non-executable statements
C
 2000 FORMAT(/ 'Subroutine MATMOD has not been loaded.')
      END

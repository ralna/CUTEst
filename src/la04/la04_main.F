! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      PROGRAM LA04_main

      USE CUTEST_KINDS_precision

C  --------------------------------------------------------------------
C
C  Solve the linear program
C
C     minimize     1/2 x(T) H x + c(T) x
C
C     subject to       A x = b,
C
C                   l <= x <= u
C
C  using the HSL code LA04.
C
C  Nick Gould.
C  December 1991.
C  Revised for CUTEst, January 2013
C
C  --------------------------------------------------------------------

      INTEGER ( KIND = ip_ ) :: npm, nplus, n, m, maxit, ib, iounit
      INTEGER ( KIND = ip_ ) :: status, iores
      INTEGER ( KIND = ip_ ) :: i, itern, ntotal, na, ns, nnzj, ind, job
      INTEGER ( KIND = ip_ ) :: lip, la, nt1, lws, liws, ii, nfree
      INTEGER ( KIND = ip_ ) :: nboth, nnoneg, nlower, l, ir, ic, j
      INTEGER ( KIND = ip_ ), PARAMETER :: out = 6, input = 55
      INTEGER ( KIND = ip_ ), PARAMETER :: inspec = 56, outsol = 57
      INTEGER ( KIND = ip_ ), PARAMETER :: io_buffer = 11
      REAL ( KIND = rp_ ), PARAMETER :: biginf = 9.0E+19_rp_
      REAL ( KIND = rp_ ) :: vl, vu, vx, vm, objf
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 7 )
      REAL ( KIND = rp_ ) :: CNTL( 15 ), RINFO( 40 )
      LOGICAL :: writes, pnamee
      CHARACTER ( LEN = 5 ) :: state
      CHARACTER ( LEN = 10 ) :: pname
      CHARACTER ( LEN = 14 ) :: pnames
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IPERM, IX
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: JX, INVPRM
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IRNA, IP
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IWS
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: A, X, Z
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: C, B, G, WS
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: BLOWER, BUPPER
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : , : ) :: BND
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATN, LINEAR
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: VNAME
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: CNAME

C  open the Spec file for the method

      OPEN( inspec, FILE = 'LA04.SPC', FORM = 'FORMATTED',
     *      STATUS = 'OLD' )
      REWIND( inspec )

C  read input Spec data

C     MAXIT : the maximum number of iterations
C     WRITES: write solution to "problem".sol if true

      READ ( inspec, 1000 ) MAXIT, IOUNIT, WRITES

C  close input file

      CLOSE( inspec )

C  open the relevant file

      OPEN( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND( input )

C  compute problem dimensions

      CALL CUTEST_cdimen_r( status, input, n, m )
      IF ( status /= 0 ) GO TO 910

C  allocate space 

      ALLOCATE( BLOWER( n + m ), BUPPER( n + m ),
     *          WS( n + m ), EQUATN( m ), LINEAR( m ), VNAME( n ), 
     *          CNAME( m ), STAT = status )
      IF ( status /= 0 ) GO TO 990

C  set up the data structures necessary to hold the group partially
C  separable function

      CALL CUTEST_csetup_r( status, input, out, io_buffer, n, m, 
     *              WS( 1 ), BLOWER( 1 ), BUPPER( 1 ),
     *              WS( n + 1 ), BLOWER( n + 1 ), 
     *              BUPPER( n + 1 ), EQUATN, LINEAR, 0, 0, 0 )
      IF ( status /= 0 ) GO TO 910

C  determine the names of the problem, variables and constraints

      CALL CUTEST_cnames_r( status, n, m, pname, VNAME, CNAME )
      IF ( status /= 0 ) GO TO 910

C  compute the total number of variables

      ntotal = n
      DO 10 i = 1, m
        IF ( .NOT. EQUATN( i ) ) ntotal = ntotal + 1
   10 CONTINUE

C   open the solution file if needed

      IF ( writes ) THEN
         DO 1 i = 1, 10
           IF ( PNAME( i : i ) .NE. ' ' ) THEN
             pnames( i : i ) = pname( i : i )
             j = I
           END IF
    1    CONTINUE
         pnames( j + 1 : j + 4 ) = '.sol'
         j = j + 4
         INQUIRE( FILE = pnames( 1 : j ), EXIST = pnamee )
         IF ( pnamee ) THEN
           OPEN( outsol, FILE = pnames( 1 : j ), FORM = 'FORMATTED',
     *           STATUS = 'OLD', IOSTAT = iores )
        ELSE
           OPEN( outsol, FILE = pnames( 1 : j ), FORM = 'FORMATTED',
     *           STATUS = 'NEW', IOSTAT = iores )
        END IF
        IF ( iores .NE. 0 ) THEN 
           WRITE( out, 2160 ) iores, pnames( 1 : j )
           STOP
        END IF
      END IF

C  compute the number of nonzeros in the constraint Jacobian

      CALL CUTEST_cdimsj_r( status, la )
      IF ( status /= 0 ) GO TO 910

C  allocate more space 

      la = la + ntotal - n
      ntotal = n + m
      npm = ntotal + m
      nplus = npm + 1
      ALLOCATE( IPERM( ntotal ), INVPRM( ntotal ), IX( m ), 
     *          JX( ntotal ), IRNA( la ), IP( nplus ), A( la ), 
     *          X( npm ), Z( ntotal ), C( ntotal ), 
     *          BND( 2, ntotal ), B( m ), G( ntotal ), STAT = status )
      IF ( status /= 0 ) GO TO 990

      ib = 10 * la
      lws = MAX( ntotal, ib + 3 * m + 1 ) + 3
      liws = 2 * ib + 10 * m + 12
      DEALLOCATE( WS, STAT = status )
      IF ( status /= 0 ) GO TO 990
      ALLOCATE( IWS( liws ), WS( lws ), STAT = status )
      IF ( status /= 0 ) GO TO 990

C  Set up the initial estimate of the solution, SOL, and
C  right-hand-side, RHS, of the Kuhn-Tucker system.

C  Set X to zero to determine the constant terms for
C  the problem functions.

      DO 30 i = 1, N
        X( i ) = 0.0_rp_
        C( i ) = 0.0_rp_
   30 CONTINUE

C  Evaluate the constant terms of the objective and constraint functions.

      CALL CUTEST_cfn_r( status, n, m, X, objf, B )
      IF ( status /= 0 ) GO TO 910

C     ns = na
      DO 40 i = 1, m
        BLOWER( n + i ) = BLOWER( n + i ) - B( i )
        BUPPER( n + i ) = BUPPER( n + i ) - B( i )
        B( i ) = 0.0_rp_
   40 CONTINUE

C  The variables will be permuted, before being passed to
C  the optimizer, so that the first NBOTH variables are
C  bounded on both sides and the last NTOTAL - NFREE + 1 satisfy
C  nonnegativity restrictions (the remaining variables are free)

C  On exit from the minimizer, the i-th original variable
C  will have the value X( IPERM( i ) ).

      ntotal = n
      na     = 0
      nboth  = 0
      nfree  = 0

C  Determine the status of each problem variable.

      DO 5 i = 1, n
        IF ( BUPPER( i ) .GT. biginf ) THEN
          IF ( BLOWER( i ) .LT. - biginf ) THEN
            nfree = nfree + 1
            IWS( i ) = 0
          ELSE IF ( BLOWER( i ) .EQ. 0.0_rp_ ) THEN
            IWS( i ) = 1
          ELSE
            nboth = nboth + 1
            IWS( i ) = 2
          END IF
        ELSE
          nboth = nboth + 1
          IWS( i ) = 2
        END IF
    5 CONTINUE

C  Determine the status of each slack variable.

      DO 12 i = 1, m
        IF ( .NOT. EQUATN( i ) ) THEN
          ntotal = ntotal + 1
          IF ( ( BLOWER( n + i ) .EQ. 0.0_rp_ .AND.
     *           BUPPER( n + i ) .GT. biginf ) .OR.
     *         ( BUPPER( n + i ) .EQ. 0.0_rp_ .AND.
     *           BLOWER( n + i ) .LT. - biginf ) ) THEN
             IWS( ntotal ) = 1
          ELSE
            nboth = nboth + 1
            IWS( ntotal ) = 2
          END IF
        END IF
   12 CONTINUE
      nt1 = ntotal + 1

C  permute the variables

      nnoneg = nboth + nfree
      nlower = nnoneg + 1
      nfree  = nboth
      nboth  = 0
      DO 15 i = 1, n
        IF ( IWS( i ) .EQ. 0 ) THEN
          nfree = nfree + 1
          IPERM( i ) = nfree
        ELSE IF ( IWS( i ) .EQ. 1 ) THEN
          nnoneg = nnoneg + 1
          IPERM( i ) = nnoneg
        ELSE IF ( IWS( i ) .EQ. 2 ) THEN
          nboth = nboth + 1
          IPERM( i ) = nboth
          BND( 1, nboth ) = BLOWER( i )
          BND( 2, nboth ) = BUPPER( i )
        END IF
   15 CONTINUE

C  introduce slack variables for inequality constraints.
C  Continue permuting the variables.

      IF ( ntotal .GT. n ) THEN
        ns = n
        DO 20 i = 1, m
          IF ( .NOT. EQUATN( i ) ) THEN
            na = na + 1
            ns = ns + 1
            IF ( IWS( ns ) .EQ. 1 ) THEN
              nnoneg = nnoneg + 1
              IPERM( ns ) = nnoneg
              IF ( BLOWER( n + i ) .EQ. 0.0_rp_ ) THEN
                A( na ) = - 1.0_rp_
              ELSE
                A( na ) = 1.0_rp_
              END IF
            ELSE IF ( IWS( ns ) .EQ. 2 ) THEN
              nboth = nboth + 1
              IPERM( ns ) = nboth
              BND( 1, nboth ) = BLOWER( n + i )
              BND( 2, nboth ) = BUPPER( n + i )
              A( na ) = - 1.0_rp_
            END IF
            IRNA( na ) = i
            IWS( na ) = IPERM( ns )
          END IF
   20   CONTINUE
      END IF
C     WRITE( out, "( ' n, m, k, l ', 4I7 )" ) 
C    *  ntotal, m, nboth, nfree + 1
      DO 25 i = 1, ntotal
        INVPRM( IPERM( i ) ) = i
   25 CONTINUE

C  Evaluate the linear terms of the objective and constraint functions
C  in a sparse format.

      CALL CUTEST_csgr_r( status, n, m, X, WS, .FALSE., nnzj, la - na, 
     *                  A( na + 1 ), IWS( na + 1 ), IRNA( na + 1 ) )
      IF ( status /= 0 ) GO TO 910

      ns = na
      DO 50 i = 1, nnzj
        IF ( A( ns + i ) .NE. 0.0_rp_ ) THEN
          IF ( IRNA( ns + i ) .GT. 0 ) THEN
            na = na + 1
            A( na ) = A( ns + i )
            IRNA( na ) = IRNA( ns + i )
            IWS( na ) = IPERM( IWS( ns + i ) )
          ELSE
            C( IPERM( IWS( ns + i ) ) ) = A( ns + i )
          END IF
        END IF
   50 CONTINUE
C     WRITE( out, "( ( ' row, col, val = ', 2I8, 1PD12.4 ) )" ) 
C    *  ( IRNA( i ), IWS( i ), A( i ), i = 1, NA )

C  Change from a co-ordinate storage scheme to a column-wise scheme

      ind = - 1
      lip = nt1 + 1
      CALL MC49AD( IND, ntotal, m, NA, IRNA, IWS, .TRUE., la, A,
     *             lip, IP, liws - na, IWS( na + 1 ), JOB )
      IP( lip + 1 ) = IP( lip )

C  Initialize CNTL

      CALL LA04ID( CNTL )
      CNTL( 7 ) = iounit

C  Prepare to call LA04

      job = 1

C  Loop over a sequence of simplex iterations

      DO 300 ITERN = 1, MAXIT
        CALL LA04AD( A, la, IRNA, IP, m, ntotal, B, C, BND, nboth, 
     *               nfree + 1, job, CNTL, IX, JX, X, Z, G, RINFO,
     *               WS, lws, IWS, liws )
        IF ( job .EQ. 0 ) GO TO 400
        IF ( job .EQ. - 4 ) THEN
          DEALLOCATE( WS, IWS, STAT = status )
          IF ( status /= 0 ) GO TO 990
          ib = 2 * INT( RINFO( 35 ) )
          lws = MAX( ntotal, ib + 3 * m + 1 ) + 3
          liws = 2 * ib + 10 * m + 12
          ALLOCATE( IWS( LIWS ), WS( lws ), STAT = status )
          IF ( status /= 0 ) GO TO 990
          job = 1
        END IF
        IF ( job .LT. 0 ) GO TO 890
  300 CONTINUE

C  End of main iteration loop.

  400 CONTINUE
      CALL CUTEST_creport_r( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910

C  Print details of the solution obtained.

      WRITE( out, 2010 ) JOB
      IF ( WRITES ) WRITE( outsol, 2010 ) JOB
      IF ( job .GE. 0 ) THEN
        l = 4
        DO 520 j = 1, 2
          IF ( j .EQ. 1 ) THEN
            ir = 1
            ic = MIN( l, n )
          ELSE
            IF ( ic. LT. n - l ) WRITE( out, 2000 )
            ir = MAX( ic + 1, n - ic + 1 )
            ic = n
          END IF
          DO 510 i = IR, IC
            ii = INVPRM( i )
            vm = Z( ii )
            IF ( ii .LE. nboth ) THEN
              IF ( JX( ii ) .LE. 0 ) THEN
                vx = X( ii )
                state = ' FREE'
              ELSE IF ( JX( ii ) .EQ. 1 ) THEN
                vx = X( ii ) + BND( 1, ii )
                state = 'LOWER'
              ELSE
                vx = X( ii ) + BND( 2, ii )
                state = 'UPPER'
              END IF
            ELSE
             vx = X( ii )
             state = ' FREE'
             IF ( i .GT. nfree .AND. 
     *            ABS( vx ) .LT. 1.0D-12 ) state = 'LOWER'
            END IF
            IF ( ii .LE. NBOTH ) THEN
              vl = BND( 1, ii )
              vu = BND( 2, ii )
            ELSE
              vu = biginf
               IF ( i .GE. nlower ) THEN
                 vl = 0.0_rp_
               ELSE
                 vl = - biginf
               END IF
            END IF
            WRITE( out, 2020 ) VNAME( i ), state, vx, vl, vu, vm
  510     CONTINUE
  520   CONTINUE
        IF ( writes ) THEN
          DO 540 i = 1, N
            ii = INVPRM( i )
            vm = Z( ii )
            IF ( ii .LE. nboth ) THEN
              IF ( JX( ii ) .LE. 0 ) THEN
                vx = X( ii )
                state = ' FREE'
              ELSE IF ( JX( ii ) .EQ. 1 ) THEN
                vx = X( ii ) + BND( 1, ii )
                state = 'LOWER'
              ELSE
                vx = X( ii ) + BND( 2, ii )
                state = 'UPPER'
              END IF
            ELSE
              vx = X( ii )
              state = ' FREE'
              IF ( i .GT. nfree .AND. 
     *             ABS( VX ) .LT. 1.0D-12 ) state = 'LOWER'
            END IF
            IF ( ii .LE. NBOTH ) THEN
              vl = BND( 1, ii )
              vu = BND( 2, ii )
            ELSE
              vu = biginf
              IF ( i .GE. nlower ) THEN
                vl = 0.0_rp_
              ELSE
                vl = - biginf
              END IF
            END IF
            WRITE( outsol, 2020 ) VNAME( i ), state, vx, vl, vu, vm
  540     CONTINUE
        END IF

C  Compute the Lagrange multipliers

         IF ( m .GT. 0 ) THEN
           DO 570 j = 1, m
             i = IX( j )
             IF ( i .GT. 2 * ( n + m ) ) THEN
               i = i - 2 * ( N + M ) 
             ELSE IF ( i .GT. ( n + m ) ) THEN
               i = i - ( n + m ) 
             END IF
             WS( j ) = C( i )
  570      CONTINUE
           j = 7
           CALL LA04AD( A, la, IRNA, IP, m, ntotal, B, C, BND, nboth, 
     *                  nfree + 1, j, CNTL, IX, JX, X, Z, G, RINFO,
     *                  WS, lws, IWS, liws )

           DO 580 j = 1, m
              X( ntotal + j ) = WS( j )
  580      CONTINUE

C  Now compute the constrainmt residuals

            DO 610 i = 1, M
              WS( i ) = 0.0_rp_
  610       CONTINUE
            DO 640 j = 1, NTOTAL
              IF ( INVPRM( j ) .LE. n ) THEN
              IF ( j .LE. nboth ) THEN
                IF ( JX( j ) .LE. 0 ) THEN
                   VX = X( j )
                ELSE IF ( JX( j ) .EQ. 1 ) THEN
                   VX = X( j ) + BND( 1, j )
                ELSE
                   VX = X( j ) + BND( 2, j )
                   STATE = 'UPPER'
                END IF
              ELSE
                VX = X( j )
              END IF
              DO 630 i = IP( j ), IP( j + 1 ) - 1
                WS( IRNA( i ) ) = WS( IRNA( i ) ) + A( i ) * VX
  630         CONTINUE
              END IF
  640       CONTINUE
            WRITE( out, 2040 )
            l = 2
            DO 660 j = 1, 2
              IF ( j .EQ. 1 ) THEN
                ir = 1
                ic = MIN( l, m )
              ELSE
                IF ( ic. LT. m - l ) WRITE( out, 2000 )
                ir = MAX( ic + 1, m - ic + 1 )
                ic = m
              END IF
              DO 650 i = ir, ic
                IF ( EQUATN( i ) ) THEN
                  state = 'EQUAL'
                ELSE
                  state = ' FREE'
                  IF ( ABS( WS( i ) - BLOWER( n + i ) )
     *                 .LT. 1.0D-12 ) state = 'LOWER'
                  IF ( ABS( WS( i ) - BUPPER( n + i ) )
     *                 .LT. 1.0D-12 ) state = 'UPPER'
                END IF
                WRITE( out, 2020 ) CNAME( i ), state, WS( i ),
     *            BLOWER( n + i ), BUPPER( n + i ), X( ntotal + i )
  650         CONTINUE
  660       CONTINUE
            IF ( writes ) THEN
              WRITE( outsol, 2040 )
              DO 680 i = 1, M
                IF ( EQUATN( i ) ) THEN
                  state = 'EQUAL'
                ELSE
                  state = ' FREE'
                  IF ( ABS( WS( i ) - BLOWER( n + i ) )
     *                 .LT. 1.0D-12 ) state = 'LOWER'
                  IF ( ABS( WS( i ) - BUPPER( n + i ) )
     *                 .LT. 1.0D-12 ) state = 'UPPER'
               END IF
               WRITE( outsol, 2020 ) CNAME( i ), state, WS( i ),
     *           BLOWER( n + i ), BUPPER( n + i ), X( NTOTAL + i )
  680         CONTINUE
            END IF
         END IF
         WRITE( out, 2030 ) RINFO( 1 ) + objf, itern
         IF ( WRITES ) WRITE( outsol, 2030 ) RINFO( 1 ) + objf, itern
      END IF
      WRITE ( out, 2001 ) pname, n, m, (CALLS( i ), i = 1, 3 ),
     *                  ( CALLS( i ), i = 5, 7 ),
     *                  JOB, RINFO( 1 ) + objf, CPU( 1 ), CPU( 2 )
      CLOSE( input )
      CALL CUTEST_cterminate_r( status )
      STOP

  890 CONTINUE
      WRITE( out, 2050 ) JOB
      STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', I0, ', stopping' )") 
     *   status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP

C  Non-executable statements

 1000 FORMAT( I10, /, I10, /, L10 )
 2000 FORMAT( ' .          .....  ............',
     *        '  ............  ............  ............ ' )
 2001 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *    ,' Package used            :  LA04',     /
     *    ,' Problem                 :  ', A,     /
     *    ,' # variables             =      ', I0 /
     *    ,' # constraints           =      ', I0 /
     *    ,' # objective functions   =        ', F8.2 /
     *    ,' # objective gradients   =        ', F8.2 / 
     *    ,' # objective Hessians    =        ', F8.2 /
     *    ,' # constraints functions =        ', F8.2 /
     *    ,' # constraints gradients =        ', F8.2 /
     *    ,' # constraints Hessians  =        ', F8.2 /
     *     ' Exit code               =      ', I0 /
     *    ,' Final f                 = ', E15.7 /
     *    ,' Set up time             =      ', 0P, F10.2, ' seconds' /
     *     ' Solve time              =      ', 0P, F10.2, ' seconds' //
     *     66('*') / )
 2010 FORMAT( ' Stopping with job = ', I0 /,
     *        ' Solution:', //,
     *        ' name       state     value   ',
     *        '   Lower bound   Upper bound  Dual variable ' )
 2020 FORMAT( 1X, A10, A6, 1P, 4D14.6 )
 2030 FORMAT( /, ' Final objective function value ', 1P, D22.14,
     *        /, ' Total number of iterations = ', I0 )
 2040 FORMAT( /, ' Constraints:', //,
     *        ' name       state     value   ',
     *        '   Lower bound   Upper bound    Multiplier ' )
 2050 FORMAT( /, ' Error return from LA04, job = ', I0 )
 2160 FORMAT( ' iostat = ', I0, ' when opening file ', A, 
     *        '. Stopping ' )

C  End of LA04_main

      END

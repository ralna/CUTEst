C     ( Last modified on 20 Feb 2013 at 12:00:00 )

      PROGRAM LQ_main

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     Driver for running LQ on CUTEst problems.
C
C     Nick Gould, February 2013
C     
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C  Set up parameters, variables and arrays required by constrained tools

      INTEGER, PARAMETER :: input = 55, indr = 46, out = 6
      INTEGER, PARAMETER :: io_buffer = 11
      INTEGER :: liwork, lwork, i, j, alloc_stat, status
      INTEGER :: n, m, m_e, iprint, ifail, lu, la1, lh1, lj1, m_total
      DOUBLE PRECISION :: f, eps, t
      DOUBLE PRECISION, PARAMETER :: zero = 0.0D+0, half = 5.0D-1
      DOUBLE PRECISION, PARAMETER :: infinity = 1.0D+19
      DOUBLE PRECISION :: CPU( 2 ), CALLS( 7 )
      CHARACTER * 10   pname
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: IWORK
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: X, X_l, X_u, X0
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: C, C_l, C_u, Y
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: U, B, G, WORK
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : , : ) :: A, H, J_val
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATN, LINEAR
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: X_names

C  Open the relevant file

      OPEN ( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND( input )

C  Determine the number of variables and constraints

      CALL CUTEST_cdimen( status, input, n, m )
      IF ( status /= 0 ) GO TO 910

C  Set workspace dimensions

      lh1 = MAX( n, 1 )
      lj1 = MAX( m, 1 )

C  Allocate suitable arrays

      ALLOCATE( X( n ), X_l( n ), X_u( n ), X0( n ), J_val( lj1, n ), 
     &          Y( m ), C_l( m ), C_u( m ), H( lh1, n ), G( n ),
     &          C( m ), EQUATN( m ), LINEAR( m ), X_names( n ), 
     &          STAT = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

C  Set up the data structures necessary to hold the group partially
C  separable function.

      CALL CUTEST_csetup( status, input, out, io_buffer, 
     &                    n, m, X, X_l, X_u,                    
     &                    Y, C_l, C_u, EQUATN, LINEAR, 1, 0, 0 )
      IF ( status /= 0 ) GO TO 910
      CLOSE( input )

C  count the number of equality constraints

      m_e = 0
      m_total = 0
      DO i = 1, m
        IF ( EQUATN( i ) ) THEN
          m_e = m_e + 1
          m_total = m_total + 1
        ELSE
          IF ( C_l( i ) > - infinity ) m_total = m_total + 1
          IF ( C_u( i ) < infinity ) m_total = m_total + 1
        END IF
      END DO 

C  Determine the name of the problem

      CALL CUTEST_probname( status, pname )
      IF ( status /= 0 ) GO TO 910
C     WRITE( out, "( /, ' Problem: ', A10 )" ) pname 

C  Set up the initial estimate of the solution and
C  right-hand-side of the Kuhn-Tucker system.

C  Determine the constant terms for the problem functions.

      X( : n ) = MIN( X_u( : n ),  MAX( X_l( : n ), X( : n ) ) )

C  Set X0 to zero to determine the constant terms for the problem functions

      X0 = zero 

C  Evaluate the constant terms of the objective (f) and constraint 
C  functions (C)

      CALL CUTEST_cfn( status, n, m, X0, f, C( : m ) )
      IF ( status /= 0 ) GO TO 910

C  Evaluate the linear terms of the constraint functions

      CALL CUTEST_cgr( status, n, m, X0, Y, .FALSE., G, .FALSE., 
     &                 lj1, n, J_val )
      IF ( status /= 0 ) GO TO 910

C  Evaluate the Hessian of the Lagrangian function at the initial point

      CALL CUTEST_cdh( status, n, m, X0, Y, lh1, H )
      IF ( status /= 0 ) GO TO 910
      DEALLOCATE( X0, LINEAR )

C  Allocate more arrays

      la1 = MAX( m_total, 1 )
      lu = m_total + n + n
      lwork = 3 * n * n / 2 + 10 * n + 2 * m_total + 14
      liwork = n
      ALLOCATE( A( la1, n ), B( m_total ), U( lu ),
     &          WORK( lwork ), IWORK( liwork ), STAT = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

C  Move to the constraint format required by QL in which only one-sided 
C  inequalities are allowed

      m_total = 0
      DO i = 1, m 
        IF ( EQUATN( i ) ) THEN 
          m_total = m_total + 1
          B( m_total ) = C( i ) - C_l( i )
          A( m_total, 1 : n ) = J_val( i, 1 : n )
        ELSE
          IF ( C_l( i ) > - infinity ) THEN
            m_total = m_total + 1
            B( m_total ) = C( i ) - C_l( i )
            A( m_total, 1 : n ) = J_val( i, 1 : n )
          END IF
          IF ( C_u( i ) < infinity ) THEN
            m_total = m_total + 1
            B( m_total ) = C_u( i ) - C( i )
            A( m_total, 1 : n ) = - J_val( i, 1 : n )
          END IF
        END IF 
      END DO

C  Deallocate arrays holding matrix row indices

      DEALLOCATE( J_val, C, C_l, C_u, Y, EQUATN )

C  open the Spec file for the package

      OPEN( indr, FILE = 'QL.SPC', FORM = 'FORMATTED', STATUS = 'OLD')
      REWIND( indr )

C  set up algorithmic input data

C   iprint  controls output level (0 = no print)
C   eps     tolerance for the convergence criterion

      READ ( indr, "( ( G10.8 ) )" ) iprint, eps
      CLOSE ( indr )

C   Call the optimizer

      CALL QL( m_total, m_e, la1, n, lh1, lu, H, G, A, B, X_l, X_u, 
     &         X, U, eps, 1, out, ifail, iprint, 
     &         WORK, lwork, IWORK, liwork )

C   Final objective function value

      f = zero
      IF ( ifail == 0 ) THEN
        DO i = 1, n
          t = zero
          DO j = 1, n
            t = t + H( i, j ) * X( j )
          ENDDO
          f = f + ( half * t + G( i ) ) * X( i )   
        ENDDO
      ENDIF

C  Output final objective function value and timing information

      IF ( out .GT. 0 ) THEN
        CALL CUTEST_creport( status, CALLS, CPU )
        IF ( ifail == 0 ) THEN
          CALL CUTEST_varnames( status, n, X_names )
          IF ( status /= 0 ) GO TO 910
          WRITE( out,"(' Objective function value:', ES12.4 )" ) f
          WRITE ( out, "( /, ' Solution:',
     &       /, '              X         X_l          X_u ',
     &       /, ( A10, 1P, 3D12.4 ) )" ) 
     &       ( X_names( i ), X( i ), X_l( i ), X_u( i ), i = 1, n )
C       ELSE
C         WRITE( out, "( 'Error message: ifail =', I0 )" ) ifail
        ENDIF
        WRITE ( out, 2000 ) pname, n, m, CALLS( 1 ), CALLS( 2 ), 
     &    CALLS( 5 ), CALLS( 6 ), ifail, f, CPU( 1 ), CPU( 2 )
      END IF

      DEALLOCATE( X, X_l, X_u, U, G, A, H, WORK, IWORK, STAT = status )
      CALL CUTEST_cterminate( status )
      STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     &   status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP

C  Non-executable statements

 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     &    ,' Package used            :  QL',    /
     &    ,' Problem                 :  ', A10,    /
     &    ,' # variables             =      ', I10 /
     &    ,' # constraints           =      ', I10 /
     &    ,' # objective functions   =        ', F8.2 /
     &    ,' # objective gradients   =        ', F8.2 / 
     &    ,' # constraints functions =        ', F8.2 /
     &    ,' # constraints gradients =        ', F8.2 /
     &    ,' Exit code               =      ', I10 /
     &    ,' Final f                 = ', E15.7 /
     &    ,' Set up time             =      ', 0P, F10.2, ' seconds' /
     &    ,' Solve time              =      ', 0P, F10.2, ' seconds' //
     &     66('*') / )

      END

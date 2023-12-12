! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      PROGRAM NLPQLP_main

      USE CUTEST_KINDS_precision
      IMPLICIT NONE

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     Driver for running NLPQLP on CUTEst problems.
C
C     Nick Gould, February 2013
C     
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C  Set up parameters, variables and arrays required by constrained tools

      INTEGER ( KIND = ip_ ), PARAMETER :: input = 55, indr = 46
      INTEGER ( KIND = ip_ ), PARAMETER :: io_buffer = 11, out = 6
      INTEGER ( KIND = ip_ ) :: la1, n1, lj1, lu, l_par, alloc_stat
      INTEGER ( KIND = ip_ ) :: status, iprint, maxnm
      INTEGER ( KIND = ip_ ) :: n, m_e, m, i, k, l, maxit, maxfun
      INTEGER ( KIND = ip_ ) :: lwa, lkwa, lactiv, mode, ifail, m_total
      REAL ( KIND = rp_ ) :: acc, accqp, stpmin, rho
      LOGICAL :: lql
      REAL ( KIND = rp_ ), PARAMETER :: infinity = 1.0E+19_rp_
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 7 )
      CHARACTER * 10 :: pname
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: KWA
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X_l, X_u, F
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: C_l, C_u, Y
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: U, G, D, WA
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : , : ) :: A, H, C, X
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : , : ) :: J_val, CON
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATN, LINEAR, ACTIVE
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: X_names
      EXTERNAL :: QL

C  open the Spec file for the package

      OPEN( indr, FILE = 'NLPQLP.SPC', FORM = 'FORMATTED', 
     &      STATUS = 'OLD')
      REWIND( indr )

C  set up algorithmic input data

C   iprint  controls output level (0 = no print)
C   acc     desired final accuracy
C   accqp   QP accuracy tolerance
C   stpmin  minimum step length when using parallel line searches
C   maxit   maximum number of iterations
C   maxfun  maximum number of function evaluations during line search
C   maxnm   history length for non-monotone line search
C   rho     scaling for initial QN Hessian approximation
C   l_par   number of parallel systems
C   lql     true if the QP is solved with a full QN approximation

      READ( indr, "( 9( G10.8, / ), L1 )" ) iprint, acc, accqp, stpmin, 
     &        maxit, maxfun, maxnm, rho, l_par, lql   
      CLOSE( indr )

C     write(6,* ) iprint, acc, accqp, stpmin, 
C    &  maxit, maxfun, maxnm, rho, l_par, lql   

C  Open the relevant file

      OPEN( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *      STATUS = 'OLD' )
      REWIND( input )

C  Determine the number of variables and constraints

      CALL CUTEST_cdimen_r( status, input, n, m )
      IF ( status /= 0 ) GO TO 910

C  Set workspace dimensions

      n1 = MAX( n + 1, 2 )
      lj1 = MAX( m, 1 )

C  Allocate suitable arrays

      ALLOCATE( X( n1, l_par ), F( l_par ), X_l( n ), X_u( n ), 
     &          J_val( lj1, n ), Y( m ), C_l( m ), C_u( m ), G( n1 ), 
     &          D( n1 ), H( n1, n1 ), C( m, l_par ), EQUATN( m ), 
     &          LINEAR( m ), X_names( n ), STAT = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

C  Set up the data structures necessary to hold the group partially
C  separable function.

      CALL CUTEST_csetup_r( status, input, out, io_buffer,
     &                    n, m, X( : n, 1 ), X_l, X_u,
     &                    Y, C_l, C_u, EQUATN, LINEAR, 1, 0, 0 )
      IF ( status /= 0 ) GO TO 910
      DEALLOCATE( LINEAR )

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

      CALL CUTEST_probname_r( status, pname )
      IF ( status /= 0 ) GO TO 910
C     WRITE( out, "( /, ' Problem: ', A10 )" ) pname 

C  Allocate more arrays

      lu = m_total + n + n + 2
      lwa = 3 * n * n / 2 + 33 * n + 9 * m_total + 200
      lkwa = n + 25
      lactiv = 2 * m_total + 10
      la1 = MAX( m_total, 1 )
      ALLOCATE( U( lu ), CON( m_total, l_par ), A( la1, n1 ), WA( lwa ),
     &          KWA( lkwa ), ACTIVE( lactiv ), STAT = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

C  main iteration

      X( : n, 1 ) = MIN( X_u( : n ),  MAX( X_l( : n ), X( : n, 1 ) ) )
      mode = 0
      ifail = 0
      l = 1

      DO

C  compute the function and constraints at l points X(:,1:l)

        IF ( ifail == 0 .OR. ifail == - 1 ) THEN
          DO k = 1, l
            CALL CUTEST_cfn_r( status, n, m, X( : n, k ), 
     &                       F( k ), C( : m, k ) )
            IF ( status == 3 ) THEN
              ifail = - 10
              EXIT
            END IF
            IF ( status /= 0 ) GO TO 910

C  convert the constraints to the form requird by NLPQLP

            m_total = 0
            DO i = 1, m 
              IF ( EQUATN( i ) ) THEN 
                m_total = m_total + 1
                CON( m_total, k ) = C( i, k ) - C_l( i )
              ELSE
                IF ( C_l( i ) > - infinity ) THEN
                  m_total = m_total + 1
                  CON( m_total, k ) = C( i, k ) - C_l( i )
                END IF
                IF ( C_u( i ) < infinity ) THEN
                  m_total = m_total + 1
                  CON( m_total, k ) = C_u( i ) - C( i, k )
                END IF
              END IF 
            END DO
          END DO
        END IF

C  compute the function and constraints at the point X(:,1)

        IF ( ifail == 0 .OR. ifail == - 2 ) THEN
          CALL CUTEST_cgr_r( status, n, m, X( : n, 1 ), Y, .FALSE., G, 
     &                     .FALSE., lj1, n, J_val )
          IF ( status /= 0 ) GO TO 910

C  convert the constraint gradients to the form requird by NLPQLP

          m_total = 0
          DO i = 1, m 
            IF ( EQUATN( i ) ) THEN 
              m_total = m_total + 1
              A( m_total, 1 : n ) = J_val( i, 1 : n )
            ELSE
              IF ( C_l( i ) > - infinity ) THEN
                m_total = m_total + 1
                A( m_total, 1 : n ) = J_val( i, 1 : n )
              END IF
              IF ( C_u( i ) < infinity ) THEN
                m_total = m_total + 1
                A( m_total, 1 : n ) = - J_val( i, 1 : n )
              END IF
            END IF 
          END DO
        END IF
        IF ( ifail == 0 ) l = l_par

C  perform another iteration of the optimization method

        CALL NLPQLP( l, m_total, m_e, la1, n, n1, lu, X, F, CON,  
     &               G, A, U, X_l, X_u, H, D, acc,  accqp, stpmin, 
     &               maxfun,  maxit,  maxnm, rho, iprint, mode,
     &               out, ifail, WA, lwa, KWA, lkwa, ACTIVE, lactiv,
     &               lql, QL)
        IF ( ifail >= 0 ) EXIT
      END DO

C  Output final objective function value and timing information

      IF ( out .GT. 0 ) THEN
        CALL CUTEST_creport_r( status, CALLS, CPU )
        IF ( ifail == 0 ) THEN
          CALL CUTEST_varnames_r( status, n, X_names )
          IF ( status /= 0 ) GO TO 910
          WRITE( out,"(' Objective function value:', ES12.4 )" ) F( 1 )
          WRITE ( out, "( /, ' Solution:',
     &       /, '              X         X_l          X_u ',
     &       /, ( A10, 1P, 3D12.4 ) )" ) 
     &       ( X_names( i ), X( i, 1 ), X_l( i ), X_u( i ), i = 1, n )
C       ELSE
C         WRITE( out, "( 'Error message: ifail =', I0 )" ) ifail
        ENDIF
        WRITE ( out, 2000 ) pname, n, m, CALLS( 1 ), CALLS( 2 ), 
     &    CALLS( 5 ), CALLS( 6 ), ifail, F( 1 ), CPU( 1 ), CPU( 2 )
      END IF

      DEALLOCATE( X, X_l, X_u, U, F, G, A, H, WA, KWA, ACTIVE, CON, 
     &            J_val, C, C_l, C_u, Y, EQUATN, STAT = status )
      CALL CUTEST_cterminate_r( status )
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
     &    ,' Package used            :  NLPQLP',    /
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

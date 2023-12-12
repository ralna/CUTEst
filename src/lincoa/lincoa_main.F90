! THIS VERSION: CUTEST 2.2 - 2023-11-16 AT 13:00 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      PROGRAM LINCOA_main

!  LINCOA test driver for problems derived from SIF files

!  Nick Gould, December 2013

      USE CUTEST_KINDS_precision
      USE CUTEST_PROBLEM_precision

      IMPLICIT NONE
      INTEGER ( KIND = ip_ ) :: maxfun, lw, status, iprint, i, npt, m, n, mc
      REAL( KIND = rp_ ) :: rhobeg, rhoend, f
      REAL( KIND = rp_ ), PARAMETER :: infty = 1.0E+19_rp_
      REAL( KIND = rp_ ), DIMENSION( : ), ALLOCATABLE :: X, X_l, X_u, B, G, W
      REAL( KIND = rp_ ), DIMENSION( : ), ALLOCATABLE :: Y, C_l, C_u
      REAL( KIND = rp_ ), DIMENSION( : , : ), ALLOCATABLE :: A, J
      LOGICAL, DIMENSION( : ), ALLOCATABLE  :: EQUATN, LINEAR
      CHARACTER ( LEN = 10 ) :: pname
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: VNAMES, CNAMES
      REAL( KIND = rp_ ), DIMENSION( 4 ) :: CPU
      REAL( KIND = rp_ ), DIMENSION( 4 ) :: CALLS
      INTEGER ( KIND = ip_ ) :: io_buffer = 11
      INTEGER ( KIND = ip_ ), PARAMETER :: input = 55, indr = 46, out = 6

!  open the relevant file

      OPEN( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED', STATUS = 'OLD' )
      REWIND( input )

!  compute problem dimensions

      CALL CUTEST_cdimen_r( status, input, n, m )
      IF ( status /= 0 ) GO TO 910

!  allocate space

      ALLOCATE( X( n ), X_l( n ), X_u( n ), G( n ), Y( m ), C_l( m ),          &
                C_u( m ), J( m, n ), EQUATN( m ), LINEAR( m ), STAT = status )
      IF ( status /= 0 ) GO TO 990

!  initialize problem data structure

!  set up the data structures necessary to hold the problem functions.

      CALL CUTEST_csetup_r( status, input, out, io_buffer, n, m,               &
                            X, X_l, X_u, Y, C_l, C_u, EQUATN, LINEAR, 0, 0, 0 )
      IF ( status /= 0 ) GO TO 910
      CLOSE( input )

!  compute the constraint Jacobian

      CALL CUTEST_cgr_r( status, n, m, X, Y, .FALSE., G, .FALSE., m, n, J  )
      IF ( status /= 0 ) GO TO 910

!  compute the number of constraints (include simple bounds, constraints
!  bounded on both sides and equality constraints)

      mc = 0
      DO i = 1, n
        IF ( X_l( i ) > - infty ) mc = mc + 1
        IF ( X_u( i ) < infty ) mc = mc + 1
      END DO
      DO i = 1, m
        IF ( C_l( i ) > - infty ) mc = mc + 1
        IF ( C_u( i ) < infty ) mc = mc + 1
      END DO

!  allocate further space

      DEALLOCATE( EQUATN, LINEAR )
      ALLOCATE( A( n, mc ), B( mc ), STAT = status )
      IF ( status /= 0 ) GO TO 990

!  set A and b; all constraints are inequalities A x <= b

      mc = 0
      DO i = 1, n
        IF ( X_l( i ) > - infty ) THEN
          mc = mc + 1
          A( 1 : n, mc ) = 0.0_rp_
          A( i, mc ) = - 1.0_rp_
          B( mc ) = - X_l( i )
        END IF
        IF ( X_u( i ) < infty ) THEN
          mc = mc + 1
          A( 1 : n, mc ) = 0.0_rp_
          A( i, mc ) = 1.0_rp_
          B( mc ) = X_u( i )
        END IF
      END DO
      DO i = 1, m
        IF ( C_l( i ) > - infty ) THEN
          mc = mc + 1
          A( 1 : n, mc ) = - J( i, 1 : n )
          B( mc ) = - C_l( i )
        END IF
        IF ( C_u( i ) < infty ) THEN
          mc = mc + 1
          A( 1 : n, mc ) = J( i, 1 : n )
          B( mc ) = C_u( i )
        END IF
      END DO

!  open the Spec file for the method

      OPEN( indr, FILE = 'LINCOA.SPC', FORM = 'FORMATTED', STATUS = 'OLD')
      REWIND( indr )

!  read input Spec data

!  RHOBEG = the size of the simplex initially
!  RHOEND = the size of the simplex at termination
!  NPT = the number of interpolation conditions; <=0 defaults to 2n+1
!  MAXFUN = the maximum number of function calls allowed.
!  IPRINT should be set to 0, 1, 2 or 3, it controls the amount of printing

!  set up algorithmic input data

      READ ( indr, 1000 ) rhobeg, rhoend, npt, maxfun, iprint
      CLOSE ( indr )

!  ensure that npt satsfies interpolation limits

      IF ( npt <= 0 ) npt = 2 * n + 1
      npt = MIN( MAX( npt, n + 2 ), ( n + 1 ) * ( n + 2 ) / 2 )

!  allocate the temporary work array W of length at least mc*(2+n) +
!  npt*(4+n+npt) + n*(9+3*n) + max( mc+3*n, 2*mc+n, 2*npt) ... so double this

      lw = 2 * ( mc * ( 2 + n ) + npt * ( 4 + n + npt ) + n * ( 9 + 3 * n ) +  &
                 MAX( mc + 3 * n, 2 * mc + n, 2 * npt ) )
      ALLOCATE( W( lw ), STAT = status )
      IF ( status /= 0 ) GO TO 990

!  perform the minimization

      CALL LINCOA( n, npt, mc, A, n, B, X, rhobeg, rhoend, iprint, maxfun, W )

!  compute the constraint values

      DO i = 1, m
        Y( i ) = DOT_PRODUCT( J( i, 1 : n ), X( 1 : n ) )
      END DO

!  output report

      CALL CUTEST_ureport_r( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910

      ALLOCATE( VNAMES( n ), CNAMES( m ), STAT = status )
      CALL CUTEST_cnames_r( status, n, m, pname, VNAMES, CNAMES )
      CALL CALFUN( n, X, f )

      WRITE( out, 2110 ) ( i, VNAMES( i ), X( i ), X_l( i ), X_u( i ),         &
                           i = 1, n )
      WRITE( out, 2120 ) ( i, CNAMES( i ), Y( i ), C_l( i ), C_u( i ),         &
                           i = 1, m )
      WRITE( out, 2000 ) pname, n, CALLS( 1 ), f, CPU( 1 ), CPU( 2 )

!  clean-up data structures

      DEALLOCATE( G, J, X, X_l, X_u, Y, C_l, C_u, VNAMES, CNAMES, W,           &
                  STAT = status )
      IF ( status /= 0 ) GO TO 910
      CALL CUTEST_cterminate_r( status )
      STOP

!  error returns

  910 CONTINUE
      WRITE( 6, "( ' CUTEst error, status = ', i0, ', stopping' )") status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP

!  Non-executable statements

2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //,                    &
          ' Package used            :  LINCOA ',  /,                           &
          ' Problem                 :  ', A10,    /,                           &
          ' # variables             =      ', I10 /,                           &
          ' # objective functions   =        ', F8.2 /,                        &
          ' Final f                 = ', E15.7 /,                              &
          ' Set up time             =      ', 0P, F10.2, ' seconds' /,         &
          ' Solve time              =      ', 0P, F10.2, ' seconds' //,        &
          66('*') / )
1000 FORMAT( D12.4, /, D12.4, /, I6, /, I6, /, I6 )
2110 FORMAT( /, ' The variables:', /, &
          '     i name          value    lower bound upper bound',             &
          /, ( I6, 1X, A10, 1P, 3D12.4 ) )
2120 FORMAT( /, ' The constraints:', /, &
          '     i name          value    lower bound upper bound',             &
          /, ( I6, 1X, A10, 1P, 3D12.4 ) )

!  End of LINCOA_main

      END PROGRAM LINCOA_main

      SUBROUTINE CALFUN( n, X, f )

!  evaluates the objective function value in a format compatible with LINCOA,
!  but using the CUTEst tools.

      USE CUTEST_KINDS_precision
      USE CUTEST_PROBLEM_precision

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
      REAL( KIND = rp_ ), INTENT( OUT ) :: f
      REAL( KIND = rp_ ), INTENT( IN ) :: X( n )

      INTEGER ( KIND = ip_ ) ::  status
      REAL( KIND = rp_ ) :: G( n )

!  Evaluate the objective function and constraints.

      CALL CUTEST_uofg_r( status, n, X, f, G, .FALSE. )
      IF ( status /= 0 ) GO TO 910
!     f = CUTEST_problem_global%f
      RETURN

  910 CONTINUE
      WRITE( 6, "( ' CUTEst error, status = ', i0, ', stopping' )" ) status
      STOP

!  End of CALFUN

      END SUBROUTINE CALFUN


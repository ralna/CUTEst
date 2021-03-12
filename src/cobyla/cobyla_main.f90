!   ( Last modified on 3 Jan 2013 at 13:20:00 )

      PROGRAM COBYLA_main

!  COBYLA test driver for problems derived from SIF files

!  A. R. Conn and Ph. Toint (based upon Nick Gould's vf13ma.f)
!  January 1995.
!  Fortran 90/95 version, D. Orban, December 2006
!  Revised for CUTEst, Nick Gould, January 2013

      USE CUTEst_problem

      IMPLICIT NONE
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )
      INTEGER :: m, maxfun, lw, liw, status, iprint, i, mgeq, nfix, ierr
      INTEGER, DIMENSION(:), ALLOCATABLE :: IW
      REAL( KIND = wp ) :: rhobeg, rhoend
      REAL( KIND = wp ), PARAMETER :: infty = 1.0D+19
      REAL( KIND = wp ), DIMENSION(:), ALLOCATABLE :: W
      REAL( KIND = wp ), DIMENSION( 4 ) :: CPU
      REAL( KIND = wp ), DIMENSION( 7 ) :: CALLS
      INTEGER :: io_buffer = 11
      INTEGER, PARAMETER :: input = 55, indr = 46, out = 6

!  open the relevant file

      OPEN( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED', STATUS = 'OLD' )
      REWIND( input )

!  initialize problem data structure

! No need for Hessian of objective/Lagrangian

      CUTEST_problem_global%allocate_H = .FALSE.

! No need for Jacobian of constraints

      CUTEST_problem_global%allocate_J = .FALSE.   

      CALL CUTEST_problem_setup( status, CUTEST_problem_global, input )
      IF ( status /= 0 ) GO TO 910

!  set up the data structures necessary to hold the problem functions.

      CALL CUTEST_csetup( status, input, out, io_buffer,                       &
                          CUTEST_problem_global%n, CUTEST_problem_global%m,    &
                          CUTEST_problem_global%x, CUTEST_problem_global%x_l,  &
                          CUTEST_problem_global%x_u, CUTEST_problem_global%y,  &
                          CUTEST_problem_global%c_l, CUTEST_problem_global%c_u,&
                          CUTEST_problem_global%equation,                      &
                          CUTEST_problem_global%linear, 1, 0, 0 )
      IF ( status /= 0 ) GO TO 910
      CLOSE( input )

!  allocate temporary work arrays

      lw  = CUTEST_problem_global%n * ( 3 * CUTEST_problem_global%n +          &
        2 * CUTEST_problem_global%m + 11 ) + 4 * CUTEST_problem_global%m + 6
      ALLOCATE( W( lw ), STAT = status )
      IF ( status /= 0 ) GO TO 990
      liw = CUTEST_problem_global%n + 1
      ALLOCATE( IW( liw ), STAT = status )
      IF ( status /= 0 ) GO TO 990

!  count the number of general equality constraints and ignore them by
!  shifting the remaining constraints at the beginning of the constraint list

      mgeq = COUNT( CUTEST_problem_global%EQUATION( : CUTEST_problem_global%m ))
      IF ( mgeq > 0 ) THEN
        WRITE( 6, 3090 ) mgeq
        DO i = CUTEST_problem_global%m, mgeq + 1, - 1
          CUTEST_problem_global%c_u( i - mgeq ) = CUTEST_problem_global%c_u( i )
          CUTEST_problem_global%c_l( i - mgeq ) = CUTEST_problem_global%c_l( i )
        END DO
      END IF
      m = CUTEST_problem_global%m - mgeq

!  if constraints have both lower and upper bounds, they must be included twice!

      DO i = 1, CUTEST_problem_global%m - mgeq
         IF ( CUTEST_problem_global%c_l( i ) > - infty .AND.                   &
              CUTEST_problem_global%c_u( i ) < infty ) m = m + 1
      END DO

!  include any simple bounds

      nfix = 0
      DO i = 1, CUTEST_problem_global%n
        IF ( CUTEST_problem_global%x_l( i ) ==                                 &
             CUTEST_problem_global%x_u( i ) ) THEN
          nfix = nfix + 1
        Else
          IF ( CUTEST_problem_global%x_l( i ) > -infty ) m = m + 1
          IF ( CUTEST_problem_global%x_u( i ) <  infty ) m = m + 1
        END IF
      END DO
      IF ( nfix > 0 ) WRITE( 6, 3020 ) nfix

!  open the Spec file for the method

      OPEN( indr, FILE = 'COBYLA.SPC', FORM = 'FORMATTED', STATUS = 'OLD')
      REWIND( indr )

!  read input Spec data

!  RHOBEG = the size of the simplex initially
!  RHOEND = the size of the simplex at termination
!  MAXFUN = the maximum number of function calls allowed.
!  IPRINT   should be set to 0, 1, 2 or 3, it controls the amount of printing

!  set up algorithmic input data

      READ ( indr, 1000 ) rhobeg, rhoend, maxfun, iprint
      CLOSE ( indr )

!  perform the minimization

      CALL COBYLA( CUTEST_problem_global%n, m, CUTEST_problem_global%X,        &
                   rhobeg, rhoend, iprint, maxfun, W, IW )

!  output report

      CALL CUTEST_creport( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910

      CALL CUTEST_cnames( status, CUTEST_problem_global%n,                     &
                          CUTEST_problem_global%m,                             &
                          CUTEST_problem_global%pname,                         &
                          CUTEST_problem_global%vnames,                        &
                          CUTEST_problem_global%cnames )
      WRITE( out, 2110 ) ( i, CUTEST_problem_global%vnames( i ),               &
         CUTEST_problem_global%x( i ), CUTEST_problem_global%x_l( i ),         &
         CUTEST_problem_global%x_u( i ), i = 1, CUTEST_problem_global%n )
      IF ( CUTEST_problem_global%m > 0 ) WRITE( 6, 2120 ) ( i,                 &
         CUTEST_problem_global%cnames( i ), CUTEST_problem_global%c( i ),      &
         CUTEST_problem_global%c_l( i ), CUTEST_problem_global%c_u( i ),       &
         CUTEST_problem_global%linear( i ), i = 1, CUTEST_problem_global%m )
      WRITE( out, 2000 ) CUTEST_problem_global%pname, CUTEST_problem_global%n, &
         CUTEST_problem_global%m, CALLS( 1 ), CALLS( 5 ),                      &
         CUTEST_problem_global%f, CPU( 1 ), CPU( 2 )

!  clean-up data structures

      CALL CUTEST_problem_terminate( status, CUTEST_problem_global )
      IF ( status /= 0 ) GO TO 910
      DEALLOCATE( IW, STAT = ierr )
      DEALLOCATE( W, STAT = ierr )
      CALL CUTEST_cterminate( status )
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
          ' Package used            :  COBYLA ',  /,                           &
          ' Problem                 :  ', A10,    /,                           &
          ' # variables             =      ', I10 /,                           &
          ' # constraints           =      ', I10 /,                           &
          ' # objective functions   =        ', F8.2 /,                        &
          ' # constraints functions =        ', F8.2 /,                        &
          ' Final f                 = ', E15.7 /,                              &
          ' Set up time             =      ', 0P, F10.2, ' seconds' /,         &
          ' Solve time              =      ', 0P, F10.2, ' seconds' //,        &
          66('*') / )
1000 FORMAT( D12.4, /, D12.4, /,I6, /, I6 )
2110 FORMAT( /, ' The variables:', /, &
          '     i name          value    lower bound upper bound',             &
          /, ( I6, 1X, A10, 1P, 3D12.4 ) )
2120 FORMAT( /, ' The constraints:', /, &
          '     i name          value    lower bound upper bound',             &
          ' linear? ', &
          /, ( I6, 1X, A10, 1P, 3D12.4, 5X, L1 ) )
3020 FORMAT( /,'  ** Warning from COBYLA_main. **',                            &
          /,'     In the problem as stated , ', I0,                            &
            ' variables are fixed: they are changed to free.' )
3090 FORMAT( /,'  ** Warning from COBYLA_main. **',                            &
          /,'     The problem as stated includes ', I0,                        &
            ' equality constraints: they are ignored ' )

!  End of COBYLA_main

      END PROGRAM COBYLA_main

      SUBROUTINE CALCFC( n, m, X, f, C )

!  evaluates the objective function value in a format compatible with COBYLA,
!  but using the CUTEst tools.

      USE CUTEst_problem

      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )
      INTEGER, INTENT( IN ) :: n, m
      REAL( KIND = wp ), INTENT( OUT ) :: f
      REAL( KIND = wp ), INTENT( IN ) :: X( n )
      REAL( KIND = wp ), INTENT( OUT ) :: C( m )
      REAL( KIND = wp ), PARAMETER :: biginf = 9.0D+19

      INTEGER ::  mgeq, i, mt, status

!  Evaluate the objective function and constraints.

      CALL CUTEST_cfn( status, CUTEST_problem_global%n,                        &
                       CUTEST_problem_global%m, X, CUTEST_problem_global%f,    &
                       CUTEST_problem_global%C )
      IF ( status /= 0 ) GO TO 910
      f = CUTEST_problem_global%f

!  if there are equality constraints, ignore them and shift all the inequality 
!  constraint values.

      mgeq = COUNT( CUTEST_problem_global%EQUATION( : CUTEST_problem_global%m ))
      DO i = mgeq + 1, CUTEST_problem_global%m
        C( i - mgeq ) = CUTEST_problem_global%C( i )
      END DO

!  If constraints have both lower and upper bounds, they have to be included 
!  twice! Reverse the signs of less-than-or-equal-to constraints

      mt = CUTEST_problem_global%m - mgeq + 1
      DO i = 1, CUTEST_problem_global%m - mgeq
        IF ( CUTEST_problem_global%C_l( i ) > - biginf .AND.                   &
             CUTEST_problem_global%C_u( i ) < biginf ) THEN
           C( i )  = CUTEST_problem_global%C_u( i ) - C( i )
           C( mt ) = C( i ) - CUTEST_problem_global%C_l( i )
           mt = mt + 1
        ELSE IF ( CUTEST_problem_global%C_l( i ) > - biginf ) THEN
           C( i ) = C( i ) - CUTEST_problem_global%C_l( i )
        ELSE IF ( CUTEST_problem_global%C_u( i ) < biginf ) THEN
           C( i ) = CUTEST_problem_global%C_u( i ) - C( i )
        END IF
      END DO

!  include any simple bounds, including fixed variables

      DO i = 1, CUTEST_problem_global%n
        IF ( CUTEST_problem_global%X_l( i ) /=                                 &
             CUTEST_problem_global%X_u( i ) ) THEN
          IF ( CUTEST_problem_global%X_l( i ) > - biginf ) THEN
            C( mt ) = X( i ) - CUTEST_problem_global%X_l( i )
            mt = mt + 1
          END IF
          IF ( CUTEST_problem_global%X_u( i ) < biginf ) THEN
            C( mt ) = CUTEST_problem_global%X_u( i ) - X( i )
            mt = mt + 1
          END IF
        END IF
      END DO
      RETURN

  910 CONTINUE
      WRITE( 6, "( ' CUTEst error, status = ', i0, ', stopping' )") status
      STOP

!  End of CALCFC

      END SUBROUTINE CALCFC


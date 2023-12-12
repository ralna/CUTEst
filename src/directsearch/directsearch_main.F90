! THIS VERSION: CUTEST 2.2 - 2023-11-22 AT 11:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      PROGRAM DIRECTSEARCH_main

!  DIRECTSEARCH test driver for problems derived from SIF files

!  Based on MyMain.f from the Direct Search distribution

!  Nick Gould, February 2013

      USE CUTEST_KINDS_precision
      USE CUTEST_PROBLEM_precision

      IMPLICIT NONE
      INTEGER ( KIND = ip_ ) :: n, status, i, ierr
      INTEGER ( KIND = ip_ ) :: type, maxf, stype, sedge, stopc, iaux, istat
      LOGICAL :: right, stddev
      REAL( KIND = rp_ ) :: sigma, alpha, beta, gamma, stepi, stepf
      REAL( KIND = rp_ ), PARAMETER :: infty = REAL( 1.0D+19, KIND = rp_ )
      REAL( KIND = rp_ ), DIMENSION( : ), ALLOCATABLE :: X0, SSTEPI, AUX
      REAL( KIND = rp_ ), DIMENSION( 4 ) :: CPU
      REAL( KIND = rp_ ), DIMENSION( 4 ) :: CALLS
      CHARACTER ( len = 10 ) :: pname
      INTEGER ( KIND = ip_ ) :: io_buffer = 11
      INTEGER ( KIND = ip_ ), PARAMETER :: input = 55, indr = 46
      INTEGER ( KIND = ip_ ), PARAMETER :: in = 5, out = 6
      EXTERNAL :: DIRECTSEARCH_evalf

!  open the relevant file

      OPEN( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED', STATUS = 'OLD' )
      REWIND( input )

!  compute problem dimensions

      CALL CUTEST_udimen_r( status, input, n )
      IF ( status /= 0 ) GO TO 910

!   allocate space 

      ALLOCATE( X0( n ), SSTEPI( n ), AUX( n + 2 ), STAT = status )
      IF ( status /= 0 ) GO TO 990

!  set up the data structures necessary to hold the problem functions

      CALL CUTEST_usetup_r( status, input, out, io_buffer, n, X0, AUX, SSTEPI )
      IF ( status /= 0 ) GO TO 910
      CLOSE( input )

!  open the Spec file for the method

      OPEN( indr, FILE = 'DIRECTSEARCH.SPC', FORM = 'FORMATTED', STATUS = 'OLD')
      REWIND( indr )

!  read input Spec data

! type = search type (1=Coord,2=Compass,3=NLess,4=H&J,5=H&J+,6=SN&H,7=NM,8=SMS) 
! maxf = maximum evaluations allowed (-ve = no maximum) 
! stepi = initial step size for pattern search (-ve = 0.25)
! sedge = initial simplex edge lengths (1 = ask me, other = 2.0 for each)    
! stepf = final step length (-ve = 10E-8)
! stype = simplex type (2 = regular, other = right)
! stopc = stopping criteria based on (2 = delta, other =  standard deviation) 
! sigma = NM shrinking coefficient (-ve = default = 0.5.)
! alpha = NM reflection coefficient (-ve uses a default = 1.0.)
! beta = NM contraction coefficient (-ve uses a default = 0.5.)
! gamma = NM expansion coefficient (-ve uses a default = 2.0.)

!  set up algorithmic input data

      READ( indr, "( ( G8.6 ) )" ) type, maxf, stepi, sedge, stepf, stype,     &
         stopc, sigma, alpha, beta, gamma
      CLOSE ( indr )

!  ensure direct search algorithm selection is valid

      IF ( type < 1 .OR. type > 8 ) THEN
        type = 8
        WRITE( out, "( ' Invalid search type reset to ', I0 )" ) type 
      END IF

!  solve the problem using a pattern search

      IF ( type >= 1 .AND. type <= 5 ) THEN 
        CALL pattrn( type, DIRECTSEARCH_evalf, n, X0, stepi, stepf, maxf,      &
                     istat, AUX, iaux )

!  solve the propblem using a simplex search

      ELSE
        right = stype /= 2
        stddev = stopc /= 2

        IF ( sedge == 1 ) THEN
          IF ( right ) THEN
            WRITE( out, "( ' Enter n = ', I0, ' edge lengths', /,              &
           &               ' - direct search will calculate length n+1:' )" ) n
            READ( in, * ) ( SSTEPI( i ), i = 1, n )
          ELSE
            WRITE( out, "( ' Enter one edgelength of the regular simplex:' )" )
            READ( in, * ) SSTEPI( 1 )
          END IF
        END IF

!  use the simplex search based on Spendley, Hext and Himsworth

        IF ( type == 6 ) THEN
           CALL shh( DIRECTSEARCH_evalf, n, X0, right, SSTEPI, stepf, maxf,    &
                     stddev, istat, AUX, iaux )

!  use the simplex search based on Nelder and Meade

        ELSE IF ( type == 7 ) then
          CALL nm( DIRECTSEARCH_evalf, n, X0, right, sigma, alpha, beta,       &
                   gamma, SSTEPI, stepf, maxf, stddev, istat, AUX, iaux )

!  use a Sequential Multidirectional Search

        ELSE
          CALL smd( DIRECTSEARCH_evalf, n, X0, right, SSTEPI, stepf, maxf,     &
                    stddev, istat, AUX, iaux )
        END IF
      END IF
      
!     IF ( istat == 1 ) THEN
!       WRITE( out, "(' The final delta is: ', 1PE11.4)" ) AUX( 1 )
!       WRITE( out, "(' The minimum function evaluation is: ', F10.6)") AUX( 2 )
!       WRITE( out, "(' The corresponding X is: ', /, ( 5ES12.4 )" )           &
!         AUX( 3 : n + 2 )
!       WRITE( out, "(' The number of evaluations was ', I0 )" ) iaux
!     END IF

!  output report

      CALL CUTEST_ureport_r( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910

      CALL CUTEST_probname_r( status, pname )
      WRITE( out, 2000 ) pname, n, CALLS( 1 ), istat, AUX( 2 ), iaux,         &
        CPU( 1 ), CPU( 2 )

!  clean-up data structures

      DEALLOCATE( X0, AUX, SSTEPI, STAT = ierr )
      CALL CUTEST_cterminate_r( status )
      STOP

!  error returns

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP

!  Non-executable statements

2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //,                    &
          ' Package used            :  DIRECTSEARCH',  /,                      &
          ' Problem                 :  ', A10,    /,                           &
          ' # variables             =      ', I10 /,                           &
          ' # objective functions   =        ', F8.2 /,                        &
          ' Exit code               =      ', I10 /,                           &
          ' Final f                 = ', E15.7 /,                              &
          ' # iterations            =      ', I10 /,                           &
          ' Set up time             =      ', 0P, F10.2, ' seconds' /,         &
          ' Solve time              =      ', 0P, F10.2, ' seconds' //,        &
          66('*') / )

!  End of DIRECTSEARCH_main

      END PROGRAM DIRECTSEARCH_main

      SUBROUTINE DIRECTSEARCH_evalf( X, f, n )

!  evaluates the objective function value in a format compatible with 
!  DIRECTSEARCH, but using the CUTEst tools.

      USE CUTEST_KINDS_precision
      USE CUTEST_PROBLEM_precision

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
      REAL( KIND = rp_ ), INTENT( OUT ) :: f
      REAL( KIND = rp_ ), INTENT( IN ) :: X( n )

      INTEGER ::  status

!  Evaluate the objective function and constraints.

      CUTEST_problem_global%n = n
      CALL CUTEST_ufn_r( status, CUTEST_problem_global%n,                      &
                         X, CUTEST_problem_global%f )
      IF ( status /= 0 ) GO TO 910
      f = CUTEST_problem_global%f
      RETURN

  910 CONTINUE
      WRITE( 6, "( ' CUTEst error, status = ', I0, ', stopping' )" ) status
      STOP

!  End of DIRECTSEARCH_evalf

      END SUBROUTINE DIRECTSEARCH_evalf

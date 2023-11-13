! THIS VERSION: CUTEST 2.2 - 2023-11-12 AT 10:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-*-*-*- C U T E S T _ P R O B L E M     M O D U l E -*-*-*-*-*-*-*-

!  Copyright reserved, Bongartz/Conn/Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Nick Gould and Dominique Orban

!  History -
!   Released in CUTEr, April 2004
!   Updated fortran 2003 version released January 2013

!  For full documentation, see 
!   http://galahad.rl.ac.uk/galahad-www/specs.html

    MODULE CUTEST_PROBLEM_precision

      USE CUTEST_KINDS_precision
      USE CUTEST_precision

      IMPLICIT None

      PRIVATE
      PUBLIC :: CUTEST_problem_setup_r, CUTEST_problem_terminate_r

!----------------------
!   P a r a m e t e r s
!----------------------

      INTEGER ( KIND = ip_ ), PARAMETER :: error_device = 6

!  ====================================
!  The CUTEST_problem_type derived type
!  ====================================

      TYPE, PUBLIC :: CUTEST_problem_type
        INTEGER ( KIND = ip_ ) :: n = 0          ! Number of variables
        INTEGER ( KIND = ip_ ) :: m = 0          ! Number of general constraints
        CHARACTER ( LEN = 10 ) :: pname    ! Problem name
        REAL ( KIND = rp_ ) :: f   ! objective function
        CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: vnames
        CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: cnames

! Vector of variables and lower and upper bounds

        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: x  
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: x_l
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: x_u

! Vector of dual variables associated to bound constraints

        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: z

! Vector of constraints and lower and upper bounds

        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: c  
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: c_l
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: c_u

! Lagrange multipliers associated with general constraints

        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: y

! Constraint indicators

        LOGICAL, ALLOCATABLE, DIMENSION( : ) :: equation
        LOGICAL, ALLOCATABLE, DIMENSION( : ) :: linear  

! Gradient of the Lagrangian

        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: g

! sparse Hessian storage (coordinate)

        Logical :: allocate_H = .False.
        INTEGER ( KIND = ip_ ) :: nnzh = 0
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: H_row
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: H_col
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: H_val

! sparse Jacobian storage (coordinate)

        Logical :: allocate_J = .False.
        INTEGER ( KIND = ip_ ) :: nnzj = 0
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: J_row
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: J_col
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: J_val
      END TYPE CUTEST_problem_type

!--------------------------------
!  G l o b a l  v a r i a b l e s
!--------------------------------

      TYPE ( CUTEST_problem_type ), SAVE, PUBLIC :: CUTEST_problem_global

!============================================================================

    CONTAINS

      SUBROUTINE CUTEST_problem_setup_r( status, problem, input )
!
! Allocate main problem data structure
!
    Implicit None
!
      TYPE( CUTEST_problem_type ), INTENT( INOUT ) :: problem
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: input
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status

      INTEGER ( KIND = ip_ ) :: ierr, nerr
!
! Obtain problem dimensions
! 
      CALL CUTEST_cdimen_r( status, input, problem%n, problem%m )
      IF ( status /= 0 ) RETURN
!
! Trap invalid input
!
      IF ( problem%n <= 0 .Or. problem%m < 0 ) THEN
         WRITE( error_device, "( 'CUTEST_problem_setup:: invalid problem ',    &
        &  'dimensions: n = ', I0, ', m = ', I0 )" ) problem%n, problem%m
         Call CUTEST_problem_terminate_r( status, problem )
         RETURN
      ENDIF
!
! Allocate arrays depending on n
!
      ierr = 0
      nerr = 0
      ALLOCATE( problem%vnames( problem%n ), STAT = ierr )
      IF ( ierr /= 0 ) Then
         nerr = nerr + 1
      END IF
      ALLOCATE( problem%x( problem%n ), STAT = ierr )
      IF ( ierr /= 0 ) Then
         nerr = nerr + 1
      END IF
      ALLOCATE( problem%x_l( problem%n ), STAT = ierr )
      IF ( ierr /= 0 ) Then
         nerr = nerr + 1
      END IF
      ALLOCATE( problem%x_u( problem%n ), STAT = ierr )
      IF ( ierr /= 0 ) Then
         nerr = nerr + 1
      END IF
      ALLOCATE( problem%z( problem%n ), STAT = ierr )
      IF ( ierr /= 0 ) Then
         nerr = nerr + 1
      END IF
!
! Allocate memory for Hessian if required
!
      IF ( problem%allocate_H ) Then
         Call CUTEST_cdimsh_r( status, problem%nnzh )
         IF ( status /= 0 ) RETURN
         ALLOCATE( problem%H_row( problem%nnzh ), STAT = ierr )
         IF ( ierr /= 0 ) Then
            nerr = nerr + 1
         END IF
         ALLOCATE( problem%H_col( problem%nnzh ), STAT = ierr )
         IF ( ierr /= 0 ) Then
            nerr = nerr + 1
         END IF
         ALLOCATE( problem%H_val( problem%nnzh ), STAT = ierr )
         IF ( ierr /= 0 ) Then
            nerr = nerr + 1
         END IF
      END IF
!
! Allocate arrays depending on m
!
      IF ( problem%m > 0 ) Then
         ALLOCATE( problem%cnames( problem%m ), STAT = ierr )
         IF ( ierr /= 0 ) Then
            nerr = nerr + 1
         END IF
         ALLOCATE( problem%c( problem%m ), STAT = ierr )
         IF ( ierr /= 0 ) Then
            nerr = nerr + 1
         END IF
         ALLOCATE( problem%c_l( problem%m ), STAT = ierr )
         IF ( ierr /= 0 ) Then
            nerr = nerr + 1
         END IF
         ALLOCATE( problem%c_u( problem%m ), STAT = ierr )
         IF ( ierr /= 0 ) Then
            nerr = nerr + 1
         END IF
         ALLOCATE( problem%y( problem%m ), STAT = ierr )
         IF ( ierr /= 0 ) Then
            nerr = nerr + 1
         END IF
         ALLOCATE( problem%equation( problem%m ), STAT = ierr )
         IF ( ierr /= 0 ) Then
            nerr = nerr + 1
         END IF
         ALLOCATE( problem%linear( problem%m ), STAT = ierr )
         IF ( ierr /= 0 ) Then
            nerr = nerr + 1
         END IF
!
! Allocate memory for Jacobian if required
!
         IF ( problem%allocate_J ) Then
            CALL CUTEST_cdimsj_r( status, problem%nnzj )
            ALLOCATE( problem%J_row( problem%nnzj ), STAT = ierr )
            IF ( ierr /= 0 ) Then
               nerr = nerr + 1
            END IF
            ALLOCATE( problem%J_col( problem%nnzj ), STAT = ierr )
            IF ( ierr /= 0 ) Then
               nerr = nerr + 1
            END IF
            ALLOCATE( problem%J_val( problem%nnzj ), STAT = ierr )
            IF ( ierr /= 0 ) Then
               nerr = nerr + 1
            END IF
         END IF
      END IF
!
! Report errors if any
!
      IF ( nerr > 0 ) THEN
         WRITE( error_device, "( 'CUTEST_problem_setup:: ', I0,                &
        &  ' errors in memory allocation' )" ) nerr
        status = 1
      END IF

!  end of subroutine CUTEST_problem_setup_r

      END SUBROUTINE CUTEST_problem_setup_r

  !============================================================================

      SUBROUTINE CUTEST_problem_terminate_r( status, problem )

! Deallocates dynamically-allocated memory for problem storage

      IMPLICIT NONE

      INTEGER ( KIND = ip_ ), INTENT( out ) :: status
      TYPE( CUTEST_problem_type ), INTENT( INOUT ) :: problem
      INTEGER ( KIND = ip_ ) :: ierr, nerr

      nerr = 0 ;  status = 0

      IF ( ALLOCATED( problem%vnames ) ) Then
         DEALLOCATE( problem%vnames, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%cnames ) ) Then
         DEALLOCATE( problem%cnames, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%x ) ) Then
         DEALLOCATE( problem%x, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%x_l ) ) Then
         DEALLOCATE( problem%x_l, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%x_u ) ) Then
         DEALLOCATE( problem%x_u, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%z ) ) Then
         DEALLOCATE( problem%z, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%c ) ) Then
         DEALLOCATE( problem%c, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%c_l ) ) Then
         DEALLOCATE( problem%c_l, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%c_u ) ) Then
         DEALLOCATE( problem%c_u, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%y ) ) Then
         DEALLOCATE( problem%y, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%equation ) ) Then
         DEALLOCATE( problem%equation, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%linear ) ) Then
         DEALLOCATE( problem%linear, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%g ) ) Then
         DEALLOCATE( problem%g, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%H_row ) ) Then
         DEALLOCATE( problem%H_row, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%H_col ) ) Then
         DEALLOCATE( problem%H_col, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%H_val ) ) Then
         DEALLOCATE( problem%H_val, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%J_row ) ) Then
         DEALLOCATE( problem%J_row, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%J_col ) ) Then
         DEALLOCATE( problem%J_col, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF
      IF ( ALLOCATED( problem%J_val ) ) Then
         DEALLOCATE( problem%J_val, STAT = ierr )
         IF ( ierr /= 0 ) nerr = nerr + 1
      END IF

      IF ( nerr > 0 ) THEN
        WRITE( error_device,                                                   &
           "( I0, ' errors encountered in freeing memory' )" ) nerr
        status = 1
      END IF

!  end of subroutine CUTEST_problem_terminate_r

      End Subroutine CUTEST_problem_terminate_r

    END MODULE CUTEST_PROBLEM_precision

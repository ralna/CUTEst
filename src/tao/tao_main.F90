! THIS VERSION: CUTEST 2.2 - 2023-12-11 AT 08:00 GMT.

! Driver program for solving a .SIF example problem with TAO 
! (after sifdecode has been used on the .SIF file)

#include "cutest_modules.h"
#include "cutest_routines.h"

     MODULE CUTEST_TAO
      USE CUTEST_KINDS_precision
       TYPE, PUBLIC :: TAO_data_type
         INTEGER ( KIND = ip_ ) :: n, m, nnzh, nnzj
         INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: H_row, H_col
         INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: J_row, J_col
         REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: H_val
         REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: J_val
         REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: Y
       END TYPE TAO_data_type
     END MODULE CUTEST_TAO

! --------------------------  M A I N  P R O G R A M  ------------------------

      PROGRAM tao_main

#include "petsc/finclude/petsctao.h"
      USE petsctao
      USE CUTEST_KINDS_precision
      USE CUTEST_TAO
      IMPLICIT NONE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!                   Variable declarations
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      PetscErrorCode  ierr     ! used to check for functions returning nonzeros
      TYPE ( tVec ) :: x       ! solution vector
      TYPE ( tVec ) :: r       ! residual vector
      TYPE ( tVec ) :: xl, xu  ! vectors of bounds
      TYPE ( tMat ) :: H       ! Hessian matrix
      TYPE ( tMat ) :: J       ! Jacobian matrix
      TYPE ( tTao ) :: tao     ! TAO_SOVER context
      TaoType ::  tao_method   ! solver used
      PetscBool flg
      PetscMPIInt comm_size
      PetscReal, POINTER, DIMENSION( : ) :: x_array, xl_array, xu_array
      PetscInt :: n, m, nnzh, nnzj
      TYPE ( TAO_data_type ) :: tao_data
      EXTERNAL FormFunction, FormGradient, FormFunctionGradient, FormHessian, &
               FormResidual, FormJacobian

      INTEGER ( KIND = ip_ ) :: i, jj, l, status
      INTEGER ( KIND = ip_ ), PARAMETER :: input = 55, out = 6, inspec = 46
      INTEGER ( KIND = ip_ ), PARAMETER :: io_buffer = 11
      CHARACTER ( LEN = 10 ) :: method = REPEAT( ' ', 10 )
      CHARACTER ( LEN = 10 ) :: pname
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: XNAMES
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: H_ptr, J_ptr
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: Y, C_l, C_u
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATION, LINEAR

      REAL ( KIND = rp_ ) :: CALLS( 4 ), TIME( 4 )
      PetscReal :: gatol, grtol, gttol, radius, weight
      PetscInt :: max_evals, max_its, sol_comp

      LOGICAL :: least_squares, noobj
      INTEGER :: iter, reason
      REAL ( kind = rp_ ) f_min, g_norm, c_norm, x_diff

!  open the solver specification file to set parameters from a file

      OPEN( inspec, FILE = 'TAO.SPC', FORM = 'FORMATTED', STATUS = 'OLD' )

!  read input specification data (*)

!  method      <-- optimization method employed (**)
!  gatol       <-- stop if ||g(x)|| <= gatol
!  grtol       <-- stop if ||g(x)|| / |f(x)| <= grtol
!  gttol       <-- stop if ||g(x)|| / ||g(x_0)|| <= gttol
!  radius      <-- the initial trust-region radius
!  weight      <-- the regularization weight for least-squares problems
!  max_evals   <-- the maximum number of function evaluations
!  max_its     <-- the maximum number of iterations
!  sol_comp    <-- print compomnents 1:sol & n-sol+1:n of the solution (0=none)

!  (*)  any negative value keeps default
!  (**) may be one of: 
!         blmvm, bncg, bnls, bntl, bntr, bqnkls, bqnktl, bqnktr, 
!         bqnls, lmvm, nls, ntl, ntr or tron (brgn to come in future)
!        
!       see entry TAO"METHOD" on https://petsc.org/main/manualpages/Tao/
!       for details about "method"

      READ ( inspec, "( A10, /, 5( D10.3, / ), I10, /, I10, /, I10 )" )        &
        method, gatol, grtol, gttol, radius, weight,                           &
        max_evals, max_its, sol_comp

!  close input file

      CLOSE ( inspec )

!  initialize TAO and PETSc

      CALL PetscInitialize( ierr )
      CALL MPI_Comm_size( PETSC_COMM_WORLD, comm_size, ierr )

!  check not available in fortran
!     CALL PetscCheck( comm_size == 1, PETSC_COMM_SELF,                        &
!                      PETSC_ERR_WRONG_MPI_SIZE,                               &
!                      'This is a uniprocessor example only' )

!  open the input data file

      OPEN ( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED', STATUS = 'OLD' )
      REWIND input

!  is a least-squares solver required? Get the dimension size 

      SELECT CASE ( TRIM( ADJUSTL( method ) ) )
      CASE( "brgn" )
        least_squares = .TRUE.
        CALL CUTEST_cdimen_r( status, input, n, m )
        IF ( status /= 0 ) GO TO 910
        IF ( m <= 0 ) THEN
          WRITE( out, "( ' least squares solver invoked for problem with no',  &
         & ' residuals ... stopping' )" )
          STOP
        END IF
        tao_data%m = m
        CALL CUTEST_cnoobj_r( status, input, noobj )
        IF ( status /= 0 ) GO TO 910
        IF ( .NOT. noobj ) THEN
          WRITE( out, "( ' least squares solver invoked for problem',          &
         & ' with explicit objective ... stopping' )" )
          STOP
        END IF
      CASE DEFAULT
        least_squares = .FALSE.
        CALL CUTEST_udimen_r( status, input, n )
        IF ( status /= 0 ) GO TO 910
      END SELECT
      tao_data%n = n

!  check for command line arguments to override defaults

      CALL PetscOptionsGetInt( PETSC_NULL_OPTIONS,                             &
                               PETSC_NULL_CHARACTER, '-n', n, flg, ierr )

!  allocate vectors for the solution and its bounds

      CALL VecCreateSeq( PETSC_COMM_SELF, n, x, ierr )
      CALL VecDuplicate( x, xl, ierr )
      CALL VecDuplicate( x, xu, ierr )

!  create fortran equivalents

      CALL VecGetArrayF90( x, x_array, ierr )
      CALL VecGetArrayF90( xl, xl_array, ierr )
      CALL VecGetArrayF90( xu, xu_array, ierr )

!  compute the lower and upper bounds on variables and initial values 

      IF ( least_squares ) THEN
        ALLOCATE( Y( m ), C_l( m ), C_u( m ), EQUATION( m ), LINEAR( m ),      &
                  STAT = status )
        IF ( status /= 0 ) GO TO 910
        CALL CUTEST_csetup( status, input, out, io_buffer, n, m,               &
                            x_array, xl_array, xu_array,                       &
                            Y, C_l, C_u, EQUATION, LINEAR, 0, 0, 0 )
        DEALLOCATE( Y, C_l, C_u, EQUATION, LINEAR, STAT = status )
        IF ( status /= 0 ) GO TO 910
      ELSE
        CALL CUTEST_usetup_r( status, input, out, io_buffer,                   &
                              n, x_array, xl_array, xu_array )
      END IF
      IF ( status /= 0 ) GO TO 910
      
      DO i = 1, n
        IF ( xl_array( i ) > xu_array( i ) ) THEN
          WRITE( out, "( ' Bad vector bounds' )" )
          stop
        ELSE IF ( x_array( i ) < xl_array( i ) )  THEN
          x_array( i ) = xl_array( i )
        ELSE IF ( x_array( i ) > xu_array( i ) ) THEN
          x_array( i ) = xu_array( i )
        END IF
      END DO

!  record the name of the problem and the variables

      CALL CUTEST_probname_r( status, pname )
      IF ( status /= 0 ) GO TO 910
      ALLOCATE( XNAMES( n ), STAT = status )
      IF ( status /= 0 ) GO TO 910
      CALL CUTEST_varnames_r( status, n, XNAMES )
      IF ( status /= 0 ) GO TO 910

!  restore the petsc vectors

      CALL VecRestoreArray( x, x_array, ierr )

!  compute the number of nonzeros in the residual Jacobian, and allocate 
!  storage space 

      IF ( least_squares ) THEN
        CALL CUTEST_cdimsj_r( status, nnzj )
        tao_data%nnzj = nnzj
        ALLOCATE( tao_data%J_row( nnzj ), tao_data%J_col( nnzj ),              &
                  tao_data%J_val( nnzj ), tao_data%Y( m ),                     &
                  J_ptr( m ), STAT = status )
        IF ( status /= 0 ) GO TO 910

!  find the row and column indices (symmetric storage)

        CALL CUTEST_csgrp( status, n, nnzj, nnzj,                              &
                           tao_data%J_row, tao_data%J_col )
        IF ( status /= 0 ) GO TO 910

!  compute the number of nonzeros in each row (unsymmetric storage)

        J_ptr( : m ) = 0
        DO l = 1, nnzh
          i = tao_data%J_row( l )
          IF ( i > 0 ) J_ptr( i ) = J_ptr( i ) + 1
        END DO

!  create the petsc Jacobian

        CALL MatCreateSeqAIJ( PETSC_COMM_SELF, m, n, PETSC_DEFAULT_INTEGER,    &
                              J_ptr, J, ierr )
        DEALLOCATE( J_ptr, STAT = status )

!  compute the number of nonzeros in the objective Hessian, and allocate 
!  storage space 

      ELSE
        CALL CUTEST_udimsh_r( status, nnzh )
        tao_data%nnzh = nnzh
        ALLOCATE( tao_data%H_row( nnzh ), tao_data%H_col( nnzh ),              &
                  tao_data%H_val( nnzh ), H_ptr( n ), STAT = status )
        IF ( status /= 0 ) GO TO 910

!  find the row and column indices (symmetric storage)

        CALL CUTEST_ushp( status, n, nnzh, l, tao_data%H_row, tao_data%H_col )
        IF ( status /= 0 ) GO TO 910

!  compute the number of nonzeros in each row (unsymmetric storage)

        H_ptr( : n ) = 0
        DO l = 1, nnzh
          i = tao_data%H_row( l ) ; jj = tao_data%H_col( l )
          H_ptr( i ) = H_ptr( i ) + 1
          IF ( i /= jj ) H_ptr( jj ) = H_ptr( jj ) + 1
        END DO

!  create the petsc Hessian

        CALL MatCreateSeqAIJ( PETSC_COMM_SELF, n, n, PETSC_DEFAULT_INTEGER,    &
                              H_ptr, H, ierr )
        CALL MatSetOption( H, MAT_SYMMETRIC, PETSC_TRUE, ierr )
        DEALLOCATE( H_ptr, STAT = status )
      END IF

!  ------------------------
!  The TAO code begins here
!  ------------------------

!  Create TAO solver

      CALL TaoCreate( PETSC_COMM_SELF, tao, ierr )

!  specify the optimization method to be employed

      SELECT CASE ( TRIM( ADJUSTL( method ) ) )
      CASE( "blmvm" )
        tao_method = TAOBLMVM
      CASE( "bncg" )
        tao_method = TAOBNCG
      CASE( "bnls" )
        tao_method = TAOBNLS
      CASE( "bntl" )
        tao_method = TAOBNTL
      CASE( "bntr" )
        tao_method = TAOBNTR
      CASE( "bqnkls" )
        tao_method = TAOBQNKLS
      CASE( "bqnktl" )
        tao_method = TAOBQNKTL
      CASE( "bqnktr" )
        tao_method = TAOBQNKTR
      CASE( "bqnls" )
        tao_method = TAOBQNLS
      CASE( "lmvm" )
        tao_method = TAOLMVM
      CASE( "nls" )
        tao_method = TAONLS
      CASE( "ntl" )
        tao_method = TAONTL
      CASE( "ntr" )
        tao_method = TAONTR
      CASE( "tron" )
        tao_method = TAOTRON
      CASE( "brgn" )
        tao_method = TAOBRGN
      CASE DEFAULT
        WRITE( out, "( ' Method ', A, ' not recognised, stopping' )" )        &
          TRIM( ADJUSTL( method ) )
        STOP
      END SELECT

      CALL TaoSetType( tao, tao_method, ierr )

!  set algorithmic parameters

      IF ( gatol < 0.0 ) THEN
        IF ( grtol < 0.0 ) THEN
          IF ( gttol < 0.0 ) THEN
            CALL TaoSetTolerances( tao, PETSC_DEFAULT_REAL,                    &
                                        PETSC_DEFAULT_REAL,                    &
                                        PETSC_DEFAULT_REAL, ierr )
          ELSE
            CALL TaoSetTolerances( tao, PETSC_DEFAULT_REAL,                    &
                                        PETSC_DEFAULT_REAL,                    &
                                        gttol, ierr )
          END IF
        ELSE
          IF ( gttol < 0.0 ) THEN
            CALL TaoSetTolerances( tao, PETSC_DEFAULT_REAL,                    &
                                        grtol,                                 &
                                        PETSC_DEFAULT_REAL, ierr )
          ELSE
            CALL TaoSetTolerances( tao, PETSC_DEFAULT_REAL,                    &
                                        grtol,                                 &
                                        gttol, ierr )
          END IF
        END IF
      ELSE
        IF ( grtol < 0.0 ) THEN
          IF ( gttol < 0.0 ) THEN
            CALL TaoSetTolerances( tao, gatol,                                 &
                                        PETSC_DEFAULT_REAL,                    &
                                        PETSC_DEFAULT_REAL, ierr )
          ELSE
            CALL TaoSetTolerances( tao, gatol,                                 &
                                        PETSC_DEFAULT_REAL,                    &
                                        gttol, ierr )
          END IF
        ELSE
          IF ( gttol < 0.0 ) THEN
            CALL TaoSetTolerances( tao, gatol,                                 &
                                        grtol,                                 &
                                        PETSC_DEFAULT_REAL, ierr )
          ELSE
            CALL TaoSetTolerances( tao, gatol,                                 &
                                        grtol,                                 &
                                        gttol, ierr )
          END IF
        END IF
      END IF

      IF ( max_evals >= 0 )                                                    &
        CALL TaoSetMaximumFunctionEvaluations( tao, max_evals, ierr )
      IF ( max_its >= 0 )                                                      &
        CALL TaoSetMaximumIterations( tao, max_its, ierr )
      IF ( radius > 0.0_rp_ )                                                  &
        CALL TaoSetInitialTrustRegionRadius( tao, radius, ierr )
      IF ( least_squares .AND. weight < 0.0 )                                  &
        CALL TaoBRGNSetRegularizerWeight( tao, weight, ierr )

!  set routines for function, gradient, Hessian, residual and Jacobian
!  evaluation as necessary

      IF ( least_squares ) THEN
        CALL VecCreateSeq( PETSC_COMM_SELF, m, r, ierr )
        CALL TaoSetResidualRoutine( tao, r, FormResidual, tao_data, ierr )
        CALL TaoSetJacobianResidualRoutine( tao, J, J,                         &
                                            FormJacobian, tao_data, ierr )
      ELSE
        CALL TaoSetObjective( tao, FormFunction, tao_data, ierr )
        CALL TaoSetGradient( tao, PETSC_NULL_VEC, FormGradient, tao_data, ierr )
        CALL TaoSetObjectiveAndGradient( tao, PETSC_NULL_VEC,                  &
                                         FormFunctionGradient, tao_data, ierr )
        CALL TaoSetHessian( tao, H, H, FormHessian, tao_data, ierr )
      END IF

!  set lower and upper bounds on the variables

      CALL TaoSetVariableBounds( tao, xl, xu, ierr )

!  optional: Set initial guess

      CALL TaoSetSolution( tao, x, ierr )

!  Check for TAO command line options

      CALL TaoSetFromOptions( tao, ierr )

!  Solve the application

      CALL TaoSolve( tao, ierr )

!  TaoView() prints ierr about the TAO solver; the option
!      -tao_view
!  can alternatively be used to activate this at runtime.
!      CALL TaoView( tao, PETSC_VIEWER_STDOUT_SELF, ierr )
!   Get information on termination

       call TaoGetSolutionStatus( tao, iter, f_min, g_norm, c_norm, x_diff,    &
                                  reason, ierr )
       IF ( reason < 0 )                                                       &
          WRITE( out, "( 'TAO did not terminate successfully' )" )

!  print segments of the solution if required

      IF ( sol_comp > 0 ) THEN
        CALL TaoGetSolution( tao, x, ierr )
        CALL VecGetArrayF90( x, x_array, ierr )
        WRITE( out, "( /, 79( '*' ), // ' Solution:', //, '       i name  ',   &
       &  '            lower               value               upper ' )" )
        DO i = 1, MIN( n, sol_comp )
          WRITE( out, 2000 ) &
            i, XNAMES( i ), xl_array( i ), x_array( i ),  xu_array( i )
        END DO
        IF ( 2 * sol_comp < n + 1 ) THEN
          IF ( 2 * sol_comp < n ) &
          WRITE( out, "( '       - -         ', 3( 2X, 18( '-' ) ) )" )
          DO i = n - sol_comp + 1, n
            WRITE( out, 2000 ) &
              i, XNAMES( i ), xl_array( i ), x_array( i ),  xu_array( i )
          END DO
        END IF
      END IF

!  record the evaluation statistics

      CALL CUTEST_ureport_r( status, CALLS, TIME )
      IF ( status /= 0 ) GO TO 910

      WRITE ( out, "( /, 30( '*' ), ' CUTEst statistics ', 30('*'), //,        &
     &    ' Package used            :  TAO (', A, ')', /,                      &
     &    ' Problem                 :  ', A10,  /,                             &
     &    ' # variables             =           ', I10, /,                     &
     &    ' # function evaluations  =             ', F8.2, /,                  &
     &    ' # gradient evaluations  =             ', F8.2, /,                  &
     &    ' # Hessian evaluations   =             ', F8.2, /,                  &
     &    ' # iterations            =           ', I10, /,                     &
     &    ' Package exit code       =           ', I10, /,                     &
     &    ' Final f                 = ', ES20.12, /,                           &
     &    ' Final ||g||             = ', ES20.12, /,                           &
     &    ' Set up time             =           ', 0P, F10.2, ' seconds', /,   &
     &    ' Solve time              =           ', 0P, F10.2, ' seconds', //,  &
     &     79( '*' ), / )" ) TRIM( ADJUSTL( method ) ), pname, n,              &
             ( CALLS( i ), i = 1, 3 ), iter, reason, f_min, g_norm,            &
             TIME( 1 ), TIME( 2 )

!  free TAO data structures

      CALL TaoDestroy( tao, ierr )

!  free PETSc and CUTESt data structures

      CALL VecDestroy( x, ierr )
      CALL VecDestroy( xl, ierr )
      CALL VecDestroy( xu, ierr )

      IF ( least_squares ) THEN
        CALL VecDestroy( r, ierr )
        CALL MatDestroy( J, ierr )
        DEALLOCATE( tao_data%J_row, tao_data%J_col,                            &
                    tao_data%J_val, tao_data%Y, STAT = status )
        CALL CUTEST_cterminate_r( status )
      ELSE
        CALL MatDestroy( H, ierr )
        DEALLOCATE( tao_data%H_row, tao_data%H_col,                            &
                    tao_data%H_val, STAT = status )
        CALL CUTEST_uterminate_r( status )
      END IF

      CALL PetscFinalize( ierr )

      STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )" ) status
      STOP

!  non-executable statement

 2000 FORMAT( I8, 1X, A10, 3ES20.12 )

      END PROGRAM tao_main

! --------------------  E N D   O F  M A I N  P R O G R A M  ------------------

      SUBROUTINE FormFunction( tao, X, f, tao_data, ierr )

!  FormFunctionGradient - Evaluates the function f(X)
!
!  Input Parameters:
!  tao - the Tao context
!  X   - input vector
!  tao_data - private data
!
!  Output Parameters:
!  f - function value

#include "petsc/finclude/petsctao.h"

      USE petsctao
      USE CUTEST_KINDS_precision
      USE CUTEST_TAO
      IMPLICIT NONE

!  dummy arguments

      TYPE( tTao ) :: tao
      TYPE( tVec ) :: X
      PetscReal :: f
      PetscErrorCode :: ierr
      TYPE ( TAO_data_type ) :: tao_data

!  local variables

      PetscReal, POINTER, DIMENSION( : ) :: x_array
      PetscInt :: n

      n = tao_data%n

!  get pointers to vector data

      CALL VecGetArrayReadF90( X, x_array, ierr )

!  compute g(x)

      CALL CUTEST_ufn_r( ierr, n, x_array, f )

!  restore vectors

      CALL VecRestoreArrayReadF90( X, x_array, ierr )

!     CALL PetscLogFlops( 15.0d0*nn, ierr )

      RETURN
      END SUBROUTINE FormFunction

      SUBROUTINE FormGradient( tao, X, G, tao_data, ierr )

!  FormFunctionGradient - Evaluates the gradient G(X)
!
!  Input Parameters:
!  tao - the Tao context
!  X   - input vector
!  tao_data - private data
!
!  Output Parameters:
!  G - vector containing the newly evaluated gradient

#include "petsc/finclude/petsctao.h"

      USE petsctao
      USE CUTEST_KINDS_precision
      USE CUTEST_TAO
      IMPLICIT NONE

!  dummy arguments

      TYPE( tTao ) :: tao
      TYPE( tVec ) :: X, G
      PetscErrorCode :: ierr
      TYPE ( TAO_data_type ) :: tao_data

!  local variables

      PetscReal, POINTER, DIMENSION( : ) :: g_array, x_array
      PetscInt :: n

      n = tao_data%n

!  get pointers to vector data

      CALL VecGetArrayReadF90( X, x_array, ierr )
      CALL VecGetArrayF90( G, g_array, ierr )

!  compute g(x)

      CALL CUTEST_ugr_r( ierr, n, x_array, g_array )

!  restore vectors

      CALL VecRestoreArrayReadF90( X, x_array, ierr )
      CALL VecRestoreArrayF90( G, g_array, ierr )

!     CALL PetscLogFlops( 15.0d0*nn, ierr )

      RETURN
      END SUBROUTINE FormGradient

      SUBROUTINE FormFunctionGradient( tao, X, f, G, tao_data, ierr )

!  FormFunctionGradient - Evaluates the function f(X) and gradient G(X)
!
!  Input Parameters:
!  tao - the Tao context
!  X   - input vector
!  tao_data - private data
!
!  Output Parameters:
!  f - function value
!  G - vector containing the newly evaluated gradient

#include "petsc/finclude/petsctao.h"

      USE petsctao
      USE CUTEST_KINDS_precision
      USE CUTEST_TAO
      IMPLICIT NONE

!  dummy arguments

      TYPE( tTao ) :: tao
      TYPE( tVec ) :: X, G
      PetscReal :: f
      PetscErrorCode :: ierr
      TYPE ( TAO_data_type ) :: tao_data

!  local variables

      PetscReal, POINTER, DIMENSION( : ) :: g_array, x_array
      PetscInt :: n

      n = tao_data%n

!  get pointers to vector data

      CALL VecGetArrayReadF90( X, x_array, ierr )
      CALL VecGetArrayF90( G, g_array, ierr )

!  compute g(x)

      CALL CUTEST_uofg_r( ierr, n, x_array, f, g_array, .TRUE. )

!  restore vectors

      CALL VecRestoreArrayReadF90( X, x_array, ierr )
      CALL VecRestoreArrayF90( G, g_array, ierr )

!     CALL PetscLogFlops( 15.0d0*nn, ierr )

      RETURN
      END SUBROUTINE FormFunctionGradient

      SUBROUTINE FormHessian( tao, X, H, PrecH, tao_data, ierr )

!  FormHessian - Evaluates Hessian matrix.
!
!  Input Parameters:
!  tao     - the Tao context
!  X       - input vector
!  tao_data - private data
!
!  Output Parameters:
!  H      - Hessian matrix
!  PrecH  - optionally different preconditioning matrix (not used here)
!  flag   - flag indicating matrix structure
!  ierr   - error code
!
!  Note: Providing the Hessian may not be necessary.  Only some solvers
!  require this matrix.

#include "petsc/finclude/petsctao.h"

      USE petsctao
      USE CUTEST_KINDS_precision
      USE CUTEST_TAO
      IMPLICIT NONE

!  dummy arguments

      TYPE ( tTao ) :: tao
      TYPE ( tVec ) :: X
      TYPE ( tMat ) :: H, PrecH
      PetscErrorCode   ierr
      TYPE ( TAO_data_type ) :: tao_data

!  local variables

      PetscReal, POINTER, DIMENSION( : ) :: x_array
      PetscInt :: i, j, l, n, nnzh
      PetscReal :: val

      n = tao_data%n ; nnzh = tao_data%nnzh

!  get a pointer to vector data

      CALL VecGetArrayReadF90( X, x_array, ierr )

!  compute Hessian entries at x

      CALL CUTEST_ush_r( ierr, n, x_array, nnzh, nnzh, tao_data%H_val,         &
                         tao_data%H_row, tao_data%H_col )
      IF ( ierr /= 0 ) RETURN

!  restore vector

      CALL VecRestoreArrayReadF90( X, x_array, ierr )

!  insert the Hessian values into petsc storage

      DO l = 1, nnzh
        i = tao_data%H_row( l ) - 1 ; j = tao_data%H_col( l ) - 1
        val = tao_data%H_val( l )
        CALL MatSetValue( H, i, j, val, INSERT_VALUES, ierr )
        IF ( i /= j ) CALL MatSetValue( H, j, i, val, INSERT_VALUES, ierr )
      END DO

!  assemble matrix

      CALL MatAssemblyBegin( H, MAT_FINAL_ASSEMBLY, ierr )
      CALL MatAssemblyEnd( H, MAT_FINAL_ASSEMBLY, ierr )

!     CALL PetscLogFlops(9.0d0*nn,ierr)

      RETURN
      END SUBROUTINE FormHessian

      SUBROUTINE FormResidual( tao, X, R, tao_data, ierr )

!  FormFunctionGradient - Evaluates the gradient R(X)
!
!  Input Parameters:
!  tao - the Tao context
!  X   - input vector
!  tao_data - private data
!
!  Output Parameters:
!  R - vector containing the newly evaluated residual

#include "petsc/finclude/petsctao.h"

      USE petsctao
      USE CUTEST_KINDS_precision
      USE CUTEST_TAO
      IMPLICIT NONE

!  dummy arguments

      TYPE ( tTao ) :: tao
      TYPE ( tVec ) :: X, R
      PetscErrorCode :: ierr
      TYPE ( TAO_data_type ) :: tao_data

!  local variables

      PetscReal :: f
      PetscReal, POINTER, DIMENSION( : ) :: r_array, x_array
      PetscInt :: n, m

      n = tao_data%n ; m = tao_data%m

!  get pointers to vector data

      CALL VecGetArrayReadF90( X, x_array, ierr )
      CALL VecGetArrayF90( R, r_array, ierr )

!  compute r(x)

      CALL CUTEST_cfn_r( ierr, n, m, x_array, f, r_array )

!  restore vectors

      CALL VecRestoreArrayReadF90( X, x_array, ierr )
      CALL VecRestoreArrayF90( R, r_array, ierr )

!     CALL PetscLogFlops( 15.0d0*nn, ierr )

      RETURN
      END SUBROUTINE FormResidual

      SUBROUTINE FormJacobian( tao, X, J, PrecJ, tao_data, ierr )

!  FormHessian - Evaluates Jacobian matrix.
!
!  Input Parameters:
!  tao     - the Tao context
!  X       - input vector
!  tao_data - private data
!
!  Output Parameters:
!  J      - Jacobian matrix
!  PrecJ  - optionally different preconditioning matrix (not used here)
!  flag   - flag indicating matrix structure
!  ierr   - error code
!
!  Note: Providing the Hessian may not be necessary.  Only some solvers
!  require this matrix.

#include "petsc/finclude/petsctao.h"

      USE petsctao
      USE CUTEST_KINDS_precision
      USE CUTEST_TAO
      IMPLICIT NONE

!  dummy arguments

      TYPE ( tTao ) :: tao
      TYPE ( tVec ) :: X
      TYPE ( tMat ) :: J, PrecJ
      PetscErrorCode   ierr
      TYPE ( TAO_data_type ) :: tao_data

!  local variables

      PetscReal, POINTER, DIMENSION( : ) :: x_array
      PetscInt :: i, l, m, n, nnzj

      n = tao_data%n ; m = tao_data%m ; nnzj = tao_data%nnzj

!  get a pointer to vector data

      CALL VecGetArrayReadF90( X, x_array, ierr )

!  compute Jacobian entries at x

      CALL CUTEST_csgr_r( ierr, n, m, x_array, tao_data%Y,                     &
                          .FALSE., nnzj, nnzj,                                 &
                          tao_data%J_val, tao_data%H_row, tao_data%H_col )
      IF ( ierr /= 0 ) RETURN

!  restore vector

      CALL VecRestoreArrayReadF90( X, x_array, ierr )

!  insert the Hessian values into petsc storage

      DO l = 1, nnzj
        i = tao_data%J_row( l ) - 1
        IF ( i >= 0 ) CALL MatSetValue( J, i, tao_data%J_col( l ) - 1,         &
                                        tao_data%J_val( l ), INSERT_VALUES,    &
                                        ierr )
      END DO

!  assemble matrix

      CALL MatAssemblyBegin( J, MAT_FINAL_ASSEMBLY, ierr )
      CALL MatAssemblyEnd( J, MAT_FINAL_ASSEMBLY, ierr )

!     CALL PetscLogFlops(9.0d0*nn,ierr)

      RETURN
      END SUBROUTINE FormJacobian

!
!/*TEST
!
!   build:
!      requires: !complex
!
!   test:
!      args: -tao_smonitor -tao_type ntr -tao_gatol 1.e-5
!      requires: !single
!
!TEST*/

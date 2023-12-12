! THIS VERSION: CUTEST 2.2 - 2023-12-06 AT 14:20 GMT.

c Driver program for solving a .SIF example problem with TAO 
c (after sifdecode has been used on the .SIF file)

#include "cutest_modules.h"
#include "cutest_routines.h"

      program tao_main

! ----------------------------------------------------------------------
!
#include "petsc/finclude/petsctao.h"
      use petsctao
      implicit none

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!                   Variable declarations
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!  See additional variable declarations in the file rosenbrock1f.h

      PetscErrorCode  ierr    ! used to check for functions returning nonzeros
      type(tVec)      x       ! solution vector
      type(tMat)      H       ! hessian matrix
      type(tTao)      tao     ! TAO_SOVER context
      PetscBool       flg
      PetscInt        i2,i1
      PetscMPIInt     size
      PetscReal       zero
      PetscReal       alpha
      PetscInt        n
      common /params/ alpha, n

!  Note: Any user-defined Fortran routines (such as FormGradient)
!  MUST be declared as external.

      external FormFunctionGradient, FormHessian

      zero = 0.0d0
      i2 = 2
      i1 = 1

!  Initialize TAO and PETSc

      CALL PetscInitialize(ierr)
      CALL MPI_Comm_size(PETSC_COMM_WORLD,size,ierr)

!  Check not available in fortran
!     CALL PetscCheck(size .eq. 1,PETSC_COMM_SELF, PETSC_ERR_WRONG_MPI_SIZE, &
!                      'This is a uniprocessor example only')

!  Initialize problem parameters
      n     = 2
      alpha = 99.0d0

! Check for command line arguments to override defaults
      Call PetscOptionsGetInt(PETSC_NULL_OPTIONS, &
                              PETSC_NULL_CHARACTER,'-n',n,flg,ierr)
      Call PetscOptionsGetReal(PETSC_NULL_OPTIONS, &
                               PETSC_NULL_CHARACTER,'-alpha',alpha,flg,ierr)

!  Allocate vectors for the solution and gradient
      Call VecCreateSeq(PETSC_COMM_SELF,n,x,ierr)

!  Allocate storage space for Hessian;
      Call MatCreateSeqBAIJ(PETSC_COMM_SELF,i2,n,n,i1, &
                            PETSC_NULL_INTEGER, H,ierr)

      Call MatSetOption(H,MAT_SYMMETRIC,PETSC_TRUE,ierr)

!  The TAO code begins here

!  Create TAO solver
      Call TaoCreate(PETSC_COMM_SELF,tao,ierr)
      Call TaoSetType(tao,TAOLMVM,ierr)

!  Set routines for function, gradient, and hessian evaluation
      Call TaoSetObjectiveAndGradient(tao,PETSC_NULL_VEC, &
                                      FormFunctionGradient,0,ierr)
      Call TaoSetHessian(tao,H,H,FormHessian,0,ierr)

!  Optional: Set initial guess
      Call VecSet(x, zero, ierr)
      Call TaoSetSolution(tao, x, ierr)

!  Check for TAO command line options
      Call TaoSetFromOptions(tao,ierr)

!  SOLVE THE APPLICATION
      Call TaoSolve(tao,ierr)

!  TaoView() prints ierr about the TAO solver; the option
!      -tao_view
!  can alternatively be used to activate this at runtime.
!      Call TaoView(tao,PETSC_VIEWER_STDOUT_SELF,ierr)

!  Free TAO data structures
      Call TaoDestroy(tao,ierr)

!  Free PETSc data structures
      Call VecDestroy(x,ierr)
      Call MatDestroy(H,ierr)

      Call PetscFinalize(ierr)

      end program tao_main


! --------------------------------------------------------------------
!  FormFunctionGradient - Evaluates the function f(X) and gradient G(X)
!
!  Input Parameters:
!  tao - the Tao context
!  X   - input vector
!  dummy - not used
!
!  Output Parameters:
!  G - vector containing the newly evaluated gradient
!  f - function value

      subroutine FormFunctionGradient(tao, X, f, G, dummy, ierr)
#include "petsc/finclude/petsctao.h"
      use petsctao
      implicit none

      type(tTao)       tao
      type(tVec)       X,G
      PetscReal        f
      PetscErrorCode   ierr
      PetscInt         dummy

      PetscReal        ff,t1,t2
      PetscInt         i,nn
      PetscReal, pointer :: g_v(:),x_v(:)
      PetscReal        alpha
      PetscInt         n
      common /params/ alpha, n

      ierr = 0
      nn = n/2
      ff = 0

!     Get pointers to vector data
      Call VecGetArrayReadF90(X,x_v,ierr)
      Call VecGetArrayF90(G,g_v,ierr)

!     Compute G(X)
      do i=0,nn-1
         t1 = x_v(1+2*i+1) - x_v(1+2*i)*x_v(1+2*i)
         t2 = 1.0 - x_v(1 + 2*i)
         ff = ff + alpha*t1*t1 + t2*t2
         g_v(1 + 2*i) = -4*alpha*t1*x_v(1 + 2*i) - 2.0*t2
         g_v(1 + 2*i + 1) = 2.0*alpha*t1
      enddo

!     Restore vectors
      Call VecRestoreArrayReadF90(X,x_v,ierr)
      Call VecRestoreArrayF90(G,g_v,ierr)

      f = ff
      Call PetscLogFlops(15.0d0*nn,ierr)

      return
      end subroutine FormFunctionGradient

!
! ---------------------------------------------------------------------
!
!  FormHessian - Evaluates Hessian matrix.
!
!  Input Parameters:
!  tao     - the Tao context
!  X       - input vector
!  dummy   - optional user-defined context, as set by SNESSetHessian()
!            (not used here)
!
!  Output Parameters:
!  H      - Hessian matrix
!  PrecH  - optionally different preconditioning matrix (not used here)
!  flag   - flag indicating matrix structure
!  ierr   - error code
!
!  Note: Providing the Hessian may not be necessary.  Only some solvers
!  require this matrix.

      subroutine FormHessian(tao,X,H,PrecH,dummy,ierr)
#include "petsc/finclude/petsctao.h"
      use petsctao
      implicit none

!  Input/output variables:
      type(tTao)       tao
      type(tVec)       X
      type(tMat)       H, PrecH
      PetscErrorCode   ierr
      PetscInt         dummy

      PetscReal        v(0:1,0:1)
      PetscBool        assembled

! PETSc's VecGetArray acts differently in Fortran than it does in C.
! Calling VecGetArray((Vec) X, (PetscReal) x_array(0:1), (PetscOffset) x_index, ierr)
! will return an array of doubles referenced by x_array offset by x_index.
!  i.e.,  to reference the kth element of X, use x_array(k + x_index).
! Notice that by declaring the arrays with range (0:1), we are using the C 0-indexing practice.
      PetscReal, pointer :: x_v(:)
      PetscInt         i,nn,ind(0:1),i2
      PetscReal        alpha
      PetscInt         n
      common /params/ alpha, n

      ierr = 0
      nn= n/2
      i2 = 2

!  Zero existing matrix entries
      Call MatAssembled(H,assembled,ierr)
      if (assembled .eqv. PETSC_TRUE) Call MatZeroEntries(H,ierr)

!  Get a pointer to vector data

      Call VecGetArrayReadF90(X,x_v,ierr)

!  Compute Hessian entries

      do i=0,nn-1
         v(1,1) = 2.0*alpha
         v(0,0) = -4.0*alpha*(x_v(1+2*i+1) - 3*x_v(1+2*i)*x_v(1+2*i))+2
         v(1,0) = -4.0*alpha*x_v(1+2*i)
         v(0,1) = v(1,0)
         ind(0) = 2*i
         ind(1) = 2*i + 1
         Call MatSetValues(H,i2,ind,i2,ind,v,INSERT_VALUES,ierr)
      enddo

!  Restore vector

      Call VecRestoreArrayReadF90(X,x_v,ierr)

!  Assemble matrix

      Call MatAssemblyBegin(H,MAT_FINAL_ASSEMBLY,ierr)
      Call MatAssemblyEnd(H,MAT_FINAL_ASSEMBLY,ierr)

      CALL PetscLogFlops(9.0d0*nn,ierr)

      return
      end subroutine FormHessian

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

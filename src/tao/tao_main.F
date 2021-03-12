c Driver program for solving a .SIF example problem with TAO 
c (after sifdecode has been used on the .SIF file)

      program tao_main
      implicit none

#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
#include "finclude/tao_solver.h"

c *** for statistical information
      integer n,nfev,ngrad,nhes
      double precision f_val, g_val
      common /stats/ n,nfev,ngrad,nhes
      common /fstats/ f_val, g_val
      SAVE / fstats /

      double precision h_array(1800000)
      integer rowindex(1800000), colindex(1800000)
      common /hessian/ h_array, rowindex, colindex
      
      CHARACTER ( LEN = 30 ) :: rfilename = 'TAORES.d'
      INTEGER :: rfiledevice = 47
      INTEGER :: io_buffer = 11

      TAO_SOLVER       tao
      TAO_APPLICATION  myapp
      Vec              x,xl,xu,g
      Mat              H
      integer          infile, outfile
      PetscTruth       flg
      PetscScalar      x_array(1), xl_array(1)
      PetscScalar      xu_array(1),g_array(1)
      PetscFortranAddr x_index, xl_index 
      PetscFortranAddr xu_index, g_index
      PetscLogDouble   t1,tlimit
      PetscReal        maxtime

      integer          nfree, nbind, iter
      double precision f,gnorm,ff,cnorm,xdiff
      real calls(4)
      real time(4)

      integer          i,info,nnzh,reason,na
      integer          its(100000)
      double precision hist(100000)

      double precision gatol,grtol
      double precision fatol,frtol,catol,crtol
      external FormFunctionGradient, FormHessian, getstats
      external FormFunction, FormGradient
      external MyTimeMonitor

      LOGICAL :: filexx, is_specfile
      INTEGER :: iores
      INTEGER :: errout = 6
      CHARACTER ( len = 10 ) :: pname
      CHARACTER ( len = 10 ), ALLOCATABLE, DIMENSION( : ) :: xnames

!  If required, open a file for the results

       INQUIRE( FILE = rfilename, EXIST = filexx )
       IF ( filexx ) THEN
          OPEN( rfiledevice, FILE = rfilename, FORM = 'FORMATTED',             &
     &          STATUS = 'OLD', POSITION = 'APPEND', IOSTAT = iores )
       ELSE
          OPEN( rfiledevice, FILE = rfilename, FORM = 'FORMATTED',             &
     &          STATUS = 'NEW', IOSTAT = iores )
       END IF
       IF ( iores /= 0 ) THEN 
         write( errout, "( ' result file opening error. stopping ' )" )
         STOP
       END IF

c *** absolute and relative gradient tolerances

      call PetscInitialize(PETSC_NULL_CHARACTER,info)
      call TaoInitialize(PETSC_NULL_CHARACTER,info)

      f_val = HUGE( 1.0d+0 )
      g_val = HUGE( 1.0d+0 )
      gatol = 1.0d-3
      grtol = 0
      infile = 1
      outfile = 6 
c *** outfile = 6  -> standard output      
      open (infile,file='OUTSDIF.d',status='old')


c *** Get the dimension size 
      call CUTEST_udimen(info,infile,n)
      IF ( info /= 0 ) GO TO 910
      allocate( xnames( n ), STAT =iores )
      
      IF ( iores /= 0 ) THEN 
         write( errout, "( ' allocation error. stopping ' )" )
         STOP
       END IF

c *** create the vectors
      call VecCreateSeq(PETSC_COMM_SELF,n,x,info)
      call VecDuplicate(x,xl,info)
      call VecDuplicate(x,xu,info)
      call VecDuplicate(x,g,info)

c *** Generate the initial point and project into [xl,xu]

c *** When getting a petsc vector array using fortran, the VecGetArray
c     function returns a double precision address x_array and an offset 
c     where the data begins (x_index).  So to get the nth data element 
c     of x, use x_array(x_index+n) (using 1-based arrays)

      call VecGetArray(x,x_array,x_index,info)
      call VecGetArray(xl,xl_array,xl_index,info)
      call VecGetArray(xu,xu_array,xu_index,info)
      call VecGetArray(g,g_array,g_index,info)

      call CUTEST_usetup(info,infile, outfile, io_buffern,                   &
     &                   x_array(x_index+1), xl_array(xl_index+1),             &
     &                   xu_array(xu_index+1) )
      IF ( info /= 0 ) GO TO 910
      
      do i=1,n
         if (xl_array(xl_index+i) .gt. xu_array(xu_index+i)) then
            print *,'Bad vector bounds'
            stop
         elseif (x_array(x_index+i) .lt. xl_array(xl_index+i))  then
            x_array(x_index+i) = xl_array(xl_index+i)
         elseif (x_array(x_index+i) .gt. xu_array(xu_index+i)) then
            x_array(x_index+i) = xu_array(x_index+i)
         endif
      enddo

      CALL CUTEST_unames( info, n, pname, xnames )
      IF ( info /= 0 ) GO TO 910
      WRITE( rfiledevice, "( A10, I6 )", advance = 'no' ) pname, n

c *** Count the free and binding variables, gradient projection norm
      call uofg(n,x_array(x_index+1), f, g_array(g_index+1),.true.)

      call getstats(n,x_array(x_index+1),xl_array(xl_index+1),              &
     &             xu_array(xu_index+1),g_array(g_index+1),nfree,nbind,     &
     &             gnorm)


      call VecRestoreArray(x,x_array,x_index,info)
      call VecRestoreArray(xl,xl_array,xl_index,info)
      call VecRestoreArray(xu,xu_array,xu_index,info)
      call VecRestoreArray(g,g_array,g_index,info)

c *** Get the number of nonzeros and create the hessian
      call CUTEST_udimsh(info, nnzh)
      IF ( info /= 0 ) GO TO 910
      call MatCreateSeqAIJ(PETSC_COMM_SELF,n,n,2*nnzh/n,                &
     &                     PETSC_NULL_INTEGER,H,info)
      call MatSetOption(H,MAT_SYMMETRIC,.true.,info)

!      na = 100000
!      call TaoSetConvergenceHistory(tao,hist,its,na,.FALSE.,info)

      write (outfile,1000) n, nfree, n-nfree, nbind


      call TaoCreate(PETSC_COMM_SELF,'tao_lmvm',tao,info)
      call TaoApplicationCreate(PETSC_COMM_SELF,myapp,info)

      call TaoSetTolerances(tao,0.0d0, 0.0d0, 0.0d0, 0.0d0, info)
      call TaoSetGradientTolerances(tao,gatol, grtol, 0.0d0, info)
      call TaoSetMaximumFunctionEvaluations(tao,200000,info)
      call TaoSetMaximumIterates(tao,100000,info)

      call TaoAppSetObjectiveRoutine(myapp,                             &
     &      FormFunction,PETSC_NULL_OBJECT,info) 
      call TaoAppSetGradientRoutine(myapp,                              &
     &      FormGradient,PETSC_NULL_OBJECT,info) 
      call TaoAppSetObjectiveAndGradientRo(myapp,                       &
     &      FormFunctionGradient,PETSC_NULL_OBJECT,info) 

*     Check for time limit
      call PetscOptionsGetReal(PETSC_NULL_CHARACTER,"-time_limit",      &
     &     maxtime, flg, info)
      if (flg) then
         call PetscGetTime(t1,info)
         tlimit = t1 + maxtime
         call TaoAppSetMonitor(myapp,MyTimeMonitor,tlimit,info)
      endif

      call TaoAppSetHessianMat(myapp,H,H,info)
      call TaoAppSetHessianRoutine(myapp,FormHessian,nnzh,              &
     &     info)


      call TaoAppSetVariableBounds(myapp,xl,xu,info)
      call TaoAppSetInitialSolutionVec(myapp,x,info)


      nfev = 0
      ngrad = 0
      nhes = 0
*     call TaoSetApplication(tao,myapp,info)
      call TaoSetOptions(myapp,tao,info)
      call TaoSolveApplication(myapp,tao,info)

c *** Count the free and binding variables again

c *** When getting a petsc vector array using fortran, the VecGetArray
c     function returns a double precision address x_array and an offset 
c     where the data begins (x_index).  So to get the nth data element 
c     of x, use x_array(x_index+n) (using 1-based arrays)

      call VecGetArray(x,x_array,x_index,info)
      call VecGetArray(xl,xl_array,xl_index,info)
      call VecGetArray(xu,xu_array,xu_index,info)
      call VecGetArray(g,g_array,g_index,info)

      call CUTEST_uofg(info,n,x_array(x_index+1), f,                         &
     &                 g_array(g_index+1), .true.)
      IF ( info /= 0 ) GO TO 910
      call getstats(n,x_array(x_index+1),xl_array(xl_index+1),                 &
     &             xu_array(xu_index+1),g_array(g_index+1),nfree,nbind,        &
     &             gnorm)

      call VecRestoreArray(x,x_array,x_index,info)
      call VecRestoreArray(xl,xl_array,xl_index,info)
      call VecRestoreArray(xu,xu_array,xu_index,info)
      call VecRestoreArray(g,g_array,g_index,info)


      write (outfile,2000) nfree, n-nfree, nbind, nfev, f, gnorm

!     write(6,*) nfev,ngrad,nhes

      call CUTEST_ureport(info,calls,time)
      IF ( info /= 0 ) GO TO 910
      write (outfile,3000) time(1), time(2)
      call TaoView(tao,info)


      if (gnorm .le. gatol .or. gnorm .le. grtol*abs(f)) then
         write (outfile,4000)
      else
         write (outfile,5000) gnorm, gatol
      endif
         
!   Get information on termination
       call TaoGetSolutionStatus(tao,iter,ff,gnorm,cnorm,xdiff,            &
     &                           reason,info)
       if (reason .lt. 0) then
          print *,'TAO did not terminate successfully'
       endif

!        call TaoGetConvergenceHistory(tao, hist, its, na, info)
!        write(6,*) 'na',na
!        write(6,*) hist(1:na)
!        write(6,*) its(1:na)
!       call TaoGetTolerances(tao,fatol,frtol,catol,crtol,info)
!       write(6,*) fatol,frtol,catol,crtol

!     write(6,*) calls(1), calls(2), calls(3), calls(4)

!     BACKSPACE( rfiledevice )
      IF ( reason >= 0 ) THEN
        WRITE( rfiledevice, 2050 ) ff, gnorm,                               &
     &    INT( calls(2) ),                                                  & 
     &    INT( calls(3) ), INT( calls(4) ), time(2), reason
      ELSE
        WRITE( rfiledevice, 2050 ) ff, gnorm,                               &
     &    - INT( calls(2) ),                                                & 
     &    - INT( calls(3) ), - INT( calls(4) ), - time(2), reason
      END IF
      CLOSE( rfiledevice ) 

      call VecDestroy(x,info)
      call VecDestroy(g,info)
      call VecDestroy(xl,info)
      call VecDestroy(xu,info)
      call MatDestroy(H,info)

      call TaoDestroy(tao,info)
      call TaoAppDestroy(myapp,info)

      close(outfile)
      close(infile)

      call TaoFinalize(info)
      call PetscFinalize(info)

      call CUTEST_uterminate( info )
      return

  910 continue
      write( errout, "( ' cutest error, status = ', i0, ', stopping' )") 
     +   info
      return
      

 1000 format (
     +        ' Number of variables                         ',i12,/,
     +        ' Number of free variables at x(start)        ',i12,/,
     +        ' Number of active variables at x(start)      ',i12,/,
     +        ' Number of binding variables at x(start)     ',i12,/)
 2000 format (
     +        ' Number of free variables at x(final)        ',i12,/,
     +        ' Number of active variables at x(final)      ',i12,/,
     +        ' Number of binding variables at x(final)     ',i12,/,
     +        ' Number of function evaluations              ',i12,/,
     +        ' Function value at final iterate          '   ,d20.6,/,
     +        ' Projected gradient at final iterate      '   ,d20.6,/)

 2050 FORMAT( ES16.8, ES9.1, bn, 2I7, I9, ' :', F9.2, I5 )

 3000 format ('CPU time (s) for USETUP  ',f10.3,/,
     +        'CPU time (s) since USETUP',f10.3,/)

 4000 format (' Exit message     CONVERGENCE: GTOL TEST SATISFIED',//)
 5000 format (' Exit message     GRADIENT NORM ',f9.6,' > GTOL ',f9.6//)


      end


c ***************************************************** 
c ***  FormHessian
c ***       Calculates the hessian matrix at the
c ***       given vector.
c ***  
c ***       This function is called from withing the tao solver
c *** 
c *** IN:  tao - pointer to tao solver structure (used in C)
c ***      X   - the pointer to the PETSc vector 
c *** 
c *** OUT: H - PETSc matrix holding hessian matrix
c ***      Hpre - not used (for preconditioning)
c ***      flag - not used
c ***      nnzh - max number of nonzeros in the hessian
c ***      info - 0 if everything is ok
c *****************************************************

      subroutine FormHessian(taoapp, X, H, Hpre, flag, nnzh, info)
      implicit none

#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
#include "finclude/tao_solver.h"


      TAO_APPLICATION taoapp
      Vec        X
      Mat        H,Hpre
      MatStructure flag
      integer    nnzh, info
      

      double precision h_array(1800000)
      integer colindex(1800000),rowindex(1800000)
      common /hessian/ h_array, colindex, rowindex

      integer n,nfev,ngrad,nhes
      common /stats/ n,nfev,ngrad,nhes

      external ush

      PetscScalar x_array(1)
      PetscScalar z
      PetscFortranAddr x_index
      integer i

      
      info = 0
      z = 0.0

      nhes = nhes + 1

c *** When getting a petsc vector array using fortran, the VecGetArray
c     function returns a double precision address x_array and an offset 
c     where the data begins (x_index).  So to get the nth data element 
c     of x, use x_array(x_index+n) (using 1-based arrays)

      call VecGetArray(X,x_array,x_index,info)

c *** Get sparse hessian at x
      call CUTEST_ush(info,n,x_array(x_index+1),nnzh,nnzh,h_array,             &
     &                rowindex,colindex)
      IF ( info /= 0 ) RETURN
      call VecRestoreArray(X,x_array,x_index,info)

c *** Convert the sparse hessian to the petsc matrix
      call MatZeroEntries(H,info)

C *** Insert diagonal values into the sparse Hessian matrix

      do i=1,n
         call MatSetValue(H,i-1,i-1,z,INSERT_VALUES, info)
      enddo

      do i=1,nnzh
         call MatSetValue(H,rowindex(i)-1,colindex(i)-1,h_array(i),            &
     &                    INSERT_VALUES, info)

         if (rowindex(i) .ne. colindex(i)) then
            call MatSetValue(H,colindex(i)-1,rowindex(i)-1,h_array(i),         &
     &                    INSERT_VALUES, info)
         endif
      enddo

      call MatAssemblyBegin(H,MAT_FINAL_ASSEMBLY,info)
      call MatAssemblyEnd(H,MAT_FINAL_ASSEMBLY,info)

      return
      end

c ***************************************************** 
c ***  FormFunction
c ***       Calculates the function at the
c ***       given vector.
c ***  
c ***       This function is called from withing the tao solver
c *** 
c *** IN:  tao - pointer to tao solver structure (used in C)
c ***      X   - the pointer to the PETSc vector 
c ***      dummy - not used
c *** 
c *** OUT: f - value of the function at X
c ***      info - 0 if everything is ok
c *****************************************************
      
      subroutine FormFunction(taoapp,X,f,dummy,info)
      implicit none

#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
#include "finclude/tao_solver.h"


      TAO_APPLICATION taoapp
      Vec X
      double precision f
      integer dummy, info
      

      integer n,nfev,ngrad,nhes
      double precision f_val, g_val
      common /stats/ n,nfev,ngrad,nhes
      common /fstats/ f_val, g_val

      external ufn

      PetscScalar x_array(1)
      PetscFortranAddr x_index

      info = 0
      nfev = nfev + 1

c *** When getting a petsc vector array using fortran, the VecGetArray
c     function returns a double precision address x_array and an offset 
c     where the data begins (x_index).  So to get the nth data element 
c     of x, use x_array(x_index+n) (using 1-based arrays)

      call VecGetArray(X,x_array,x_index,info)

      call CUTEST_ufn(info,n,x_array(x_index+1),f)
      IF ( info /= 0 ) RETURN
      f_val = f
      call VecRestoreArray(X,x_array,x_index,info)

      return
      end
      
      
c ***************************************************** 
c ***  FormGradient
c ***       Calculates the gradient at the
c ***       given vector.
c ***  
c ***       This function is called from withing the tao solver
c *** 
c *** IN:  tao - pointer to tao solver structure (used in C)
c ***      X   - the pointer to the PETSc vector 
c ***      dummy - not used
c *** 
c *** OUT: G - PETSc vector holding value of gradient at X
c ***      info - 0 if everything is ok
c *****************************************************
      
      subroutine FormGradient(taoapp,X,G,dummy,info)
      implicit none

#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
#include "finclude/tao_solver.h"


      TAO_APPLICATION taoapp
      Vec X,G
      integer dummy, info
      integer i

      integer n,nfev,ngrad,nhes
      double precision f_val, g_val
      common /stats/ n,nfev,ngrad,nhes
      common /fstats/ f_val, g_val


      external ugr

      PetscScalar x_array(1),g_array(1)
      PetscFortranAddr x_index,g_index


      info = 0
      ngrad = ngrad + 1

c *** When getting a petsc vector array using fortran, the VecGetArray
c     function returns a double precision address x_array and an offset 
c     where the data begins (x_index).  So to get the nth data element 
c     of x, use x_array(x_index+n) (using 1-based arrays)

      call VecGetArray(X,x_array,x_index,info)
      call VecGetArray(G,g_array,g_index,info)

      call ugr(info,n,x_array(x_index+1),g_array(g_index+1))


      g_val = 0.0d0
      do i = 1, n
        g_val = g_val + g_array(g_index+i)**2
      end do
      g_val = sqrt(g_val)


      call VecRestoreArray(X,x_array,x_index,info)
      call VecRestoreArray(G,g_array,g_index,info)

      return
      end
      
      
c ***************************************************** 
c ***  FormFunctionGradient
c ***       Calculates the function and gradient at the
c ***       given vector.
c ***  
c ***       This function is called from withing the tao solver
c *** 
c *** IN:  tao - pointer to tao solver structure (used in C)
c ***      X   - the pointer to the PETSc vector 
c ***      dummy - not used
c *** 
c *** OUT: f - value of the function at X
c ***      G - PETSc vector holding value of gradient at X
c ***      info - 0 if everything is ok
c *****************************************************
      
      subroutine FormFunctionGradient(taoapp,X,f,G,dummy,info)
      implicit none

#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
#include "finclude/tao_solver.h"


      TAO_APPLICATION taoapp
      Vec X,G
      double precision f
      integer dummy, info
      integer i
      

      integer n,nfev,ngrad,nhes
      double precision f_val, g_val
      common /stats/ n,nfev,ngrad,nhes
      common /fstats/ f_val, g_val


      external uofg

      PetscScalar x_array(1),g_array(1)
      PetscFortranAddr x_index,g_index


      info = 0
      nfev = nfev + 1
      ngrad = ngrad + 1

c *** When getting a petsc vector array using fortran, the VecGetArray
c     function returns a double precision address x_array and an offset 
c     where the data begins (x_index).  So to get the nth data element 
c     of x, use x_array(x_index+n) (using 1-based arrays)

      call VecGetArray(X,x_array,x_index,info)
      call VecGetArray(G,g_array,g_index,info)

      call CUTEST_uofg(info,n,x_array(x_index+1), f,                           &
     &                 g_array(g_index+1),.true.)
      IF ( info /= 0 ) RETURN
      f_val = f
      g_val = 0.0d0
      do i = 1, n
        g_val = g_val + g_array(g_index+i)**2
      end do
      g_val = sqrt(g_val)

      call VecRestoreArray(X,x_array,x_index,info)
      call VecRestoreArray(G,g_array,g_index,info)

      return
      end
      
      
c ***************************************************** 
c ***  getstats :
c ***       calculates the number of free and binding 
c ***       variables at the given vector
c *** IN:  n - number of elements in vector
c ***      x - array of vector elements
c ***      xl - array of lower bounds
c ***      xu - array of upper bounds
c ***      g - gradient array
c *** OUT: nfree - number of free variables at x
c ***      nbind - number of binding variables at x
c ***      gnorm - norm of the gradient vector at x
c *****************************************************
      subroutine getstats(n,x,xl,xu,g,nfree,nbind,gnorm)
      implicit none

      integer          n,nfree,nbind
      double precision x(n), xl(n), xu(n), g(n)
      double precision gnorm

      integer i

      nfree=0
      nbind =0

c *** count the free and binding variables      
      do i=1,n
         if (xl(i) .lt. x(i)  .and.  x(i) .lt. xu(i)) then
            nfree = nfree + 1
         elseif ((x(i).eq.xl(i) .and. g(i) .ge. 0)        .or.              &
     &           (x(i) .eq. xu(i) .and.   g(i) .le. 0)     .or.             &
     &           xl(i) .eq. xu(i)) then
            nbind = nbind + 1
         endif
      enddo

            
c *** calculate the gradient projection norm      
      gnorm = 0.0d0
      do i = 1, n
         if (xl(i) .ne. xu(i)) then
            if (x(i) .eq. xl(i)) then
               gnorm = gnorm + min(g(i),0.0d0)**2
            else if (x(i) .eq. xu(i)) then
               gnorm = gnorm + max(g(i),0.0d0)**2
            else
               gnorm = gnorm + g(i)**2
            end if
         end if
      end do
      gnorm = sqrt(gnorm)
 
      return 
      end

      subroutine MyTimeMonitor(tao, tlimit)
#include "finclude/petscsys.h"
#include "finclude/tao_solver.h"
C#include "finclude/petsctime.h"      
      TAO_SOLVER tao
      PetscLogDouble tlimit, now
      integer info
      INTEGER :: rfiledevice = 47

      integer          iter, reason
      double precision f,gnorm,ff,cnorm,xdiff
      real calls(4)
      real time(2)

      double precision f_val, g_val
      common /fstats/ f_val, g_val

      call PetscGetTime(now,info)
      if (now .gt. tlimit) then
         call CUTEST_ureport(info,calls,time)
         WRITE( rfiledevice, 
     &    "( ES16.8, ES9.1, bn, 2I7, I9, ' :', F9.2, I5 )" )               &
     &    f_val, g_val, - INT( calls(2) ),                                 & 
     &    - INT( calls(3) ), - INT( calls(4) ), - time(2), -8

         CLOSE( rfiledevice ) 
         SETERRQ(1,"Time limit reached",info)
!         call TaoSetTerminationReason(tao,-8,info)
      endif
      return

      end subroutine MyTimeMonitor
         

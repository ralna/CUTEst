! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      PROGRAM PENNLP_main

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     Driver for running PENNLP on CUTEst problems.
C
C     Nick Gould, February 2013
C     
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      USE CUTEST_KINDS_precision
      IMPLICIT none

C  Set up parameters, variables and arrays required by constrained tools

      INTEGER ( KIND = ip_ ), PARAMETER :: input = 55, indr = 46
      INTEGER ( KIND = ip_ ), PARAMETER :: io_buffer = 11, out = 6
      INTEGER ( KIND = ip_ ) :: alloc_stat, status, i, m, m_lin
      INTEGER ( KIND = ip_ ) :: pennlp_status
      INTEGER ( KIND = ip_ ) :: IOPTIONS( 17 ), IRESULTS( 4 )
      REAL ( KIND = rp_ ), PARAMETER :: cutest_inf = 1.0E+19_rp_
      REAL ( KIND = rp_ ), PARAMETER :: pennlp_inf = 2.0E+38_rp_
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 7 )
      REAL ( KIND = rp_ ) :: DOPTIONS( 13 ), DRESULTS( 5 )
      CHARACTER * 10 :: pname
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X_l, X_u, X
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: C_l, C_u, Y
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATN, LINEAR
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: X_names
      EXTERNAL :: PENNLP_evalof, PENNLP_evalog, PENNLP_evaloh
      EXTERNAL :: PENNLP_evalcf, PENNLP_evalcg, PENNLP_evalch

C  common needed to pass assumed size array dimensions

      INTEGER ( KIND = ip_ ) :: n, max_nnzg, max_nnzh
      COMMON / PENNLP_common / n, max_nnzg, max_nnzh
      SAVE / PENNLP_common /

C  open the Spec file for the package

      OPEN( indr, FILE = 'PENNLP.SPC', FORM = 'FORMATTED', 
     &      STATUS = 'OLD')
      REWIND( indr )

C  set up algorithmic input data in IOPTIONS( 1 : 17 ), DOPTIONS( 1 : 13 )

C  maxit        maximum numbers of outer iterations
C  nwtiters     maximum number of iterations in inner loop
C  outlev       output level (0=none,1=options,2=brief,3=full)
C  hessianmode  check density of hessian (0=auto,1=dense)
C  autoscale    automatic scaling? (0=no,1=yes)
C  convex       convex problem? (0=no,1=yes)
C  eqltymode    treat equality constraints (0,1=spilit,2=aug,3=direct)
C  ignoreinit   ignore initial solutions? (0=no,1=yes)
C  ncmode       nonconvex mode (0=modified Newton,1=trust region)
C  nwtstopcrit  stopping criterion (0=abs,1=rel,3=scaled rel)
C  penalty      penalty function (0=log barrier,1=reciprocal barrier)
C  nwtmode      Newton system mode (0=Chol,1=cg,2=app Hess cg,3=dual)
C  prec         cg preconditioner (0=none,1=diag,2=bfgs,3=ainv,4=sgs)
C  cmaxnzs      Hessian tuning in nwtmode 1-3 (-1=off,other=on)
C  autoini      initialization of multipliers (0=off,1=nlp,2=lp mode)
C  penup        penalty parameter update (0=adaptive,1=every iter)
C  usebarrier   box constraint mode (0=none,1=barrier,2=modified bar)
C  precision    required final precision
C  uinit        initial multilplier scaling factor               
C  pinit        initial penalty
C  alpha        stopping parameter for inner Newton/Trust region method
C  mu           restriction factor of multiplier update
C  penup        penalty update
C  peps         minimal penalty
C  umin         minimal multiplier
C  preckkt      precision of the KKT conditions
C  cgtolmin     minimum tolerance of the conjugate gradient algorithm
C  cgtolup      update of tolerance of the conjugate gradient algorithm
C  uinitbox     initial multiplier box constraints
C  uinitnc      initial multiplier nonlinear constraints

      READ( indr, "( 29( G10.8, / ), G10.8 )" ) 
     &  IOPTIONS( 1 : 17 ), DOPTIONS( 1 : 13 )
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

C  Allocate suitable arrays

      ALLOCATE( X( n ), X_l( n ), X_u( n ), Y( m ), C_l( m ), 
     &          C_u( m ), EQUATN( m ), LINEAR( m ), X_names( n ), 
     &          STAT = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

C  Set up the data structures necessary to hold the group partially
C  separable function.

      CALL CUTEST_csetup_r( status, input, out, io_buffer, 
     &                    n, m, X, X_l, X_u,                    
     &                    Y, C_l, C_u, EQUATN, LINEAR, 0, 2, 0 )
      IF ( status /= 0 ) GO TO 910

C  count the number of linear constraints

      m_lin = 0
      DO i = 1, m
        IF ( LINEAR( i ) ) m_lin = m_lin + 1
      END DO 
      DEALLOCATE( EQUATN, LINEAR )
       
C  match PENNLP's infinite bound value

      DO i = 1, n
        IF ( X_l( i ) < - cutest_inf ) X_l( i ) = - pennlp_inf
        IF ( X_u( i ) > cutest_inf ) X_u( i ) = pennlp_inf
      END DO

      DO i = 1, m
        IF ( C_l( i ) < - cutest_inf ) C_l( i ) = - pennlp_inf
        IF ( C_u( i ) > cutest_inf ) C_u( i ) = pennlp_inf
      END DO

C  how many nonzeros are there in the Hessian of the Lagrangian

      max_nnzg = n
      CALL CUTEST_cdimsh_r( status, max_nnzh )
      IF ( status /= 0 ) GO TO 910

C  Determine the name of the problem

      CALL CUTEST_probname_r( status, pname )
      IF ( status /= 0 ) GO TO 910
C     WRITE( out, "( /, ' Problem: ', A10 )" ) pname 

C  call solver

      CALL PENNLPF( n, m_lin, m, max_nnzg, max_nnzh,
     &              X_l, X_u, C_l, C_u, X, Y, 
     &              PENNLP_evalof, PENNLP_evalog, PENNLP_evaloh,
     &              PENNLP_evalcf, PENNLP_evalcg, PENNLP_evalch,
     &              IOPTIONS, DOPTIONS, IRESULTS, DRESULTS, 
     &              pennlp_status )

C  Output final objective function value and timing information

      IF ( out .GT. 0 ) THEN
        CALL CUTEST_creport_r( status, CALLS, CPU )
        IF ( pennlp_status >= 0 .AND. pennlp_status <= 2 ) THEN
          CALL CUTEST_varnames_r( status, n, X_names )
          IF ( status /= 0 ) GO TO 910
          WRITE( out,"(' Objective function value:', ES12.4 )" ) 
     &      DRESULTS( 1 )
          WRITE ( out, "( /, ' Solution:',
     &       /, ' name            X          X_l         X_u ',
     &       /, ( 1X, A10, 1P, 3D12.4 ) )" ) 
     &       ( X_names( i ), X( i ), X_l( i ), X_u( i ), i = 1, n )
        ENDIF
        WRITE ( out, 2000 ) pname, n, m, CALLS( 1 ), CALLS( 2 ), 
     &    CALLS( 5 ), CALLS( 6 ), pennlp_status, DRESULTS( 1 ), 
     &    CPU( 1 ), CPU( 2 )
      END IF
      DEALLOCATE( X, X_l, X_u, C_l, C_u, Y, x_names, STAT = status )
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
     &    ,' Package used            :  PENNLP',    /
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

C  ===========================================================================

      SUBROUTINE PENNLP_evalof( X, f )

C  compute f(x)

      USE CUTEST_KINDS_precision
      REAL ( KIND = rp_ ) :: f
      REAL ( KIND = rp_ ) :: X( * )

C  local variables

      INTEGER ( KIND = ip_ ) :: nnzg, status
      INTEGER ( KIND = ip_ ) :: G_var( 0 )
      REAL ( KIND = rp_ ) :: G_val( 0 )

C  common needed to pass assumed size array dimensions

      INTEGER ( KIND = ip_ ) :: n, max_nnzg, max_nnzh
      COMMON / PENNLP_common / n, max_nnzg, max_nnzh

C  evaluate f

      CALL CUTEST_cofsg_r( status, n, X, f, nnzg, 0, G_val, G_var, 
     &                   .FALSE. )

C  check for errors

      IF ( status /= 0 ) THEN
        WRITE( 6, "( ' CUTEst error, status = ', I0, ', stopping' )" )
     &   status
        STOP
      END IF
      RETURN
      END

C  ===========================================================================

      SUBROUTINE PENNLP_evalog( X, nnzg, G_var, G_val )

C  compute nabla_x f(x) in sparse format

      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: nnzg
      INTEGER ( KIND = ip_ ) :: G_var( * )
      REAL ( KIND = rp_ ) :: X( * ), G_val( * )

C  local variables

      INTEGER ( KIND = ip_ ) :: status
      REAL ( KIND = rp_ ) :: f

C  common needed to pass assumed size array dimensions

      INTEGER ( KIND = ip_ ) :: n, lg, lh
      COMMON / PENNLP_common / n, lg, lh

C  evaluate the gradient

      CALL CUTEST_cofsg_r( status, n, X, f, nnzg, lg, G_val, G_var, 
     &                     .TRUE. )

C  check for errors

      IF ( status /= 0 ) THEN
        WRITE( 6, "( ' CUTEst error, status = ', I0, ', stopping' )" )
     &   status
        STOP
      END IF
      RETURN
      END

C  ===========================================================================

      SUBROUTINE PENNLP_evaloh( X, nnzh, H_row, H_col, H_val )

C  compute nabla_xx f(x) in sparse format

      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: nnzh
      INTEGER ( KIND = ip_ ) :: H_row( * ), H_col( * )
      REAL ( KIND = rp_ ) :: X( * ), H_val( * )

C  local variables

      INTEGER ( KIND = ip_ ) :: status

C  common needed to pass assumed size array dimensions

      INTEGER ( KIND = ip_ ) :: n, lg, lh
      COMMON / PENNLP_common / n, lg, lh

C  evaluate the Hessian

      CALL CUTEST_cish_r( status, n, X, 0, 
     &                    nnzh, lh, H_val, H_col, H_row )

C  check for errors

      IF ( status /= 0 ) THEN
        WRITE( 6, "( ' CUTEst error, status = ', I0, ', stopping' )" )
     &   status
        STOP
      END IF
      RETURN
      END

C  ===========================================================================

      SUBROUTINE PENNLP_evalcf( i, X, ci )

C  compute c_i(x)

      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: i
      REAL ( KIND = rp_ ) :: ci
      REAL ( KIND = rp_ ) :: X( * )

C  local variables

      INTEGER ( KIND = ip_ ) :: nnzgci, status
      INTEGER ( KIND = ip_ ) :: GCI_var( 0 )
      REAL ( KIND = rp_ ) :: GCI_val( 0 )

C  common needed to pass assumed size array dimensions

      INTEGER ( KIND = ip_ ) :: n, max_nnzg, max_nnzh
      COMMON / PENNLP_common / n, max_nnzg, max_nnzh

C  evaluate the constraint function

      CALL CUTEST_ccifsg_r( status, n, i + 1, X, ci,  
     &                      nnzgci, 0, GCI_val, GCI_var, .FALSE. )

C  check for errors

      IF ( status /= 0 ) THEN
        WRITE( 6, "( ' CUTEst error, status = ', I0, ', stopping' )" )
     &   status
        STOP
      END IF
      RETURN
      END

C  ===========================================================================

      SUBROUTINE PENNLP_evalcg( i, X, nnzgci, GCI_var, GCI_val )

C  compute nabla_x c_i(x) in sparse format

      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: i, nnzgci
      INTEGER ( KIND = ip_ ) :: GCI_var( * )
      REAL ( KIND = rp_ ) :: X( * ), GCI_val( * )

C  local variables

      INTEGER ( KIND = ip_ ) :: status
      REAL ( KIND = rp_ ) :: ci

C  common needed to pass assumed size array dimensions

      INTEGER ( KIND = ip_ ) :: n, lg, lh
      COMMON / PENNLP_common / n, lg, lh

C  evaluate the gradient

      CALL CUTEST_ccifsg_r( status, n, i + 1, X, ci,  
     &                      nnzgci, lg, GCI_val, GCI_var, .TRUE. )

C  check for errors

      IF ( status /= 0 ) THEN
        WRITE( 6, "( ' CUTEst error, status = ', I0, ', stopping' )" )
     &   status
        STOP
      END IF
      RETURN
      END

C  ===========================================================================

      SUBROUTINE PENNLP_evalch( i, X, nnzh, H_row, H_col, H_val )

C  compute nabla_xx c_i(x) in sparse format

      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: i, nnzh
      INTEGER ( KIND = ip_ ) :: H_row( * ), H_col( * )
      REAL ( KIND = rp_ ) :: X( * ), H_val( * )

C  local variables

      INTEGER ( KIND = ip_ ) :: status

C  common needed to pass assumed size array dimensions

      INTEGER ( KIND = ip_ ) :: n, lg, lh
      COMMON / PENNLP_common / n, lg, lh

C  evaluate the Hessian

      CALL CUTEST_cish_r( status, n, X, i + 1, 
     &                    nnzh, lh, H_val, H_col, H_row )

C  check for errors

      IF ( status /= 0 ) THEN
        WRITE( 6, "( ' CUTEst error, status = ', I0, ', stopping' )" )
     &   status
        STOP
      END IF
      RETURN
      END    



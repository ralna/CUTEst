! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

C Copyright (C) 2002, 2004, 2005 Carnegie Mellon University,
C                                Dominique Orban and others.
C
C All Rights Reserved.
C This code is published under the Eclipse Public License.
C*******************************************************************************
      PROGRAM IPOPT_main
C
C     IPOPT CUTEst driver.
C     D. Orban,  adapted from Andreas Waechter's CUTE driver.
C     Adapted for C++ version by Andreas Waechter, Oct 2004
C     CUTEst evolution, Nick Gould, January 2013

      USE CUTEST_KINDS_precision
      IMPLICIT NONE
      INTEGER ( KIND = ip_ ), PARAMETER :: cnr_input = 60
      INTEGER ( KIND = ip_ ), PARAMETER :: inp_input = 70, out = 6
      INTEGER ( KIND = ip_ ), PARAMETER :: io_buffer = 11
      INTEGER ( KIND = ip_ ) :: n, m, nz, ierr, status
      INTEGER ( KIND = ip_ ) :: idx_style, nele_jac, nele_hess
      REAL ( KIND = rp_ ) :: f
      CHARACTER ( LEN = 10 ) :: pname
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IDAT
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: DAT
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 7 )
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, X_l, X_u
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: Z_l, Z_u, LAM
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: G, G_l, G_u
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATN, LINEAR
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: VNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: GNAMES
      INTEGER ( KIND = ip_ ) :: IPSOLVE
      INTEGER ( KIND = ip_ ) :: iproblem, IPCREATE
      EXTERNAL :: EV_F, EV_G, EV_GRAD_F, EV_JAC_G, EV_HESS

C     The following arrays are work space for the evaluation subroutines

      integer ( kind = ip_ ) :: i
      logical :: ex
      real ( kind = rp_ ) :: init_val

C  Open the problem data file.

      OPEN( cnr_input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *      STATUS = 'OLD' )
      REWIND( cnr_input )

C  compute problem dimensions

      CALL CUTEST_cdimen_r( status, cnr_input, n, m )
      IF ( status /= 0 ) GO TO 910

C  allocate space

      ALLOCATE( X( n ), X_l( n ), X_u( n ), Z_l( n ), Z_u( n ),
     *          G( m ), G_l( m ), G_u( m ), LAM( m ),
     *          EQUATN( m ), LINEAR( m ), VNAMES( n ), GNAMES( m ),
     *          STAT = status )
      IF ( status /= 0 ) GO TO 990

C  set up the data structures necessary to hold the problem functions

      CALL CUTEST_csetup_r( status,cnr_input, out, io_buffer,
     1                    n, m, X, X_l, X_u, LAM, G_l, G_u,
     2                    equatn, linear, 0, 0, 0 )
      CLOSE( cnr_input )

C  see if we want to set a different initial point

      INQUIRE( file = 'INITPOINT.VAL', exist = ex )
      IF ( ex ) THEN
         OPEN( inp_input, FILE = 'INITPOINT.VAL', STATUS = 'old' )
         READ( inp_input, '(D25.16)' ) init_val
         DO i = 1, n
           X( i ) = init_val
         END DO
         CLOSE( inp_input )
      endif

C  obtain the number of nonzeros in Jacobian and Hessian

      CALL CUTEST_cdimsj_r( status, nele_jac )
      nele_jac = nele_jac - n
      CALL CUTEST_cdimsh_r( status, nele_hess )

C  allocate furter space

      nz = MAX( nele_jac, nele_hess, 2 * n )
C correction by Elizabeth Wong: 12/3/14
      ALLOCATE( DAT( 2 * n + nz ), IDAT( 2 * nz ), STAT = status )
      IF ( status /= 0 ) GO TO 990

C  get problem name

      CALL CUTEST_cnames_r( status, n, m, pname, VNAMES, GNAMES )

C  call IPOPT

      idx_STYLE = 1
      iproblem = IPCREATE( n, X_L, X_U, m, G_L, G_U,
     *                     nele_jac, nele_hess, idx_style,
     *                     EV_F, EV_G, EV_GRAD_F, EV_JAC_G, EV_HESS )
      IF ( iproblem .EQ. 0 ) THEN
        write(*,*) 'Error creating Ipopt Problem.'
        STOP
      END IF
      ierr = IPSOLVE( iproblem, X, G, F, LAM, Z_L, Z_U, IDAT, DAT )
      CALL IPFREE( iproblem )

C     Display CUTEst statistics

      CALL CUTEST_creport_r( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910
      WRITE( out, 2000 ) pname, n, m, CALLS( 1 ), CALLS( 2 ),
     *     CALLS( 3 ), CALLS( 4 ), CALLS( 5 ), CALLS( 6 ), CALLS( 7 ),
     *     ierr, f, CPU( 1 ), CPU( 2 )

      CALL CUTEST_cterminate_r( status )
      STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )")
     *   status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP

 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *     ,/,' Package used            :  IPOPT',    /
     *     ,' Problem                 :  ', A10,    /
     *     ,' # variables             =      ', I10 /
     *     ,' # constraints           =      ', I10 /
     *     ,' # objective functions   =      ', E15.7 /
     *     ,' # objective gradients   =      ', E15.7 /
     *     ,' # objective Hessians    =      ', E15.7 /
     *     ,' # Hessian-vector prdct  =      ', E15.7 /
     *     ,' # constraints functions =      ', E15.7 /
     *     ,' # constraints gradients =      ', E15.7 /
     *     ,' # constraints Hessians  =      ', E15.7 /
     *     ,' Exit code               =      ', I10 /
     *     ,' Final f                 = ', E15.7 /
     *     ,' Set up time             =      ', 0P, F10.2, ' seconds' /
     *     ,' Solve time              =      ', 0P, F10.2, ' seconds' //
     *     ,/,66('*') / )

      END

C Copyright (C) 2002, Carnegie Mellon University and others.
C All Rights Reserved.
C This code is published under the Eclipse Public License.
C*******************************************************************************
C
      subroutine EV_F(N, X, NEW_X, F, IDAT, DAT, IERR)
C
C*******************************************************************************
C
C    $Id: CUTEstInterface.f 1861 2010-12-21 21:34:47Z andreasw $
C
C-------------------------------------------------------------------------------
C                                 Title
C-------------------------------------------------------------------------------
C
CT    Compute objective function value to CUTEst problem
C
C-------------------------------------------------------------------------------
C                          Programm description
C-------------------------------------------------------------------------------
C
CB
C
C-------------------------------------------------------------------------------
C                             Author, date
C-------------------------------------------------------------------------------
C
CA    Andreas Waechter      02/25/99
CA    Andreas Waechter      10/29/04 adapted for C++ version
CA    Nick Gould            15/01/13 adapted for CUTEst

C
C-------------------------------------------------------------------------------
C                             Documentation
C-------------------------------------------------------------------------------
C
CD
C
C-------------------------------------------------------------------------------
C                             Parameter list
C-------------------------------------------------------------------------------
C
C    Name     I/O   Type   Meaning
C
CP   N         I    INT    number of variables in problem statement
CP   X         I    DP     point where F is to be evaluated
CP   NEW_X     I    INT    if 1, X has not been changed since last call
CP   F         O    DP     objective function value
CP   IDAT      P    INT    privat INT data for evaluation routines
CP   DAT       P    DP     privat DP data for evaluation routines
CP   IERR      O    INT    set to nonzero value if error occurred
C
C-------------------------------------------------------------------------------
C                             local variables
C-------------------------------------------------------------------------------
C
CL
C
C-------------------------------------------------------------------------------
C                             used subroutines
C-------------------------------------------------------------------------------
C
CS    CUTEST_cofg
C
C*******************************************************************************
C
C                              Declarations
C
C*******************************************************************************
C
      USE CUTEST_KINDS_precision
      IMPLICIT NONE
C
C-------------------------------------------------------------------------------
C                             Parameter list
C-------------------------------------------------------------------------------
C
      integer ( kind = ip_ ) N
      real ( kind = rp_ ) X(N)
      integer ( kind = ip_ ) NEW_X
      real ( kind = rp_ ) F
      real ( kind = rp_ ) DAT(*)
      integer ( kind = ip_ ) IDAT(*)
      integer ( kind = ip_ ) IERR
C
C*******************************************************************************
C
C                           Executable Statements
C
C*******************************************************************************
C
      IERR = 0
C
C     Call COFG to obtain value of objective function
C
      call CUTEST_cofg_r( ierr,  N, X, F, DAT, .false.)

      return
      end
C Copyright (C) 2002, Carnegie Mellon University and others.
C All Rights Reserved.
C This code is published under the Eclipse Public License.
C*******************************************************************************
C
      subroutine EV_GRAD_F(N, X, NEW_X, GRAD, IDAT, DAT, IERR)
C
C*******************************************************************************
C
C    $Id: CUTEstInterface.f 1861 2010-12-21 21:34:47Z andreasw $
C
C-------------------------------------------------------------------------------
C                                 Title
C-------------------------------------------------------------------------------
C
CT    Compute gradient of objective function to CUTEst problem
C
C-------------------------------------------------------------------------------
C                          Programm description
C-------------------------------------------------------------------------------
C
CB
C
C-------------------------------------------------------------------------------
C                             Author, date
C-------------------------------------------------------------------------------
C
CA    Andreas Waechter      02/25/99
CA    Andreas Waechter      10/29/04 adapted for C++ version
CA    Nick Gould            15/01/13 adapted for CUTEst
C
C-------------------------------------------------------------------------------
C                             Documentation
C-------------------------------------------------------------------------------
C
CD
C
C-------------------------------------------------------------------------------
C                             Parameter list
C-------------------------------------------------------------------------------
C
C    Name     I/O   Type   Meaning
C
CP   N         I    INT    number of variables in problem statement
CP                            (including slacks for inequality constraints)
CP   X         I    DP     point where G is to be evaluated
CP   NEW_X     I    INT    if 1, X has not been changed since last call
CP   GRAD      O    DP     gradient of objective function
CP   IDAT      P    INT    privat INT data for evaluation routines
CP   DAT       P    DP     privat DP data for evaluation routines
CP   IERR      O    INT    set to nonzero value if error occurred
C
C-------------------------------------------------------------------------------
C                             local variables
C-------------------------------------------------------------------------------
C
CL
C
C-------------------------------------------------------------------------------
C                             used subroutines
C-------------------------------------------------------------------------------
C
CS    CUTEST_cofg
C
C*******************************************************************************
C
C                              Declarations
C
C*******************************************************************************
C
      USE CUTEST_KINDS_precision
      IMPLICIT NONE
C
C-------------------------------------------------------------------------------
C                             Parameter list
C-------------------------------------------------------------------------------
C
      integer ( kind = ip_ ) N
      real ( kind = rp_ ) X(N)
      integer ( kind = ip_ ) NEW_X
      real ( kind = rp_ ) GRAD(N)
      real ( kind = rp_ ) DAT(*)
      integer ( kind = ip_ ) IDAT(*)
      integer ( kind = ip_ ) IERR
C
C-------------------------------------------------------------------------------
C                            Local varibales
C-------------------------------------------------------------------------------
C
      real ( kind = rp_ ) f
C
C*******************************************************************************
C
C                           Executable Statements
C
C*******************************************************************************
C
      IERR = 0
C
C     Call COFG to obtain gradient of objective function
C
      call CUTEST_cofg_r( ierr, N, X, f, GRAD, .true.)

      return
      end
C Copyright (C) 2002, Carnegie Mellon University and others.
C All Rights Reserved.
C This code is published under the Eclipse Public License.
C*******************************************************************************
C
      subroutine EV_G(N, X, NEW_X, M, G, IDAT, DAT, IERR)
C
C*******************************************************************************
C
C    $Id: CUTEstInterface.f 1861 2010-12-21 21:34:47Z andreasw $
C
C-------------------------------------------------------------------------------
C                                 Title
C-------------------------------------------------------------------------------
C
CT    Compute values of constraints to CUTEst problem
C
C-------------------------------------------------------------------------------
C                          Programm description
C-------------------------------------------------------------------------------
C
CB
C
C-------------------------------------------------------------------------------
C                             Author, date
C-------------------------------------------------------------------------------
C
CA    Andreas Waechter      02/25/99
CA    Andreas Waechter      07/01/99 BUG: problems if ineq not first
CA    Andreas Waechter      10/29/04 adapted for C++ version
CA    Nick Gould            15/01/13 adapted for CUTEst
C
C-------------------------------------------------------------------------------
C                             Documentation
C-------------------------------------------------------------------------------
C
CD
C
C-------------------------------------------------------------------------------
C                             Parameter list
C-------------------------------------------------------------------------------
C
C    Name     I/O   Type   Meaning
C
CP   N         I    INT    number of variables in problem statement
CP                            (including slacks for inequality constraints)
CP   X         I    DP     point where G is to be evaluated
CP   NEW_X     I    INT    if 1, X has not been changed since last call
CP   M         I    INT    number of constraints
CP   G         O    DP     values of constraints
CP   IDAT      P    INT    privat INT data for evaluation routines
CP   DAT       P    DP     privat DP data for evaluation routines
CP   IERR      O    INT    set to nonzero value if error occurred
C
C-------------------------------------------------------------------------------
C                             local variables
C-------------------------------------------------------------------------------
C
CL
C
C-------------------------------------------------------------------------------
C                             used subroutines
C-------------------------------------------------------------------------------
C
CS    CUTEST_ccfg
C
C*******************************************************************************
C
C                              Declarations
C
C*******************************************************************************
C
      USE CUTEST_KINDS_precision
      IMPLICIT NONE
C
C-------------------------------------------------------------------------------
C                             Parameter list
C-------------------------------------------------------------------------------
C
      integer ( kind = ip_ ) N
      real ( kind = rp_ ) X(N)
      integer ( kind = ip_ ) NEW_X
      integer ( kind = ip_ ) M
      real ( kind = rp_ ) G(M)
      real ( kind = rp_ ) DAT(*)
      integer ( kind = ip_ ) IDAT(*)
      integer ( kind = ip_ ) IERR
C
C*******************************************************************************
C
C                           Executable Statements
C
C*******************************************************************************
C
      IERR = 0
C
C     Call CCFG to obtain constraint values, but without slacks
C
      call CUTEST_ccfg_r( ierr, N, M, X, G, .FALSE., 1, 1, DAT, .FALSE.)

      return
      end
C Copyright (C) 2002, Carnegie Mellon University and others.
C All Rights Reserved.
C This code is published under the Eclipse Public License.
C*******************************************************************************
C
      subroutine EV_JAC_G(TASK, N, X, NEW_X, M, NZ, ACON, AVAR, A,
     1     IDAT, DAT, IERR)
C
C*******************************************************************************
C
C    $Id: CUTEstInterface.f 1861 2010-12-21 21:34:47Z andreasw $
C
C-------------------------------------------------------------------------------
C                                 Title
C-------------------------------------------------------------------------------
C
CT    Compute Jacobian of constraints to CUTEst problem
C
C-------------------------------------------------------------------------------
C                          Programm description
C-------------------------------------------------------------------------------
C
CB
C
C-------------------------------------------------------------------------------
C                             Author, date
C-------------------------------------------------------------------------------
C
CA    Andreas Waechter      02/25/99
CA    Andreas Waechter      10/29/04 adapted for C++ version
CA    Nick Gould            15/01/13 adapted for CUTEst
C
C-------------------------------------------------------------------------------
C                             Documentation
C-------------------------------------------------------------------------------
C
CD
C
C-------------------------------------------------------------------------------
C                             Parameter list
C-------------------------------------------------------------------------------
C
C    Name     I/O   Type   Meaning
C
CP   TASK      I    INT     =0: Fill ACON and AVAR, don't use A
CP                         <>0: Fill A, don't use ACON, AVAR
CP   N         I    INT    number of variables in problem statement
CP   X         I    DP     point where A is to be evaluated
CP   NEW_X     I    INT    if 1, X has not been changed since last call
CP   M         I    INT    number of constraints
CP   NZ        I    INT    number of nonzero elements
CP                                     (size of A, AVAR, ACON)
CP   ACON      O    INT    (only TASK=0) row indices
CP   AVAR      O    INT    (only TASK=0) column indices
CP   A         O    DP     (only TASK<>0) values in Jacobian
CP   IDAT      P    INT    privat INT data for evaluation routines
CP   DAT       P    DP     privat DP data for evaluation routines
CP   IERR      O    INT    set to nonzero value if error occurred
C
C-------------------------------------------------------------------------------
C                             local variables
C-------------------------------------------------------------------------------
C
CL
C
C-------------------------------------------------------------------------------
C                             used subroutines
C-------------------------------------------------------------------------------
C
CS    CUTEST_cdimsj
CS    CUTEST_ccfsg
C
C*******************************************************************************
C
C                              Declarations
C
C*******************************************************************************
C
      USE CUTEST_KINDS_precision
      IMPLICIT NONE
C
C-------------------------------------------------------------------------------
C                             Parameter list
C-------------------------------------------------------------------------------
C
      integer ( kind = ip_ ) TASK
      integer ( kind = ip_ ) N
      real ( kind = rp_ ) X(N)
      integer ( kind = ip_ ) NEW_X
      integer ( kind = ip_ ) M
      integer ( kind = ip_ ) NZ
      real ( kind = rp_ ) A(NZ)
      integer ( kind = ip_ ) ACON(NZ)
      integer ( kind = ip_ ) AVAR(NZ)
      real ( kind = rp_ ) DAT(*)
      integer ( kind = ip_ ) IDAT(*)
      integer ( kind = ip_ ) IERR
C
C-------------------------------------------------------------------------------
C                            Local varibales
C-------------------------------------------------------------------------------
C
      integer ( kind = ip_ ) i, nele_jac
C
C*******************************************************************************
C
C                           Executable Statements
C
C*******************************************************************************
C
      IERR = 0
      if( TASK.eq.0 ) then
C
C     Get the nonzero structure
C
         do i = 1, n
            DAT(i) = 0.0_rp_
         enddo
         call CUTEST_ccfsg_r( ierr, n, m, DAT(1), DAT(N+1), nele_jac,
     1        nz, DAT(2*n+1), AVAR, ACON, .TRUE.)
      else
C
C     Get the values of nonzeros
C
         call CUTEST_ccfsg_r( ierr, N, M, X, DAT(1), nele_jac,
     1        NZ, A, IDAT(1), IDAT(1+NZ), .TRUE.)
      endif

      return
      end
C Copyright (C) 2002, Carnegie Mellon University and others.
C All Rights Reserved.
C This code is published under the Eclipse Public License.
C*******************************************************************************
C

      subroutine EV_HESS(TASK, N, X, NEW_X, OBJFACT, M, LAM, NEW_LAM,
     1     NNZH, IRNH, ICNH, HESS, IDAT, DAT, IERR)
C
C*******************************************************************************
C
C    $Id: CUTEstInterface.f 1861 2010-12-21 21:34:47Z andreasw $
C
C-------------------------------------------------------------------------------
C                                 Title
C-------------------------------------------------------------------------------
C
CT    Compute Hessian of Lagrangian for CUTEst problem
C
C-------------------------------------------------------------------------------
C                          Programm description
C-------------------------------------------------------------------------------
C
CB
C
C-------------------------------------------------------------------------------
C                             Author, date
C-------------------------------------------------------------------------------
C
CA    Andreas Waechter      03/23/00
CA    Andreas Waechter      10/29/04 adapted for C++ version
CA    Nick Gould            15/01/13 adapted for CUTEst
C
C-------------------------------------------------------------------------------
C                             Documentation
C-------------------------------------------------------------------------------
C
CD
C
C-------------------------------------------------------------------------------
C                             Parameter list
C-------------------------------------------------------------------------------
C
C    Name     I/O   Type   Meaning
C
CP   TASK      I    INT     =0: Fill IRNH and ICNH, don't use HESS
CP                         <>0: Fill HESS, don't use IRNH, ICNH
CP   N         I    INT    number of variables in problem statement
CP   X         I    DP     point where A is to be evaluated
CP   NEW_X     I    INT    if 1, X has not been changed since last call
CP   OBJFACT   I    DP     weighting factor for objective function Hessian
CP   M         I    INT    number of constriants
CP   LAM       I    DP     weighting factors for the constraints
CP   NEW_LAM   I    INT    if 1, LAM has not been changed since last call
CP   NNZH      I    INT    number of nonzero elements
CP                                     (size of HESS, IRNH, ICNH)
CP   IRNH      O    INT    (only TASK=0) row indices
CP   ICNH      O    INT    (only TASK=0) column indices
CP   HESS      O    DP     (only TASK<>0) values in Hessian
CP   IDAT      P    INT    privat INT data for evaluation routines
CP   DAT       P    DP     privat DP data for evaluation routines
CP   IERR      O    INT    set to nonzero value if error occurred
C
C-------------------------------------------------------------------------------
C                             local variables
C-------------------------------------------------------------------------------
C
CL
C
C-------------------------------------------------------------------------------
C                             used subroutines
C-------------------------------------------------------------------------------
C
CS    CUTEST_csh
CS    CUTEST_cshc
C
C*******************************************************************************
C
C                              Declarations
C
C*******************************************************************************
C
      USE CUTEST_KINDS_precision
      IMPLICIT NONE
C
C-------------------------------------------------------------------------------
C                             Parameter list
C-------------------------------------------------------------------------------
C
      integer ( kind = ip_ ) TASK
      integer ( kind = ip_ ) N
      real ( kind = rp_ ) X(N)
      integer ( kind = ip_ ) NEW_X
      real ( kind = rp_ ) OBJFACT
      integer ( kind = ip_ ) M
      real ( kind = rp_ ) LAM(M)
      integer ( kind = ip_ ) NEW_LAM
      integer ( kind = ip_ ) NNZH
      integer ( kind = ip_ ) IRNH(NNZH)
      integer ( kind = ip_ ) ICNH(NNZH)
      real ( kind = rp_ ) HESS(NNZH)
      real ( kind = rp_ ) DAT(*)
      integer ( kind = ip_ ) IDAT(*)
      integer ( kind = ip_ ) IERR
C
C-------------------------------------------------------------------------------
C                            Local varibales
C-------------------------------------------------------------------------------
C
      integer ( kind = ip_ ) i, nnzh2
C
C*******************************************************************************
C
C                           Executable Statements
C
C*******************************************************************************
C
      IERR = 0
      if( TASK.eq.0 ) then
C
C     Get the nonzero structure
C
         do i = 1, N
            DAT(i) = 0.0_rp_
         enddo
         call CUTEST_csh_r( ierr, N, M, DAT(1), DAT(1),
     1                    nnzh2, NNZH, DAT(N+1), IRNH, ICNH)
      else
C
C     Call CSH to get the values
C
         if( OBJFACT.ne.0.0_rp_ ) then

            if( OBJFACT.ne.1.0_rp_ ) then
               do i = 1, M
                  DAT(i) = LAM(i)/OBJFACT
               enddo
               call CUTEST_csh_r( ierr, N, M, X, DAT(1),
     1                       nnzh2, NNZH, HESS, IDAT(1), IDAT(1+NNZH))
               do i = 1, NNZH
                  HESS(i) = HESS(i)*OBJFACT
               enddo
            else
               call CUTEST_csh_r( ierr, N, M, X, LAM, nnzh2, NNZH, HESS,
     1              IDAT(1), IDAT(1+NNZH))
            endif

         else
C     now we have to call CSH twice, since we can't otherwise get rid of
C     the objective function entries
            do i = 1, M
               DAT(i) = 0.0_rp_
            enddo
C           call CUTEST_csh_r( ierr, N, M, X, DAT(1), nnzh2,
C    1           NNZH, DAT(1+M), IDAT(1), IDAT(1+NNZH))
C           IF ( ierr /= 0 ) RETURN
            call CUTEST_cshc_r( ierr, N, M, X, LAM, nnzh2, NNZH, HESS,
     1           IDAT(1), IDAT(1+NNZH))
C           do i = 1, NNZH
C              HESS(i) = HESS(i) - DAT(M+i)
C           enddo
         endif
      endif

      return
      end

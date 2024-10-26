! THIS VERSION: CUTEST 2.3 - 2024-10-20 AT 11:55 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-  C U T E S T    C S G R S H P _ C   S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 20th October 2024

      SUBROUTINE CUTEST_csgrshp_c_r( status, n, nnzj, lj, J_var, J_fun,        &
                                     nnzh, lh, H_row, H_col )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lj, lh
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nnzh, nnzj, status
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lj ) :: J_var, J_fun
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: H_row, H_col

!  ------------------------------------------------------------------------
!  compute the spasity pattern of the gradients of the objective function
!  and general constraints and the Hessian matrix of the associated
!  Lagrangian function of a group partially separable function.

!  The gradients are stored as a sparse matrix in 0-based coordinate form.
!  The i-th entry of this matrix represents the derivative of
!  function J_fun(i) with respect to variable J_var(i) for
!  i = 1, ..., nnzj, where function -1 is the objective function, and
!  function j>=0  is the jth constraint. The upper triangle of the Hessian
!  is stored in coordinate form, i.e., the entry has 0-based row index
!  H_row(i) and 0-based column index H_col(i) for i = 1, ...., nnzh
!  -----------------------------------------------------------------------

      CALL CUTEST_csgrshp_threadsafe_r( CUTEST_data_global,                    &
                                        CUTEST_work_global( 1 ),               &
                                        status, n, nnzj, lj, J_var, J_fun,     &
                                        nnzh, lh, H_row, H_col )

      J_var( : nnzj ) = J_var( : nnzj ) - 1
      J_fun( : nnzj ) = J_fun( : nnzj ) - 1
      H_row( : nnzh ) = H_row( : nnzh ) - 1
      H_col( : nnzh ) = H_col( : nnzh ) - 1

      RETURN

!  end of subroutine CUTEST_csgrshp_c_r

      END SUBROUTINE CUTEST_csgrshp_c_r

!-*-*-*-*-*-*-  C U T E S T    C S G R S H P   S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 29th March 2017

      SUBROUTINE CUTEST_csgrshp_r( status, n, nnzj, lj, J_var, J_fun,          &
                                   nnzh, lh, H_row, H_col )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lj, lh
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nnzh, nnzj, status
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lj ) :: J_var, J_fun
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: H_row, H_col

!  ------------------------------------------------------------------------
!  compute the spasity pattern of the gradients of the objective function
!  and general constraints and the Hessian matrix of the associated
!  Lagrangian function of a group partially separable function.

!  The gradients are stored as a sparse matrix in coordinate form.
!  The i-th entry of this matrix represents the derivative of
!  function J_fun(i) with respect to variable J_var(i) for
!  i = 1, ..., nnzj, where function 0 is the objective function, and
!  function j>0  is the jth constraint. The upper triangle of the Hessian
!  is stored in coordinate form, i.e., the entry has row index
!  H_row(i) and column index H_col(i) for i = 1, ...., nnzh
!  -----------------------------------------------------------------------

      CALL CUTEST_csgrshp_threadsafe_r( CUTEST_data_global,                    &
                                        CUTEST_work_global( 1 ),               &
                                        status, n, nnzj, lj, J_var, J_fun,     &
                                        nnzh, lh, H_row, H_col )
      RETURN

!  end of subroutine CUTEST_csgrshp_r

      END SUBROUTINE CUTEST_csgrshp_r

!-*-  C U T E S T   C S G R S H P _ t h r e a d s a f e   S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 29th March 2017

      SUBROUTINE CUTEST_csgrshp_threadsafe_r( data, work, status, n,           &
                                              nnzj, lj, J_var, J_fun,          &
                                              nnzh, lh, H_row, H_col )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lj, lh
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nnzh, nnzj, status
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lj ) :: J_var, J_fun
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: H_row, H_col

!  ------------------------------------------------------------------------
!  compute the spasity pattern of the gradients of the objective function
!  and general constraints and the Hessian matrix of the associated
!  Lagrangian function of a group partially separable function.

!  The gradients are stored as a sparse matrix in coordinate form.
!  The i-th entry of this matrix represents the derivative of
!  function J_fun(i) with respect to variable J_var(i) for
!  i = 1, ..., nnzj, where function 0 is the objective function, and
!  function j>0  is the jth constraint. The upper triangle of the Hessian
!  is stored in coordinate form, i.e., the entry has row index
!  H_row(i) and column index H_col(i) for i = 1, ...., nnzh
!  -----------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, ig, icon, alloc_status
      CHARACTER ( LEN = 80 ) :: bad_alloc = REPEAT( ' ', 80 )
      REAL :: time_in, time_out

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  determine the Jacobian pattern

      nnzj = 0

!  consider the ig-th group

      IF ( data%numcon > 0 ) THEN
        DO ig = 1, data%ng
          icon = data%KNDOFC( ig )

!  the group defines a constraint

          IF ( icon /= 0 ) THEN
            DO i = data%ISTAGV( ig ), data%ISTAGV( ig + 1 ) - 1
              nnzj = nnzj + 1
              IF ( nnzj <= lj ) THEN
                J_fun( nnzj ) = icon
                J_var( nnzj ) = data%ISVGRP( i )
              END IF
            END DO
          END IF
        END DO
      END IF

!  transfer the gradient of the objective function to the sparse storage scheme

      DO i = 1, n
        nnzj = nnzj + 1
        IF ( nnzj <= lj ) THEN
          J_fun( nnzj ) = 0
          J_var( nnzj ) = i
        END IF
      END DO

!  exit if the space provided was insufficient

     IF ( nnzj > lj ) THEN
       IF ( data%out > 0 ) WRITE( data%out,                                    &
          "( /, ' ** SUBROUTINE CSGRSHP: array length lj too small.',          &
         &    /, ' -- Increase the parameter lj to at least ', I0 )" ) nnzj
        status = 2 ; GO TO 990
      END IF

      work%firstg = .FALSE.

!  determine the Hessian pattern

      CALL CUTEST_assemble_hessian_pattern(                                    &
             n, data%ng, data%nel, data%ntotel, data%nvrels, data%nvargp,      &
             data%IELVAR, data%IELING, data%ISTADG,                            &
             data%ISTAEV, data%ISTAGV, data%ISVGRP, data%GXEQX,                &
             0_ip_, data%out, data%out, status, alloc_status, bad_alloc,       &
             work%hessian_setup_complete, work%lh_row, work%lh_col,            &
             work%H_row, work%H_col, work%ROW_start, work%POS_in_H, work%USED, &
             work%FILLED, work%lrowst, work%lpos, work%lused, work%lfilled,    &
             nnzh )

!  record the pattern if it is available

      IF ( status == 0 ) THEN
        H_row( : nnzh ) = work%H_row( : nnzh )
        H_col( : nnzh ) = work%H_col( : nnzh )

!  update the counters for the report tool

        work%nc2cg = work%nc2cg + work%pnc
        work%nc2og = work%nc2og + 1
        work%nc2oh = work%nc2oh + 1
        work%nc2ch = work%nc2ch + work%pnc
      END IF

!  update elapsed CPU time if required

 990  CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_csgrshp = work%time_csgrshp + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_csgrshp_threadsafe_r

      END SUBROUTINE CUTEST_csgrshp_threadsafe_r

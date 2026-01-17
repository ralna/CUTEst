! THIS VERSION: CUTEST 2.6 - 2026-01-16 AT 11:20 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-  C U T E S T    C I S H P _ C   S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 16th January 2026

      SUBROUTINE CUTEST_cishp_c_r( status, n, iprob, nnzh, lh, H_row, H_col )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, iprob, lh
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nnzh, status
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: H_row, H_col

!  ------------------------------------------------------------------------
!  compute the spasity pattern of the Hessian matrix of a specified 
!  problem function (iprob = 0 is the objective function, while iprob > 0 
!  is the iprob-th constraint) of a problem initially written in
!  Standard Input Format (SIF).
!
!  The upper triangle of the Hessian is stored in 0-based coordinate form, 
!  i.e., the entry has 0-based row index H_row(i) and column index H_col(i) 
!  for i = 1, ...., nnzh
!  ------------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: iprob_fortran

      iprob_fortran = iprob + 1
      CALL CUTEST_cishp_threadsafe_r( CUTEST_data_global,                      &
                                      CUTEST_work_global( 1 ), status, n,      &
                                      iprob_fortran, nnzh, lh, H_row, H_col )
      H_row( : nnzh ) = H_row( : nnzh ) - 1
      H_col( : nnzh ) = H_col( : nnzh ) - 1
      RETURN

!  end of subroutine CUTEST_cishp_c_r

      END SUBROUTINE CUTEST_cishp_c_r

!-*-*-*-*-*-*-*-  C U T E S T    C I S H P  S U B R O U T I N E  -*-*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 16th January 2026

      SUBROUTINE CUTEST_cishp_r( status, n, iprob, nnzh, lh, H_row, H_col )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, iprob, lh
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nnzh, status
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: H_row, H_col

!  ----------------------------------------------------------------------
!  compute the spasity pattern of the Hessian matrix of a specified 
!  problem function (iprob = 0 is the objective function, while iprob > 0 
!  is the iprob-th constraint) of a problem initially written in
!  Standard Input Format (SIF).

!  The upper triangle of the Hessian is stored in coordinate form,
!  i.e., the entry H_val(i) has row index H_row(i) and column index
!  H_col(i) for i = 1, ...., nnzh
!  ----------------------------------------------------------------------

      CALL CUTEST_cishp_threadsafe_r( CUTEST_data_global,                      &
                                      CUTEST_work_global( 1 ), status, n,      &
                                      iprob, nnzh, lh, H_row, H_col )
      RETURN

!  end of subroutine CUTEST_cishp_r

      END SUBROUTINE CUTEST_cishp_r

!-*-*-  C U T E S T   C I S H P _ t h r e a d s a f e   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 16th January 2026

      SUBROUTINE CUTEST_cishp_threadsafe_r( data, work, status, n,             &
                                            iprob, nnzh, lh, H_row, H_col )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, iprob, lh
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nnzh, status
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: H_row, H_col

!  ----------------------------------------------------------------------
!  compute the spasity pattern of the Hessian matrix of a specified 
!  problem function (iprob = 0 is the objective function, while iprob > 0 
!  is the iprob-th constraint) of a problem initially written in
!  Standard Input Format (SIF).

!  The upper triangle of the Hessian is stored in coordinate form,
!  i.e., the entry H_val(i) has row index H_row(i) and column index
!  H_col(i) for i = 1, ...., nnzh
!  ----------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: alloc_status, i
      REAL :: time_in, time_out
      CHARACTER ( LEN = 80 ) :: bad_alloc = REPEAT( ' ', 80 )

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  check input parameters

      IF ( iprob < 0 ) THEN
         IF ( data%out > 0 ) WRITE( data%out, "( ' ** SUBROUTINE CISHP: ',     &
        &    'invalid problem index iprob = ', I0 )" ) iprob
         status = 2 ; GO TO 990
      END IF

!  record all groups that are of the correct problem number in LOGIC

      DO i = 1, data%ng
        work%LOGIC( i ) = data%KNDOFC( i ) == iprob
      END DO

!  determine the Hessian pattern

      CALL CUTEST_assemble_hessian_i_pattern(                                  &
             n, data%ng, data%nel, data%ntotel, data%nvrels, data%nvargp,      &
             data%IELVAR, data%IELING, data%ISTADG,                            &
             data%ISTAEV, data%ISTAGV, data%ISVGRP, data%GXEQX,                &
             0_ip_, data%out, data%out, status, alloc_status, bad_alloc,       &
             work%hessian_setup_complete, work%lh_row, work%lh_col,            &
             work%H_row, work%H_col, work%ROW_start, work%POS_in_H, work%USED, &
             work%FILLED, work%lrowst, work%lpos, work%lused, work%lfilled,    &
             nnzh, work%LOGIC )

!  check for errors

      IF ( status > 0 ) GO TO 990

!  record the sparse Hessian

      H_row( : nnzh ) = work%H_row( : nnzh )
      H_col( : nnzh ) = work%H_col( : nnzh )

!  update the counters for the report tool

      work%nc2oh = work%nc2oh + 1
      status = 0

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_cishp = work%time_cishp + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_cishp_threadsafe_r

      END SUBROUTINE CUTEST_cishp_threadsafe_r

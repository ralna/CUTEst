! THIS VERSION: CUTEST 2.3 - 2024-10-20 AT 11:45 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-  C U T E S T    C C H P R O D S P _ C   S U B R O U T I N E  -*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 20th October 2024

      SUBROUTINE CUTEST_cchprodsp_c_r( status, m, lchp, CHP_ind, CHP_ptr )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: m, lchp
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( m + 1 ) :: CHP_ptr
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( lchp ) :: CHP_ind

!  ----------------------------------------------------------------------------
!  determine the integer storage arrays needed when computing the matrix-vector
!  products H_i(x) v, i = 1, ..., m, between each of the Hessian matrices
!  H_i(x) of the constraint functions for the problem and a given vector v.
!  The 0-based indices of the nonzero entries of the resulting products H_i(x) v
!  occur in CHP_ind), k = CHP_ptr(i)+1,..., CHP_ptr(i+1), i = 1, ..., m.
!  ----------------------------------------------------------------------------

      CALL CUTEST_cchprodsp_threadsafe_r( CUTEST_data_global,                  &
                                          CUTEST_work_global( 1 ),             &
                                          status, m, lchp, CHP_ind, CHP_ptr )

      CHP_ind( : CHP_ptr( m + 1 ) - 1 ) = CHP_ind( : CHP_ptr( m + 1 ) - 1 ) - 1
      CHP_ptr( : m + 1 ) = CHP_ptr( : m + 1 ) - 1

      RETURN

!  end of subroutine CUTEST_cchprodsp_c_r

      END SUBROUTINE CUTEST_cchprodsp_c_r

!-*-*-*-*-*-  C U T E S T    C C H P R O D S P   S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 29 March 2017

      SUBROUTINE CUTEST_cchprodsp_r( status, m, lchp, CHP_ind, CHP_ptr )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: m, lchp
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( m + 1 ) :: CHP_ptr
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( lchp ) :: CHP_ind

!  ----------------------------------------------------------------------------
!  determine the integer storage arrays needed when computing the matrix-vector
!  products H_i(x) v, i = 1, ..., m, between each of the Hessian matrices
!  H_i(x) of the constraint functions for the problem and a given vector v.
!  The indices of the nonzero entries of the resulting products H_i(x) v
!  occur in CHP_ind), k = CHP_ptr(i),..., CHP_ptr(i+1)-1, i = 1, ..., m.
!  ----------------------------------------------------------------------------

      CALL CUTEST_cchprodsp_threadsafe_r( CUTEST_data_global,                  &
                                          CUTEST_work_global( 1 ),             &
                                          status, m, lchp, CHP_ind, CHP_ptr )
      RETURN

!  end of subroutine CUTEST_cchprodsp_r

      END SUBROUTINE CUTEST_cchprodsp_r

!-  C U T E S T  C C H P R O D S P _ t h r e a d s a f e  S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 29 March 2017

      SUBROUTINE CUTEST_cchprodsp_threadsafe_r( data, work, status, m,         &
                                                lchp, CHP_ind, CHP_ptr )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: m, lchp
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( m + 1 ) :: CHP_ptr
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( lchp ) :: CHP_ind

!  ----------------------------------------------------------------------------
!  determine the integer storage arrays needed when computing the matrix-vector
!  products H_i(x) v, i = 1, ..., m, between each of the Hessian matrices
!  H_i(x) of the constraint functions for the problem and a given vector v.
!  The indices of the nonzero entries of the resulting products H_i(x) v
!  occur in CHP_ind), k = CHP_ptr(i),..., CHP_ptr(i+1)-1, i = 1, ..., m.
!  ----------------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: ic, ig, k, ls
      REAL :: time_in, time_out

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  set the indices for the nonzeros for each constraint Hessian product in turn

      ls = 1
      DO ic = 1, m
        ig = data%CGROUP( ic )
        CHP_ptr( ic ) = ls
        DO k = data%ISTAGV( ig ), data%ISTAGV( ig + 1 ) - 1
          CHP_ind( ls ) = data%ISVGRP( k )
          ls = ls + 1
        END DO
      END DO
      CHP_ptr( m + 1 ) = ls

!  update elapsed CPU time if required

      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_cchprodsp = work%time_cchprodsp + time_out - time_in
      END IF
      status = 0

      RETURN

!  end of subroutine CUTEST_cchprodsp_threadsafe_r

      END SUBROUTINE CUTEST_cchprodsp_threadsafe_r

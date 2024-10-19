! THIS VERSION: CUTEST 2.2 - 2023-11-12 AT 10:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-  C U T E S T   C D I M S H    S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 29th December 2012

      SUBROUTINE CUTEST_cdimsh_r( status, nnzh )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, nnzh

!  ------------------------------------------------------------------------
!  Compute the space required to store the Hessian matrix of the Lagrangian
!  function of a problem initially written in Standard Input Format (SIF)

!  NB. CSETUP must have been called first

!  the upper triangle of the Hessian is stored in coordinate form,
!  i.e., the entry H_val(i) has row index H_row(i) for i = 1, ...., nnzh.

!  ------------------------------------------------------------------------

      CALL CUTEST_cdimsh_threadsafe_r( CUTEST_data_global,                     &
                                       CUTEST_work_global( 1 ),                &
                                       status, nnzh )
      RETURN

!  end of subroutine CUTEST_cdimsh_r

      END SUBROUTINE CUTEST_cdimsh_r

!-*-  C U T E S T   C D I M S H _ t h r e a d s a f e   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTEr, August 1999
!   fortran 2003 version released in CUTEst, 24th November 2012

      SUBROUTINE CUTEST_cdimsh_threadsafe_r( data, work, status, nnzh )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, nnzh

!  ------------------------------------------------------------------------
!  Compute the space required to store the Hessian matrix of the Lagrangian
!  function of a problem initially written in Standard Input Format (SIF)

!  NB. CSETUP must have been called first

!  the upper triangle of the Hessian is stored in coordinate form,
!  i.e., the entry H_val(i) has row index H_row(i) for i = 1, ...., nnzh.

!  ------------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: alloc_status
      CHARACTER ( LEN = 80 ) :: bad_alloc = REPEAT( ' ', 80 )

      IF ( work%nnzh < 0 ) THEN
        CALL CUTEST_size_sparse_hessian(                                       &
                        data%n, data%ng, data%nel, data%ntotel, data%nvrels,   &
                        data%nvargp, data%IELVAR, data%IELING,                 &
                        data%ISTADG, data%ISTAEV, data%ISTAGV, data%ISVGRP,    &
                        data%GXEQX, data%out, status,                          &
                        alloc_status, bad_alloc, work%hessian_setup_complete,  &
                        work%ROW_start, work%POS_in_H, work%USED, work%lrowst, &
                        work%lpos, work%lused, nnzh )
        IF ( status == 0 ) work%nnzh = nnzh
      ELSE
        nnzh = work%nnzh
      END IF

      RETURN

!  end of subroutine CUTEST_cdimsh_threadsafe_r

      END SUBROUTINE CUTEST_cdimsh_threadsafe_r

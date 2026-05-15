! THIS VERSION: CUTEST 2.6 - 2026-05-15 AT 15:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-  C U T E S T    C D I M S C J    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 15th May, 2026

      SUBROUTINE CUTEST_cdimscj_r( status, nnzj )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nnzj, status

!  -------------------------------------------------------------------------
!  compute the space required to store the Jacobian matrix of the 
!  constraints of a problem initially written in Standard Input Format (SIF)
!  -------------------------------------------------------------------------

      CALL CUTEST_cdimscj_threadsafe_r( CUTEST_data_global, status, nnzj )
      RETURN

!  end of sunroutine CUTEST_cdimscj_r

      END SUBROUTINE CUTEST_cdimscj_r

!-   C U T E S T   C D I M S C J _ t h r e a d s a f e   S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTEr, April 1999
!   fortran 2003 version released in CUTEst, 28th November 2012

      SUBROUTINE CUTEST_cdimscj_threadsafe_r( data, status, nnzj )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nnzj, status

!  -------------------------------------------------------------------------
!  compute the space required to store the Jacobian matrix of the 
!  constraints of a problem initially written in Standard Input Format (SIF)
!  -------------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: ig

!  the total space is stored in nnzj

      nnzj = 0

!  allow space for constraint groups

      DO ig = 1, data%ng
        IF ( data%KNDOFC( ig ) /= 0 )                                          &
          nnzj = nnzj + data%ISTAGV( ig + 1 ) - data%ISTAGV( ig )
      END DO

      status = 0
      RETURN

!  end of sunroutine CUTEST_cdimscj_threadsafe_r

      END SUBROUTINE CUTEST_cdimscj_threadsafe_r

! THIS VERSION: CUTEST 2.2 - 2023-11-12 AT 10:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-  C U T E S T    C D I M C H P    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 24th November 2015

      SUBROUTINE CUTEST_cdimchp_r( status, nnzchp )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, nnzchp

!  -----------------------------------------------------------------
!  compute the space required to store the matrix of products of the 
!  constraint Hessians with a vector of a problem initially written 
!  in Standard Input Format (SIF)
!  -----------------------------------------------------------------

      CALL CUTEST_cdimchp_threadsafe_r( CUTEST_data_global, status, nnzchp )
      RETURN

!  end of sunroutine CUTEST_cdimchp_r

      END SUBROUTINE CUTEST_cdimchp_r

!-  C U T E S T   C D I M C H P _ t h r e a d s a f e   S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 24th November 2015

      SUBROUTINE CUTEST_cdimchp_threadsafe_r( data, status, nnzchp )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, nnzchp

!  -----------------------------------------------------------------
!  compute the space required to store the matrix of products of the 
!  constraint Hessians with a vector of a problem initially written 
!  in Standard Input Format (SIF)
!  -----------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: ig

!  the total space is stored in nnzchp

      nnzchp = 0

!  allow space for constraint groups

      DO ig = 1, data%ng
        IF ( data%KNDOFC( ig ) /= 0 )                                          &
          nnzchp = nnzchp + data%ISTAGV( ig + 1 ) - data%ISTAGV( ig )
      END DO

      status = 0
      RETURN

!  end of sunroutine CUTEST_cdimchp_threadsafe_r

      END SUBROUTINE CUTEST_cdimchp_threadsafe_r

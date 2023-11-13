! THIS VERSION: CUTEST 2.2 - 2023-11-12 AT 10:30 GMT.

#include "cutest_routines.h"
#include "cutest_modules.h"

!-*-*-*-*-  C U T E S T    C T E R M I N A T E    S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28nd December 2012

      SUBROUTINE CUTEST_cterminate_r( status )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status

!  ------------------------------------
!  deallocate internal workspace arrays
!  ------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: alloc_status, thread
      CHARACTER ( LEN = 80 ) :: bad_alloc

!  deallocate global workspace

      CALL CUTEST_terminate_data( CUTEST_data_global,                          &
                                  status, alloc_status, bad_alloc )
      IF ( status /= 0 ) RETURN

!  deallocate thread-specific workspace

      DO thread = 1, CUTEST_data_global%threads
        CALL CUTEST_terminate_work( CUTEST_data_global,                        &
                                    CUTEST_work_global( thread ),              &
                                    status, alloc_status, bad_alloc )
        IF ( status /= 0 ) RETURN
      END DO

!  deallocate global work threads

      DEALLOCATE( CUTEST_work_global, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        status = 1000 + alloc_status
        bad_alloc = 'CUTEST_work_global'
        IF ( CUTEST_data_global%out > 0 ) WRITE( CUTEST_data_global%out,       &
         "( ' ** Message from -CUTEST_cterminate-', /, ' Deallocation ',       &
       &  'error for ', A, ', status = ', I0 )" ) bad_alloc, alloc_status
      END IF
      RETURN

!  End of subroutine CUTEST_cterminate_r

      END SUBROUTINE CUTEST_cterminate_r



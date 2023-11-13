! THIS VERSION: CUTEST 2.2 - 2023-11-12 AT 10:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-  C U T E S T   N E W T H R E A D _ t h r e a d s a f e   S U B R O U T I N E 

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 30th December 2012

      SUBROUTINE CUTEST_newthread_threadsafe_r( work, work_new,                &
                                              status, io_buffer )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_work_type ), INTENT( IN ) :: work
      TYPE ( CUTEST_work_type ), INTENT( OUT ) :: work_new
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: io_buffer
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status

!  ---------------------------------------------------
!  copy existing workspace data and set the i/o buffer
!  ---------------------------------------------------

!  copy the workspace

      work_new = work

!  set the i/o buffer

      work_new%io_buffer = io_buffer

      status = 0
      RETURN

!  end of subroutine CUTEST_newthread_threadsafe_r

      END SUBROUTINE CUTEST_newthread_threadsafe_r


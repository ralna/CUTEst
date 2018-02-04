! THIS VERSION: CUTEST 1.0 - 30/12/2012 AT 10:50 GMT.

!-  C U T E S T   N E W T H R E A D _ t h r e a d s a f e   S U B R O U T I N E 

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 30th December 2012

      SUBROUTINE CUTEST_newthread_threadsafe( work, work_new,                  &
                                              status, io_buffer )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      TYPE ( CUTEST_work_type ), INTENT( IN ) :: work
      TYPE ( CUTEST_work_type ), INTENT( OUT ) :: work_new
      INTEGER, INTENT( IN ) :: io_buffer
      INTEGER, INTENT( OUT ) :: status

!  ---------------------------------------------------
!  copy existing workspace data and set the i/o buffer
!  ---------------------------------------------------

!  copy the workspace

      work_new = work

!  set the i/o buffer

      work_new%io_buffer = io_buffer

      status = 0
      RETURN

!  end of subroutine CUTEST_newthread_threadsafe

      END SUBROUTINE CUTEST_newthread_threadsafe


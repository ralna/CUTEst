! THIS VERSION: CUTEST 1.0 - 28/12/2012 AT 15:50 GMT.

!-*-*-*-*-  C U T E S T    C T E R M I N A T E    S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28nd December 2012

      SUBROUTINE CUTEST_cterminate( status )
      USE CUTEST

!  dummy arguments

      INTEGER, INTENT( OUT ) :: status

!  ------------------------------------
!  deallocate internal workspace arrays
!  ------------------------------------

!  local variables

      INTEGER :: alloc_status, thread
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

!  End of subroutine CUTEST_cterminate

      END SUBROUTINE CUTEST_cterminate



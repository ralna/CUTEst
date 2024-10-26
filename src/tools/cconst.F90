! THIS VERSION: CUTEST 2.3 - 2024-10-23 AT 10:00 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-  C U T E S T    C C O N S T    S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 14th March 2016

      SUBROUTINE CUTEST_cconst_r( status, m, CONST )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: m
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: CONST

!  ---------------------------------------------------------------------
!  Compute the values of the constant terms for general constraints
!  of a function initially written in Standard Input Format (SIF)
!  ---------------------------------------------------------------------

      CALL CUTEST_cconst_threadsafe_r( CUTEST_data_global,                     &
                                       CUTEST_work_global( 1 ),                &
                                       status, m, CONST )
      RETURN

!  end of subroutine CUTEST_cconst_r

      END SUBROUTINE CUTEST_cconst_r

!-*-*-  C U T E S T    C C O N S T _ t h r e a d e d   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 14th March 2016

      SUBROUTINE CUTEST_cconst_threaded_r( status, m, CONST, thread )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: m, thread
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: CONST

!  ---------------------------------------------------------------------
!  Compute the values of the constant terms for general constraints
!  of a function initially written in Standard Input Format (SIF)
!  ---------------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_cconst_threadsafe_r( CUTEST_data_global,                     &
                                       CUTEST_work_global( thread ),           &
                                       status, m, CONST )
      RETURN

!  end of subroutine CUTEST_cconst_threaded_r

      END SUBROUTINE CUTEST_cconst_threaded_r

!-*-  C U T E S T    C C O N S T _ t h r e a d s a f e   S U B R O U T I N E -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 14th March 2016

      SUBROUTINE CUTEST_cconst_threadsafe_r( data, work, status, m, CONST )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: m
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: CONST

!  ---------------------------------------------------------------------
!  Compute the values of the constant terms for general constraints
!  of a function initially written in Standard Input Format (SIF)
!  ---------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: ig
      REAL :: time_in, time_out

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  compute the objective and constraint function values.

      CONST = 0.0_rp_
      IF ( data%numcon > 0 ) THEN
        DO ig = 1, data%ng
          IF ( data%KNDOFC( ig ) /= 0 ) THEN
            CONST( data%KNDOFC( ig ) ) = - data%GSCALE( ig ) * data%B( ig )
          END IF
        END DO
      END IF

      status = 0

!  update elapsed CPU time if required

      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_cconst = work%time_cconst + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_cconst_threadsafe_r

      END SUBROUTINE CUTEST_cconst_threadsafe_r

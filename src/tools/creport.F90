! THIS VERSION: CUTEST 2.3 - 2024-02018 AT 09:15 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-  C U T E S T    C R E P O R T    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Nick Gould and Philippe Toint

!  History -
!   fortran 2003 version released in CUTEst, 29th December 2012

      SUBROUTINE CUTEST_creport_r( status, CALLS, TIME )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  Dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), DIMENSION( 7 ):: CALLS
      REAL ( KIND = rp_ ), DIMENSION( 4 ):: TIME

!  -------------------------------------------------------------
!  return the values of counters maintained by the CUTEst tools. 
!  The counters are:

!    CALLS( 1 ): number of calls to the objective function
!    CALLS( 2 ): number of calls to the objective gradient
!    CALLS( 3 ): number of calls to the objective Hessian
!    CALLS( 4 ): number of Hessian times vector products
!    CALLS( 5 ): number of calls to the constraint functions
!    CALLS( 6 ): number of calls to the constraint gradients
!    CALLS( 7 ): number of calls to the constraint Hessians

!    TIME( 1 ): CPU time (in seconds) for CSETUP
!    TIME( 2 ): CPU time (in seconds) since the end of CSETUP
!    TIME( 3 ): elapsed system clock time (in seconds) for CSETUP
!    TIME( 4 ): elapsed system clock time (in seconds) since the end of CSETUP

!  Note that each constraint function is counted separately. 
!  Evaluating all the constraints thus results in work%pnc 
!  evaluations, where work%pnc is the number of constraints in 
!  the problem. Note that work%pnc does notinclude repetitions 
!  for constraints having full ranges
!  --------------------------------------------------------------

      CALL CUTEST_creport_threadsafe_r( CUTEST_data_global,                    &
                                      CUTEST_work_global( 1 ),                 &
                                      status, CALLS, TIME )
      RETURN

!  End of subroutine CUTEST_creport_r

      END SUBROUTINE CUTEST_creport_r

!-*-  C U T E S T    C R E P O R T _ t h r e a d e d   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Nick Gould and Philippe Toint

!  History -
!   fortran 2003 version released in CUTEst, 29th December 2012

      SUBROUTINE CUTEST_creport_threaded_r( status, CALLS, TIME, thread )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  Dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: thread
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), DIMENSION( 7 ):: CALLS
      REAL ( KIND = rp_ ), DIMENSION( 4 ):: TIME

!  -------------------------------------------------------------
!  return the values of counters maintained by the CUTEst tools. 
!  The counters are:

!    CALLS( 1 ): number of calls to the objective function
!    CALLS( 2 ): number of calls to the objective gradient
!    CALLS( 3 ): number of calls to the objective Hessian
!    CALLS( 4 ): number of Hessian times vector products
!    CALLS( 5 ): number of calls to the constraint functions
!    CALLS( 6 ): number of calls to the constraint gradients
!    CALLS( 7 ): number of calls to the constraint Hessians

!    TIME( 1 ): CPU time (in seconds) for CSETUP
!    TIME( 2 ): CPU time (in seconds) since the end of CSETUP
!    TIME( 3 ): elapsed system clock time (in seconds) for CSETUP
!    TIME( 4 ): elapsed system clock time (in seconds) since the end of CSETUP

!  Note that each constraint function is counted separately. 
!  Evaluating all the constraints thus results in work%pnc 
!  evaluations, where work%pnc is the number of constraints in 
!  the problem. Note that work%pnc does notinclude repetitions 
!  for constraints having full ranges
!  --------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_creport_threadsafe_r( CUTEST_data_global,                    &
                                      CUTEST_work_global( thread ),            &
                                      status, CALLS, TIME )
      RETURN

!  End of subroutine CUTEST_creport_threaded_r

      END SUBROUTINE CUTEST_creport_threaded_r

!-  C U T E S T    C R E P O R T _ t h r e a d s a f e   S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Nick Gould and Philippe Toint

!  History -
!   fortran 77 version originally released as CREPRT in CUTEr, December, 2000
!   fortran 2003 version released in CUTEst, 4th November 2012

      SUBROUTINE CUTEST_creport_threadsafe_r( data, work, status, CALLS, TIME )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      TYPE ( CUTEST_data_type ) :: data
      TYPE ( CUTEST_work_type ) :: work

!  Dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), DIMENSION( 7 ):: CALLS
      REAL ( KIND = rp_ ), DIMENSION( 4 ):: TIME

!  -------------------------------------------------------------
!  return the values of counters maintained by the CUTEst tools. 
!  The counters are:

!    CALLS( 1 ): number of calls to the objective function
!    CALLS( 2 ): number of calls to the objective gradient
!    CALLS( 3 ): number of calls to the objective Hessian
!    CALLS( 4 ): number of Hessian times vector products
!    CALLS( 5 ): number of calls to the constraint functions
!    CALLS( 6 ): number of calls to the constraint gradients
!    CALLS( 7 ): number of calls to the constraint Hessians

!    TIME( 1 ): CPU time (in seconds) for CSETUP
!    TIME( 2 ): CPU time (in seconds) since the end of CSETUP
!    TIME( 3 ): elapsed system clock time (in seconds) for CSETUP
!    TIME( 4 ): elapsed system clock time (in seconds) since the end of CSETUP

!  Note that each constraint function is counted separately. 
!  Evaluating all the constraints thus results in work%pnc 
!  evaluations, where work%pnc is the number of constraints in 
!  the problem. Note that work%pnc does notinclude repetitions 
!  for constraints having full ranges
!  --------------------------------------------------------------

!  local variable

      INTEGER ( KIND = ip_ ) :: count, count_rate
      REAL :: cpu_time_now, time_now

      CALL CPU_TIME( cpu_time_now )
      CALL SYSTEM_CLOCK( count = count, count_rate = count_rate )
      time_now = REAL( count, KIND = sp_ ) / REAL( count_rate, KIND = sp_ )

      TIME( 1 ) = data%su_cpu_time
      TIME( 2 ) = cpu_time_now - data%st_cpu_time
      TIME( 3 ) = data%su_time
      TIME( 4 ) = time_now - data%st_time

      CALLS( 1 ) = REAL( work%nc2of, rp_)
      CALLS( 2 ) = REAL( work%nc2og, rp_)
      CALLS( 3 ) = REAL( work%nc2oh, rp_)
      CALLS( 4 ) = REAL( work%nhvpr, rp_)
      IF( work%pnc > 0 ) THEN
        CALLS( 5 ) = REAL( work%nc2cf, rp_) / REAL( work%pnc, rp_)
        CALLS( 6 ) = REAL( work%nc2cg, rp_) / REAL( work%pnc, rp_)
        CALLS( 7 ) = REAL( work%nc2ch, rp_) / REAL( work%pnc, rp_)
      ELSE
        CALLS( 5 ) = REAL( work%nc2cf, rp_)
        CALLS( 6 ) = REAL( work%nc2cg, rp_)
        CALLS( 7 ) = REAL( work%nc2ch, rp_)
      ENDIF

      status = 0
      RETURN

!  End of subroutine CUTEST_creport_threadsafe_r

      END SUBROUTINE CUTEST_creport_threadsafe_r

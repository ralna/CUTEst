! THIS VERSION: CUTEST 2.2 - 2024-08-27 AT 08:00 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-  C U T E S T    T I M I N G S    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Nick Gould and Philippe Toint

!  History -
!   fortran 2003 version released in CUTEst, 25th February 2016

      SUBROUTINE CUTEST_timings_r( status, name, time )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  Dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( LEN = * ), INTENT( IN ) :: name
      REAL, INTENT( out ) :: time

!  ---------------------------------------------------------------------------
!  return the total CPU time spent in the cutest evaluation tool called 'name'
!  while the CPU monitor was turned on (see cutest_set_monitor). Timings are
!  started (or restarted) by calling with name = 'start', and may be paused
!  by calling with name = 'stop'; initially timings are switched off
!  ---------------------------------------------------------------------------

      CALL CUTEST_timings_threadsafe_r( CUTEST_data_global,                    &
                                      CUTEST_work_global( 1 ),                 &
                                      status, name, time )
      RETURN

!  End of subroutine CUTEST_timings_r

      END SUBROUTINE CUTEST_timings_r

!-*-  C U T E S T    T I M I N G S _ t h r e a d e d   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Nick Gould and Philippe Toint

!  History -
!   fortran 2003 version released in CUTEst, 25th February 2016

      SUBROUTINE CUTEST_timings_threaded_r( status, name, time, thread )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  Dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: thread
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( LEN = * ), INTENT( IN ) :: name
      REAL, INTENT( out ) :: time

!  ---------------------------------------------------------------------------
!  return the total CPU time spent in the cutest evaluation tool called 'name'
!  while the CPU monitor was turned on (see cutest_set_monitor). Timings are
!  started (or restarted) by calling with name = 'start', and may be paused
!  by calling with name = 'stop'; initially timings are switched off
!  ---------------------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_timings_threadsafe_r( CUTEST_data_global,                    &
                                      CUTEST_work_global( thread ),            &
                                      status, name, time )
      RETURN

!  End of subroutine CUTEST_timings_threaded_r

      END SUBROUTINE CUTEST_timings_threaded_r

!-  C U T E S T    T I M I N G S _ t h r e a d s a f e   S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Nick Gould and Philippe Toint

!  History -
!   fortran 2003 version released in CUTEst, 25th February 2016

      SUBROUTINE CUTEST_timings_threadsafe_r( data, work, status, name, time )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  Dummy arguments

      TYPE ( CUTEST_data_type ) :: data
      TYPE ( CUTEST_work_type ) :: work
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( LEN = * ), INTENT( IN ) :: name
      REAL, INTENT( out ) :: time

!  ---------------------------------------------------------------------------
!  return the total CPU time spent in the cutest evaluation tool called 'name'
!  while the CPU monitor was turned on (see cutest_set_monitor). Timings are
!  started (or restarted) by calling with name = 'start', and may be paused
!  by calling with name = 'stop'; initially timings are switched off
!  ---------------------------------------------------------------------------

      status = 0
      SELECT CASE ( name )
      CASE ( 'start' )
        work%record_times = .TRUE.
        time = 0.0
      CASE ( 'stop' )
        work%record_times = .FALSE.
        time = 0.0
      CASE ( 'cutest_ccf' )
        time = work%time_ccf
      CASE ( 'cutest_ccfg' )
        time = work%time_ccfg
      CASE ( 'cutest_ccfsg' )
        time = work%time_ccfsg
      CASE ( 'cutest_cch' )
        time = work%time_cch
      CASE ( 'cutest_cohprodsp' )
        time = work%time_cohprodsp
      CASE ( 'cutest_cohprods' )
        time = work%time_cohprods
      CASE ( 'cutest_cchprodsp' )
        time = work%time_cchprodsp
      CASE ( 'cutest_cchprods' )
        time = work%time_cchprods
      CASE ( 'cutest_ccifg' )
        time = work%time_ccifg
      CASE ( 'cutest_ccifsg' )
        time = work%time_ccifsg
      CASE ( 'cutest_cdh' )
        time = work%time_cdh
      CASE ( 'cutest_cdhc' )
        time = work%time_cdhc
      CASE ( 'cutest_cdimchp' )
        time = work%time_cdimchp
      CASE ( 'cutest_ceh' )
        time = work%time_ceh
      CASE ( 'cutest_cfn' )
        time = work%time_cfn
      CASE ( 'cutest_cgr' )
        time = work%time_cgr
      CASE ( 'cutest_cgrdh' )
        time = work%time_cgrdh
      CASE ( 'cutest_chcprod' )
        time = work%time_chcprod
      CASE ( 'cutest_chprod' )
        time = work%time_chprod
      CASE ( 'cutest_chjprod' )
        time = work%time_chjprod
      CASE ( 'cutest_cifn' )
        time = work%time_cifn
      CASE ( 'cutest_cigr' )
        time = work%time_cigr
      CASE ( 'cutest_cisgr' )
        time = work%time_cisgr
      CASE ( 'cutest_cisgrp' )
        time = work%time_cisgrp
      CASE ( 'cutest_cidh' )
        time = work%time_cidh
      CASE ( 'cutest_cish' )
        time = work%time_cish
      CASE ( 'cutest_cjprod' )
        time = work%time_cjprod
      CASE ( 'cutest_clfg' )
        time = work%time_clfg
      CASE ( 'cutest_cofg' )
        time = work%time_cofg
      CASE ( 'cutest_cofsg' )
        time = work%time_cofsg
      CASE ( 'cutest_csgr' )
        time = work%time_csgr
      CASE ( 'cutest_csgreh' )
        time = work%time_csgreh
      CASE ( 'cutest_csgrsh' )
        time = work%time_csgrsh
      CASE ( 'cutest_csh' )
        time = work%time_csh
      CASE ( 'cutest_cshc' )
        time = work%time_cshc
      CASE ( 'cutest_cshj' )
        time = work%time_cshj
      CASE ( 'cutest_cshcprod' )
        time = work%time_cshcprod
      CASE ( 'cutest_cshp' )
        time = work%time_cshp
      CASE ( 'cutest_cshprod' )
        time = work%time_cshprod
      CASE ( 'cutest_csjprod' )
        time = work%time_csjprod
      CASE ( 'cutest_cconst' )
        time = work%time_cconst
      CASE ( 'cutest_ubandh' )
        time = work%time_ubandh
      CASE ( 'cutest_udh' )
        time = work%time_udh
      CASE ( 'cutest_ueh' )
        time = work%time_ueh
      CASE ( 'cutest_ufn' )
        time = work%time_ufn
      CASE ( 'cutest_ugr' )
        time = work%time_ugr
      CASE ( 'cutest_ugrdh' )
        time = work%time_ugrdh
      CASE ( 'cutest_ugreh' )
        time = work%time_ugreh
      CASE ( 'cutest_ugrsh' )
        time = work%time_ugrsh
      CASE ( 'cutest_uhprod' )
        time = work%time_uhprod
      CASE ( 'cutest_uofg' )
        time = work%time_uofg
      CASE ( 'cutest_ush' )
        time = work%time_ush
      CASE ( 'cutest_ushp' )
        time = work%time_ushp
      CASE ( 'cutest_ushprod' )
        time = work%time_ushprod
      CASE DEFAULT
        status = 26
        time = 0.0
        IF ( data%out > 0 ) WRITE( data%out,                                   &
          "( ' ** SUBROUTINE TIMINGS: unknown evaluation function ', A )" )    &
            TRIM ( name )
      END SELECT

      RETURN

!  End of subroutine CUTEST_timings_threadsafe_r

      END SUBROUTINE CUTEST_timings_threadsafe_r

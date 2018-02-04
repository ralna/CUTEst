! THIS VERSION: CUTEST 1.5 - 10/10/2016 AT 15:00 GMT.

!-*-*-*-*-*-*-  C U T E S T    T R E P O R T    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Nick Gould and Philippe Toint

!  History -
!   fortran 2003 version released in CUTEst, 25th February 2016

      SUBROUTINE CUTEST_timings( status, name, time )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  Dummy arguments

      INTEGER, INTENT( OUT ) :: status
      CHARACTER ( LEN = * ), INTENT( IN ) :: name
      REAL ( KIND = wp ), INTENT( out ) :: time

!  ---------------------------------------------------------------------------
!  return the total CPU time spent in the cutest evaluation tool called 'name'
!  while the CPU monitor was turned on (see cutest_set_monitor). Timings are
!  started (or restarted) by calling with name = 'start', and may be paused
!  by calling with name = 'stop'; initially timings are switched off
!  ---------------------------------------------------------------------------

      CALL CUTEST_timings_threadsafe( CUTEST_data_global,                      &
                                      CUTEST_work_global( 1 ),                 &
                                      status, name, time )
      RETURN

!  End of subroutine CUTEST_timings

      END SUBROUTINE CUTEST_timings

!-*-  C U T E S T    T R E P O R T _ t h r e a d e d   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Nick Gould and Philippe Toint

!  History -
!   fortran 2003 version released in CUTEst, 25th February 2016

      SUBROUTINE CUTEST_timings_threaded( status, name, time, thread )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  Dummy arguments

      INTEGER, INTENT( IN ) :: thread
      INTEGER, INTENT( OUT ) :: status
      CHARACTER ( LEN = * ), INTENT( IN ) :: name
      REAL ( KIND = wp ), INTENT( out ) :: time

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

      CALL CUTEST_timings_threadsafe( CUTEST_data_global,                      &
                                      CUTEST_work_global( thread ),            &
                                      status, name, time )
      RETURN

!  End of subroutine CUTEST_timings_threaded

      END SUBROUTINE CUTEST_timings_threaded

!-  C U T E S T    T R E P O R T _ t h r e a d s a f e   S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Nick Gould and Philippe Toint

!  History -
!   fortran 2003 version released in CUTEst, 25th February 2016

      SUBROUTINE CUTEST_timings_threadsafe( data, work, status, name, time )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  Dummy arguments

      TYPE ( CUTEST_data_type ) :: data
      TYPE ( CUTEST_work_type ) :: work
      INTEGER, INTENT( OUT ) :: status
      CHARACTER ( LEN = * ), INTENT( IN ) :: name
      REAL ( KIND = wp ), INTENT( out ) :: time

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
        time = 0.0_wp
      CASE ( 'stop' )
        work%record_times = .FALSE.
        time = 0.0_wp
      CASE ( 'cutest_ccfg' )
        time = work%time_ccfg
      CASE ( 'cutest_ccfsg' )
        time = work%time_ccfsg
      CASE ( 'cutest_cch' )
        time = work%time_cch
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
      CASE ( 'cutest_cifn' )
        time = work%time_cifn
      CASE ( 'cutest_cigr' )
        time = work%time_cigr
      CASE ( 'cutest_cisgr' )
        time = work%time_cisgr
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
        time = 0.0_wp
        IF ( data%out > 0 ) WRITE( data%out,                                   &
          "( ' ** SUBROUTINE TIMINGS: unknown evaluation function ', A )" )    &
            TRIM ( name )
      END SELECT

      RETURN

!  End of subroutine CUTEST_timings_threadsafe

      END SUBROUTINE CUTEST_timings_threadsafe

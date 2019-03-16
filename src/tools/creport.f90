! THIS VERSION: CUTEST 1.0 - 29/12/2012 AT 16:40 GMT.

!-*-*-*-*-*-*-  C U T E S T    C R E P O R T    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Nick Gould and Philippe Toint

!  History -
!   fortran 2003 version released in CUTEst, 29th December 2012

      SUBROUTINE CUTEST_creport( status, CALLS, TIME )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  Dummy arguments

      INTEGER, INTENT( OUT ) :: status
      REAL ( KIND = wp ), DIMENSION( 7 ):: CALLS
      REAL ( KIND = wp ), DIMENSION( 2 ):: TIME

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

!  Note that each constraint function is counted separately. 
!  Evaluating all the constraints thus results in work%pnc 
!  evaluations, where work%pnc is the number of constraints in 
!  the problem. Note that work%pnc does notinclude repetitions 
!  for constraints having full ranges
!  --------------------------------------------------------------

      CALL CUTEST_creport_threadsafe( CUTEST_data_global,                      &
                                      CUTEST_work_global( 1 ),                 &
                                      status, CALLS, TIME )
      RETURN

!  End of subroutine CUTEST_creport

      END SUBROUTINE CUTEST_creport

!-*-  C U T E S T    C R E P O R T _ t h r e a d e d   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Nick Gould and Philippe Toint

!  History -
!   fortran 2003 version released in CUTEst, 29th December 2012

      SUBROUTINE CUTEST_creport_threaded( status, CALLS, TIME, thread )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  Dummy arguments

      INTEGER, INTENT( IN ) :: thread
      INTEGER, INTENT( OUT ) :: status
      REAL ( KIND = wp ), DIMENSION( 7 ):: CALLS
      REAL ( KIND = wp ), DIMENSION( 2 ):: TIME

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

      CALL CUTEST_creport_threadsafe( CUTEST_data_global,                      &
                                      CUTEST_work_global( thread ),            &
                                      status, CALLS, TIME )
      RETURN

!  End of subroutine CUTEST_creport_threaded

      END SUBROUTINE CUTEST_creport_threaded

!-  C U T E S T    C R E P O R T _ t h r e a d s a f e   S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Nick Gould and Philippe Toint

!  History -
!   fortran 77 version originally released as CREPRT in CUTEr, December, 2000
!   fortran 2003 version released in CUTEst, 4th November 2012

      SUBROUTINE CUTEST_creport_threadsafe( data, work, status, CALLS, TIME )
      USE CUTEST
      TYPE ( CUTEST_data_type ) :: data
      TYPE ( CUTEST_work_type ) :: work
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  Dummy arguments

      INTEGER, INTENT( OUT ) :: status
      REAL ( KIND = wp ), DIMENSION( 7 ):: CALLS
      REAL ( KIND = wp ), DIMENSION( 2 ):: TIME

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

!  Note that each constraint function is counted separately. 
!  Evaluating all the constraints thus results in work%pnc 
!  evaluations, where work%pnc is the number of constraints in 
!  the problem. Note that work%pnc does notinclude repetitions 
!  for constraints having full ranges
!  --------------------------------------------------------------

!  local variable

      REAL :: time_now

      CALL CPU_TIME( time_now )

      TIME( 1 ) = data%sutime
      TIME( 2 ) = time_now - data%sttime

      CALLS( 1 ) = work%nc2of
      CALLS( 2 ) = work%nc2og
      CALLS( 3 ) = work%nc2oh
      CALLS( 4 ) = work%nhvpr
      IF( work%pnc > 0 ) THEN
        CALLS( 5 ) = work%nc2cf / work%pnc
        CALLS( 6 ) = work%nc2cg / work%pnc
        CALLS( 7 ) = work%nc2ch / work%pnc
      ELSE
        CALLS( 5 ) = work%nc2cf
        CALLS( 6 ) = work%nc2cg
        CALLS( 7 ) = work%nc2ch
      ENDIF

      status = 0
      RETURN

!  End of subroutine CUTEST_creport_threadsafe

      END SUBROUTINE CUTEST_creport_threadsafe

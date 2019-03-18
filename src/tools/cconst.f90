! THIS VERSION: CUTEST 1.4 - 14/03/2016 AT 11:50 GMT.

!-*-*-*-*-*-*-  C U T E S T    C C O N S T    S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 14th March 2016

      SUBROUTINE CUTEST_cconst( status, m, CONST )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER, INTENT( IN ) :: m
      INTEGER, INTENT( OUT ) :: status
      REAL ( KIND = wp ), INTENT( OUT ), DIMENSION( m ) :: CONST

!  ---------------------------------------------------------------------
!  Compute the values of the constant terms for general constraints
!  of a function initially written in Standard Input Format (SIF)
!  ---------------------------------------------------------------------

      CALL CUTEST_cconst_threadsafe( CUTEST_data_global,                       &
                                     CUTEST_work_global( 1 ),                  &
                                     status, m, CONST )
      RETURN

!  end of subroutine CUTEST_cconst

      END SUBROUTINE CUTEST_cconst

!-*-*-  C U T E S T    C C O N S T _ t h r e a d e d   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 14th March 2016

      SUBROUTINE CUTEST_cconst_threaded( status, m, CONST, thread )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER, INTENT( IN ) :: m, thread
      INTEGER, INTENT( OUT ) :: status
      REAL ( KIND = wp ), INTENT( OUT ), DIMENSION( m ) :: CONST

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

      CALL CUTEST_cconst_threadsafe( CUTEST_data_global,                       &
                                     CUTEST_work_global( thread ),             &
                                     status, m, CONST )
      RETURN

!  end of subroutine CUTEST_cconst_threaded

      END SUBROUTINE CUTEST_cconst_threaded

!-*-  C U T E S T    C C O N S T _ t h r e a d s a f e   S U B R O U T I N E -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 14th March 2016

      SUBROUTINE CUTEST_cconst_threadsafe( data, work, status, m, CONST )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER, INTENT( IN ) :: m
      INTEGER, INTENT( OUT ) :: status
      REAL ( KIND = wp ), INTENT( OUT ), DIMENSION( m ) :: CONST

!  ---------------------------------------------------------------------
!  Compute the values of the constant terms for general constraints
!  of a function initially written in Standard Input Format (SIF)
!  ---------------------------------------------------------------------

!  local variables

      INTEGER :: ig
      REAL :: time_in, time_out

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  compute the objective and constraint function values.

      CONST = 0.0_wp
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

!  end of subroutine CUTEST_cconst_threadsafe

      END SUBROUTINE CUTEST_cconst_threadsafe

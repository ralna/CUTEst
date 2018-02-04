! THIS VERSION: CUTEST 1.4 - 26/02/2016 AT 08:00 GMT.

!-*-*-*-*-*-*-*-  C U T E S T    C F N    S U B R O U T I N E  -*-*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th December 2012

      SUBROUTINE CUTEST_cfn( status, n, m, X, f, C )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER, INTENT( IN ) :: n, m
      INTEGER, INTENT( OUT ) :: status
      REAL ( KIND = wp ), INTENT( OUT ) :: f
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = wp ), INTENT( OUT ), DIMENSION( m ) :: C

!  ---------------------------------------------------------------------
!  Compute the values of the objective function and general constraints
!  of a function initially written in Standard Input Format (SIF)
!  ---------------------------------------------------------------------

      CALL CUTEST_cfn_threadsafe( CUTEST_data_global,                          &
                                  CUTEST_work_global( 1 ),                     &
                                  status, n, m, X, f, C )
      RETURN

!  end of subroutine CUTEST_cfn

      END SUBROUTINE CUTEST_cfn

!-*-*-*-  C U T E S T    C F N _ t h r e a d e d   S U B R O U T I N E  -*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th December 2012

      SUBROUTINE CUTEST_cfn_threaded( status, n, m, X, f, C, thread )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER, INTENT( IN ) :: n, m, thread
      INTEGER, INTENT( OUT ) :: status
      REAL ( KIND = wp ), INTENT( OUT ) :: f
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = wp ), INTENT( OUT ), DIMENSION( m ) :: C

!  ---------------------------------------------------------------------
!  Compute the values of the objective function and general constraints
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

      CALL CUTEST_cfn_threadsafe( CUTEST_data_global,                          &
                                  CUTEST_work_global( thread ),                &
                                  status, n, m, X, f, C )
      RETURN

!  end of subroutine CUTEST_cfn_threaded

      END SUBROUTINE CUTEST_cfn_threaded

!-*-*-  C U T E S T    C F N _ t h r e a d s a f e   S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTE, October 1991
!   fortran 2003 version released in CUTEst, 20th November 2012

      SUBROUTINE CUTEST_cfn_threadsafe( data, work, status, n, m, X, f, C )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER, INTENT( IN ) :: n, m
      INTEGER, INTENT( OUT ) :: status
      REAL ( KIND = wp ), INTENT( OUT ) :: f
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = wp ), INTENT( OUT ), DIMENSION( m ) :: C

!  ---------------------------------------------------------------------
!  Compute the values of the objective function and general constraints
!  of a function initially written in Standard Input Format (SIF)
!  ---------------------------------------------------------------------

!  local variables

      INTEGER :: i, j, ig, ifstat, igstat
      REAL ( KIND = wp ) :: ftt
      REAL ( KIND = wp ) :: time_in, time_out

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  there are non-trivial group functions

      DO i = 1, MAX( data%nel, data%ng )
        work%ICALCF( i ) = i
      END DO

!  evaluate the element function values

      CALL ELFUN( work%FUVALS, X, data%EPVALU, data%nel, data%ITYPEE,          &
                  data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,          &
                  data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,           &
                  data%lelvar, data%lntvar, data%lstadh, data%lstep,           &
                  data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,          &
                  1, ifstat )
      IF ( ifstat /= 0 ) GO TO 930

!  compute the group argument values ft

      DO ig = 1, data%ng
        ftt = - data%B( ig )

!  include the contribution from the linear element

        DO j = data%ISTADA( ig ), data%ISTADA( ig + 1 ) - 1
          ftt = ftt + data%A( j ) * X( data%ICNA( j ) )
        END DO

!  include the contributions from the nonlinear elements

        DO j = data%ISTADG( ig ), data%ISTADG( ig + 1 ) - 1
          ftt = ftt + data%ESCALE( j ) * work%FUVALS( data%IELING( j ) )
        END DO
        work%FT( ig ) = ftt
      END DO

!  compute the group function values

!  all group functions are trivial

      IF ( data%altriv ) THEN
        work%GVALS( : data%ng, 1 ) = work%FT( : data%ng )
        work%GVALS( : data%ng, 2 ) = 1.0_wp

!  evaluate the group function values

      ELSE
        CALL GROUP( work%GVALS, data%ng, work%FT, data%GPVALU, data%ng,        &
                    data%ITYPEG, data%ISTGP, work%ICALCF, data%ltypeg,         &
                    data%lstgp, data%lcalcf, data%lcalcg, data%lgpvlu,         &
                    .FALSE., igstat )
        IF ( igstat /= 0 ) GO TO 930
      END IF

!  compute the objective and constraint function values.

      f = 0.0_wp
      IF ( data%numcon > 0 ) THEN
        DO ig = 1, data%ng
          IF ( data%KNDOFC( ig ) == 0 ) THEN
            IF ( data%GXEQX( ig ) ) THEN
              f = f + data%GSCALE( ig ) * work%FT( ig )
            ELSE
              f = f + data%GSCALE( ig ) * work%GVALS( ig, 1 )
            END IF
          ELSE
            IF ( data%GXEQX( ig ) ) THEN
              C( data%KNDOFC( ig ) ) = data%GSCALE( ig ) * work%FT( ig )
            ELSE
               C( data%KNDOFC( ig ) ) = data%GSCALE( ig ) * work%GVALS( ig, 1 )
            END IF
          END IF
        END DO

!  there are no constraints, so we need not check KNDOFC

      ELSE
        DO ig = 1, data%ng
          IF ( data%GXEQX( ig ) ) THEN
            f = f + data%GSCALE( ig ) * work%FT( ig )
          ELSE
            f = f + data%GSCALE( ig ) * work%GVALS( ig, 1 )
          END IF
        END DO
      END IF

!  Update the counters for the report tool.

      work%nc2of = work%nc2of + 1
      work%nc2cf = work%nc2cf + work%pnc
      status = 0
      GO TO 990

!  unsuccessful returns

  930 CONTINUE
      IF ( data%out > 0 ) WRITE( data%out,                                     &
        "( ' ** SUBROUTINE CFN: error flag raised during SIF evaluation' )" )
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_cfn = work%time_cfn + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_cfn_threadsafe

      END SUBROUTINE CUTEST_cfn_threadsafe

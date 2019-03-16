! THIS VERSION: CUTEST 1.5 - 13/10/2016 AT 13:00 GMT.

!-*-*-*-*-*-*-  C U T E S T    C I F N    S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Ingrid Bongartz and Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 13th October 2016

      SUBROUTINE CUTEST_cifn( status, n, iprob, X, fn )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER, INTENT( IN ) :: n, iprob
      INTEGER, INTENT( OUT ) :: status
      REAL ( KIND = wp ), INTENT( OUT ) :: fn
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( n ) :: X

!  -------------------------------------------------------------------
!  compute the value of a specified problem function (iprob = 0 is the
!  objective function, while iprob > 0 is the iprob-th constraint) of
!  a problem initially written in Standard Input Format (SIF).
!  -------------------------------------------------------------------

      CALL CUTEST_cifn_threadsafe( CUTEST_data_global,                         &
                                   CUTEST_work_global( 1 ),                    &
                                   status, n, iprob, X, fn )
      RETURN

!  end of subroutine CUTEST_cifn

      END SUBROUTINE CUTEST_cifn

!-*-*-  C U T E S T    C I F N _ t h r e a d e d   S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Ingrid Bongartz and Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 13th October 2016

      SUBROUTINE CUTEST_cifn_threaded( status, n, iprob, X, fn, thread )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER, INTENT( IN ) :: n, iprob, thread
      INTEGER, INTENT( OUT ) :: status
      REAL ( KIND = wp ), INTENT( OUT ) :: fn
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( n ) :: X

!  -------------------------------------------------------------------
!  compute the value of a specified problem function (iprob = 0 is the
!  objective function, while iprob > 0 is the iprob-th constraint) of
!  a problem initially written in Standard Input Format (SIF).
!  -------------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_cifn_threadsafe( CUTEST_data_global,                         &
                                   CUTEST_work_global( thread ),               &
                                   status, n, iprob, X, fn )
      RETURN

!  end of subroutine CUTEST_cifn_threaded

      END SUBROUTINE CUTEST_cifn_threaded

!-*-  C U T E S T    C I F N _ t h r e a d s a f e   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Ingrid Bongartz and Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 13th October 2016

      SUBROUTINE CUTEST_cifn_threadsafe( data, work,                          &
                                         status, n, iprob, X, fn )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER, INTENT( IN ) :: n, iprob
      INTEGER, INTENT( OUT ) :: status
      REAL ( KIND = wp ), INTENT( OUT ) :: fn
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( n ) :: X

!  -------------------------------------------------------------------
!  compute the value of a specified problem function (iprob = 0 is the
!  objective function, while iprob > 0 is the iprob-th constraint) of
!  a problem initially written in Standard Input Format (SIF).
!  -------------------------------------------------------------------

!  local variables

      INTEGER :: i, j, iel, ig, ii, ncalcg, neling, ifstat, igstat
      REAL ( KIND = wp ) :: ftt
      REAL :: time_in, time_out
      INTEGER, DIMENSION( 1 ) :: ICALCG
      EXTERNAL :: RANGE

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  check input parameters

      IF ( iprob < 0 ) THEN
        IF ( data%out > 0 ) WRITE( data%out, "( ' ** SUBROUTINE CIFN: ',       &
       &    'invalid constraint index iprob ' )" )
        status = 2 ; GO TO 990
      END IF

!  constraint function required

      IF ( iprob > 0 ) THEN

!  find group index ig of constraint iprob

        ig = 0
        DO i = 1, data%ng
          IF ( data%KNDOFC( i ) == iprob ) THEN
            ig = i
            EXIT
          END IF
        END DO
        IF ( ig == 0 ) THEN
          IF ( data%out > 0 ) WRITE( data%out, "( ' ** SUBROUTINE CIFN: ',     &
         &    'invalid constraint index iprob ' )" )
          status = 2 ; GO TO 990
        END IF

!  determine nonlinear elements in group ig. Record their indices in ICALCF

        neling = data%ISTADG( ig + 1 ) - data%ISTADG( ig )
        j = data%ISTADG( ig ) - 1
        DO i = 1, neling
          j = j + 1
          work%ICALCF( i ) = data%IELING( j )
        END DO

!  objective function required

      ELSE

!  identify which elements are included in objective function. Use LOGIC
!  to keep track of elements already included

        work%LOGIC( : data%nel ) = .FALSE.

!  now identify elements in objective function groups

        neling = 0
        DO ig = 1, data%ng
          IF ( data%KNDOFC( ig ) == 0 ) THEN
            DO ii = data%ISTADG( ig ), data%ISTADG( ig + 1 ) - 1
              iel = data%IELING( ii )
              IF ( .NOT. work%LOGIC( iel ) ) THEN
                work%LOGIC( iel ) = .TRUE.
                neling = neling + 1
                work%ICALCF( neling ) = iel
              END IF
            END DO
          END IF
        END DO
      END IF

!  evaluate the element functions

      CALL ELFUN( work%FUVALS, X, data%EPVALU, neling, data%ITYPEE,            &
                  data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,          &
                  data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,           &
                  data%lelvar, data%lntvar, data%lstadh, data%lstep,           &
                  data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,          &
                  1, ifstat )
      IF ( ifstat /= 0 ) GO TO 930

!  constraint function required

      IF ( iprob > 0 ) THEN

!  compute the group argument value FTT. Consider only the group associated
!  with constraint iprob

        ftt = - data%B( ig )

!  include the contribution from the linear element only if the variable
!  belongs to the first n variables

        DO i = data%ISTADA( ig ), data%ISTADA( ig + 1 ) - 1
          j = data%ICNA( i )
          IF ( j <= n ) ftt = ftt + data%A( i ) * X( j )
        END DO

!  include the contributions from the nonlinear elements

        DO i = data%ISTADG( ig ), data%ISTADG( ig + 1 ) - 1
           ftt = ftt + data%ESCALE( i ) * work%FUVALS( data%IELING( i ) )
        END DO
        work%FT( ig ) = ftt

!  if ig is a trivial group, record the function value and derivative

        IF ( data%GXEQX( ig ) ) THEN
          work%GVALS( ig, 1 ) = work%FT( ig )
          work%GVALS( ig, 2 ) = 1.0_wp

!  otherwise, evaluate group ig

        ELSE
          ICALCG( 1 ) = ig
          CALL GROUP( work%GVALS, data%ng, work%FT, data%GPVALU, 1,            &
                      data%ITYPEG, data%ISTGP, ICALCG, data%ltypeg,            &
                      data%lstgp, 1, data%lcalcg, data%lgpvlu,                 &
                      .FALSE., igstat )
          IF ( igstat /= 0 ) GO TO 930
        END IF

!  compute the constraint function value

        IF ( data%GXEQX( ig ) ) THEN
          fn = data%GSCALE( ig ) * work%FT( ig )
        ELSE
          fn = data%GSCALE( ig ) * work%GVALS( ig, 1 )

!  Update the constraint function evaluation counter

          work%nc2cf = work%nc2cf + 1
        END IF

!  objective function required

      ELSE

!  compute the list of groups involved in the required problem function

        ncalcg = 0
        DO ig = 1, data%ng
          IF ( data%KNDOFC( ig ) == 0 ) THEN
            ncalcg = ncalcg + 1
            work%ICALCF( ncalcg ) = ig

!  compute the group argument values ft

            ftt = - data%B( ig )

!  include the contribution from the linear element

            DO j = data%ISTADA( ig ), data%ISTADA( ig + 1 ) - 1
               ftt = ftt + data%A( j ) * X( data%ICNA( j ) )
            END DO

!  include the contributions from the nonlinear elements

            DO j = data%ISTADG( ig ), data%ISTADG( ig + 1 ) - 1
               ftt = ftt + data%ESCALE( j ) *  &
                      work%FUVALS( data%IELING( j ) )
            END DO
            work%FT( ig ) = ftt

!  record the derivatives of trivial groups

            IF ( data%GXEQX( ig ) ) work%GVALS( ig, 2 ) = 1.0_wp

!  if this is the first ever evaluation, initialize GVALS

          ELSE
            IF ( work%firstg ) work%GVALS( ig, 2 ) = 1.0_wp
          END IF
        END DO

!  compute the group function values

!  all group functions are trivial

        IF ( data%altriv ) THEN
          work%GVALS( : data%ng, 1 ) = work%FT( : data%ng )
          work%GVALS( : data%ng, 2 ) = 1.0_wp

!  evaluate the group function values

        ELSE
          CALL GROUP( work%GVALS, data%ng, work%FT, data%GPVALU, ncalcg,       &
                      data%ITYPEG, data%ISTGP, work%ICALCF, data%ltypeg,       &
                      data%lstgp, data%lcalcf, data%lcalcg, data%lgpvlu,       &
                      .FALSE., igstat )
          IF ( igstat /= 0 ) GO TO 930
        END IF

!  compute the objective and constraint function values.

        fn = 0.0_wp
        DO ig = 1, data%ng
          IF ( data%KNDOFC( ig ) == 0 ) THEN
            IF ( data%GXEQX( ig ) ) THEN
              fn = fn + data%GSCALE( ig ) * work%FT( ig )
            ELSE
              fn = fn + data%GSCALE( ig ) * work%GVALS( ig, 1 )
            END IF
          END IF
        END DO

!  update the objective function evaluation counter

        IF ( .NOT. data%altriv ) work%nc2of = work%nc2of + 1
      END IF

      status = 0
      GO TO 990

!  unsuccessful returns

  930 CONTINUE
      IF ( data%out > 0 ) WRITE( data%out,                                     &
        "( ' ** SUBROUTINE CIFN: error flag raised during SIF evaluation' )" )
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_cifn = work%time_cifn + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_cifn_threadsafe

      END SUBROUTINE CUTEST_cifn_threadsafe

! THIS VERSION: CUTEST 2.3 - 2024-10-23 AT 10:00 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-*-  C U T E S T    C C F    S U B R O U T I N E  -*-*-*-*-*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   first released in CUTEst, 27th August 2024

      SUBROUTINE CUTEST_ccf_r( status, n, m, X, C )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: C

!  --------------------------------------------------------------
!  Compute the values of the constraint functions for constraints 
!  initially written in Standard Input Format (SIF)
!  --------------------------------------------------------------

      CALL CUTEST_ccf_threadsafe_r( CUTEST_data_global,                        &
                                    CUTEST_work_global( 1 ),                   &
                                    status, n, m, X, C )
      RETURN

!  end of subroutine CUTEST_cscfg

      END SUBROUTINE CUTEST_ccf_r

!-*-*-  C U T E S T    C C F _ t h r e a d e d   S U B R O U T I N E  -*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   first released in CUTEst, 27th August 2024

      SUBROUTINE CUTEST_ccf_threaded_r( status, n, m, X, C, thread )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, thread
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: C

!  --------------------------------------------------------------
!  Compute the values of the constraint functions for constraints 
!  initially written in Standard Input Format (SIF)
!  --------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_ccf_threadsafe_r( CUTEST_data_global,                        &
                                    CUTEST_work_global( thread ),              &
                                    status, n, m, X, C )
      RETURN

!  end of subroutine CUTEST_cscfg_threaded

      END SUBROUTINE CUTEST_ccf_threaded_r

!-*-*-  C U T E S T    C C F _ t h r e a d s a f e   S U B R O U T I N E  -*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal authors: Nick Gould

!  History -
!   first released in CUTEst, 27th August 2024

      SUBROUTINE CUTEST_ccf_threadsafe_r( data, work, status, n, m, X, C )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: C

!  --------------------------------------------------------------
!  Compute the values of the constraint functions for constraints 
!  initially written in Standard Input Format (SIF)
!  --------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, j, iel, ig, ii, icon
      INTEGER ( KIND = ip_ ) :: ifstat, igstat, icnt, nelow, nelup
      REAL ( KIND = rp_ ) :: ftt
      REAL :: time_in, time_out

      IF ( work%record_times ) CALL CPU_TIME( time_in )

      IF ( data%numcon == 0 ) GO TO 990

!  identify which elements are included in constraints. Use logical work
!  vector to keep track of elements already included

      work%LOGIC( : data%nel ) = .FALSE.

!  now identify elements in first m constraint groups

      icnt = 0
      DO ig = 1, data%ng
        icon = data%KNDOFC( ig )
        IF ( icon > 0 .AND. icon <= m ) THEN
          nelow = data%ISTADG( ig )
          nelup = data%ISTADG( ig + 1 ) - 1
          DO ii = nelow, nelup
            iel = data%IELING( ii )
            IF ( .NOT. work%LOGIC( iel ) ) THEN
              work%LOGIC( iel ) = .TRUE.
              icnt = icnt + 1
              work%ICALCF( icnt ) = iel
            END IF
          END DO
        END IF
      END DO

!  evaluate the element function values

      CALL ELFUN_r( work%FUVALS, X, data%EPVALU, icnt, data%ITYPEE,            &
                    data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,        &
                    data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,         &
                    data%lelvar, data%lntvar, data%lstadh, data%lstep,         &
                    data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,        &
                    1, ifstat )
      IF ( ifstat /= 0 ) GO TO 930

!  compute the group argument values ft

      DO ig = 1, data%ng
        ftt = 0.0_rp_

!  Consider only those groups in the constraints

        icon = data%KNDOFC( ig )
        IF ( icon > 0 .AND. icon <= m ) THEN
          ftt = - data%B( ig )

!  Include contributions from the linear element only if the variable belongs
!  to the first n variables

          DO i = data%ISTADA( ig ), data%ISTADA( ig + 1 ) - 1
            j = data%ICNA( i )
            IF ( j <= n ) ftt = ftt + data%A( i ) * X( j )
          END DO

!  include the contributions from the nonlinear elements

          DO i = data%ISTADG( ig ), data%ISTADG( ig + 1 ) - 1
            ftt = ftt + data%ESCALE( i ) * work%FUVALS( data%IELING( i ) )
          END DO

!  record derivatives of trivial groups

          IF ( data%GXEQX( ig ) ) work%GVALS( ig, 2 ) = 1.0_rp_
        END IF
        work%FT( ig ) = ftt
      END DO

!  compute the group function values

!  all group functions are trivial

      IF ( data%altriv ) THEN
        work%GVALS( : data%ng, 1 ) = work%FT( : data%ng )
        work%GVALS( : data%ng, 2 ) = 1.0_rp_
      ELSE

!  evaluate the group function values. Evaluate groups belonging to the first
!  m constraints only

        icnt = 0
        DO ig = 1, data%ng
          icon = data%KNDOFC( ig )
          IF ( icon > 0 .AND. icon <= m ) THEN
            icnt = icnt + 1
            work%ICALCF( icnt ) = ig
          END IF
        END DO
        CALL GROUP_r( work%GVALS, data%ng, work%FT, data%GPVALU, icnt,         &
                      data%ITYPEG, data%ISTGP, work%ICALCF, data%ltypeg,       &
                      data%lstgp, data%lcalcf, data%lcalcg, data%lgpvlu,       &
                      .FALSE., igstat )
        IF ( igstat /= 0 ) GO TO 930
      END IF

!  compute the constraint function values

      DO ig = 1, data%ng
        i = data%KNDOFC( ig )
        IF ( i > 0 .AND. i <= m ) THEN
          IF ( data%GXEQX( ig ) ) THEN
            C( i ) = data%GSCALE( ig ) * work%FT( ig )
          ELSE
            C( i ) = data%GSCALE( ig ) * work%GVALS( ig, 1 )
          END IF
        END IF
      END DO

!  Update the counters for the report tool.

      work%nc2cf = work%nc2cf + work%pnc
      status = 0
      GO TO 990

!  unsuccessful returns

  930 CONTINUE
      IF ( data%out > 0 ) WRITE( data%out,                                     &
        "( ' ** SUBROUTINE CCF: error flag raised during SIF evaluation' )" )
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_ccf = work%time_ccf + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_cscfg_threadsafe_r

      END SUBROUTINE CUTEST_ccf_threadsafe_r


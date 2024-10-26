! THIS VERSION: CUTEST 2.3 - 2024-10-23 AT 10:00 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-*-  C U T E S T    U F N    S U B R O U T I N E  -*-*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 22nd December 2012

      SUBROUTINE CUTEST_ufn_r( status, n, X, f )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( OUT ) :: f
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X

!  ----------------------------------------------------------
!  Compute the value of a groups partially separable function
!  initially written in Standard Input Format (SIF)
!  ----------------------------------------------------------

      CALL CUTEST_ufn_threadsafe_r( CUTEST_data_global,                        &
                                    CUTEST_work_global( 1 ),                   &
                                    status, n, X, f )
      RETURN

!  end of subroutine CUTEST_ufn_r

      END SUBROUTINE CUTEST_ufn_r

!-*-*-*-  C U T E S T    U F N _ t h r e a d e d   S U B R O U T I N E  -*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 22nd December 2012

      SUBROUTINE CUTEST_ufn_threaded_r( status, n, X, f, thread )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, thread
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( OUT ) :: f
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X

!  ----------------------------------------------------------
!  Compute the value of a groups partially separable function
!  initially written in Standard Input Format (SIF)
!  ----------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_ufn_threadsafe_r( CUTEST_data_global,                        &
                                    CUTEST_work_global( thread ),              &
                                    status, n, X, f )
      RETURN

!  end of subroutine CUTEST_ufn_threaded_r

      END SUBROUTINE CUTEST_ufn_threaded_r

!-*-*-  C U T E S T    U F N _ t h r e a d s a f e  S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTE, July 1991
!   fortran 2003 version released in CUTEst, 20th November 2012

      SUBROUTINE CUTEST_ufn_threadsafe_r( data, work, status, n, X, f )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( OUT ) :: f
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X

!  ----------------------------------------------------------
!  Compute the value of a groups partially separable function
!  initially written in Standard Input Format (SIF)
!  ----------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, j, ig, ifstat, igstat
      REAL ( KIND = rp_ ) :: ftt, one, zero
      REAL :: time_in, time_out
      PARAMETER ( zero = 0.0_rp_, one = 1.0_rp_ )

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  increment the counter for calls to the objective function value

      work%nc2of = work%nc2of + 1

!  there are non-trivial group functions.

      DO i = 1, MAX( data%nel, data%ng )
        work%ICALCF( i ) = i
      END DO

!  evaluate the element function values.

      CALL ELFUN_r( work%FUVALS, X, data%EPVALU, data%nel, data%ITYPEE,        &
                    data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,        &
                    data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,         &
                    data%lelvar, data%lntvar, data%lstadh, data%lstep,         &
                    data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,        &
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
        f = DOT_PRODUCT( data%GSCALE( : data%ng ), work%FT( : data%ng ) )
        work%GVALS( : data%ng, 1 ) = work%FT( : data%ng )
        work%GVALS( : data%ng, 2 ) = one

!  evaluate the group function values

      ELSE
        CALL GROUP_r( work%GVALS, data%ng, work%FT, data%GPVALU, data%ng,      &
                      data%ITYPEG, data%ISTGP, work%ICALCF, data%ltypeg,       &
                      data%lstgp, data%lcalcf, data%lcalcg, data%lgpvlu,       &
                      .FALSE., igstat )
        IF ( igstat /= 0 ) GO TO 930
        f = zero
        DO ig = 1, data%ng
          IF ( data%GXEQX( ig ) ) THEN
            f = f + data%GSCALE( ig ) * work%FT( ig )
          ELSE
            f = f + data%GSCALE( ig ) * work%GVALS( ig, 1 )
          END IF
        END DO
      END IF
      status = 0
      GO TO 990

!  unsuccessful returns

  930 CONTINUE
      IF ( data%out > 0 ) WRITE( data%out,                                     &
        "( ' ** SUBROUTINE UFN: error flag raised by SIF evaluation' )" )
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_ufn = work%time_ufn + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_ufn_threadsafe_r

      END SUBROUTINE CUTEST_ufn_threadsafe_r

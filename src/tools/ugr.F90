! THIS VERSION: CUTEST 2.3 - 2024-10-23 AT 10:00 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-*-  C U T E S T    U G R    S U B R O U T I N E  -*-*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 23rd December 2012

      SUBROUTINE CUTEST_ugr_r( status, n, X, G )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G

!  ------------------------------------------------------------
!  compute the gradient of a group partially separable function
!  ------------------------------------------------------------

      CALL CUTEST_ugr_threadsafe_r( CUTEST_data_global,                        &
                                    CUTEST_work_global( 1 ),                   &
                                    status, n, X, G )
      RETURN

!  end of subroutine CUTEST_ugr_r

      END SUBROUTINE CUTEST_ugr_r

!-*-*-*-  C U T E S T    U G R _ t h r e a d e d   S U B R O U T I N E  -*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 23rd December 2012

      SUBROUTINE CUTEST_ugr_threaded_r( status, n, X, G, thread )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, thread
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G

!  ------------------------------------------------------------
!  compute the gradient of a group partially separable function
!  ------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_ugr_threadsafe_r( CUTEST_data_global,                        &
                                    CUTEST_work_global( thread ),              &
                                    status, n, X, G )
      RETURN

!  end of subroutine CUTEST_ugr_threaded_r

      END SUBROUTINE CUTEST_ugr_threaded_r

!-*-*-  C U T E S T    U G R _ t h r e a d s a f e   S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTE, December 1990
!   fortran 2003 version released in CUTEst, 20th November 2012

      SUBROUTINE CUTEST_ugr_threadsafe_r( data, work, status, n, X, G )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G

!  ------------------------------------------------------------
!  compute the gradient of a group partially separable function
!  ------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, j, ig, ifstat, igstat
      REAL ( KIND = rp_ ) :: ftt
      REAL :: time_in, time_out
      EXTERNAL :: RANGE_r

      IF ( work%record_times ) CALL CPU_TIME( time_in )

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

!  evaluate the element function gradients

      CALL ELFUN_r( work%FUVALS, X, data%EPVALU, data%nel, data%ITYPEE,        &
                    data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,        &
                    data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,         &
                    data%lelvar, data%lntvar, data%lstadh, data%lstep,         &
                    data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,        &
                    2, ifstat )
      IF ( ifstat /= 0 ) GO TO 930

!  compute the group argument values ft

      DO ig = 1, data%ng
        ftt = - data%B( ig )

!  include the contribution from the linear element

        DO j = data%ISTADA( ig ), data%ISTADA( ig + 1 ) - 1
          ftt = ftt + data%A( j ) * X( data%ICNA( j ) )
        END DO

!  include the contributions from the nonlinear elements.

        DO j = data%ISTADG( ig ), data%ISTADG( ig + 1 ) - 1
          ftt = ftt + data%ESCALE( j ) * work%FUVALS( data%IELING( j ) )
        END DO
        work%FT( ig ) = ftt

!  record the derivatives of trivial groups

        IF ( data%GXEQX( ig ) ) work%GVALS( ig, 2 ) = 1.0_rp_
      END DO

!  evaluate the group derivative values.

      IF ( .NOT. data%altriv ) THEN
        CALL GROUP_r( work%GVALS, data%ng, work%FT, data%GPVALU, data%ng,      &
                      data%ITYPEG, data%ISTGP, work%ICALCF, data%ltypeg,       &
                      data%lstgp, data%lcalcf, data%lcalcg, data%lgpvlu,       &
                      .TRUE., igstat )
        IF ( igstat /= 0 ) GO TO 930
      END IF

!  Compute the gradient value.

      CALL CUTEST_form_gradients( n, data%ng, data%nel, data%ntotel,           &
             data%nvrels, data%nnza, data%nvargp, work%firstg, data%ICNA,      &
             data%ISTADA, data%IELING, data%ISTADG, data%ISTAEV,               &
             data%IELVAR, data%INTVAR, data%A, work%GVALS( : , 2 ),            &
             work%FUVALS, data%lnguvl, work%FUVALS( data%lggfx + 1 ),          &
             data%GSCALE, data%ESCALE, work%FUVALS( data%lgrjac + 1 ),         &
             data%GXEQX, data%INTREP, data%ISVGRP, data%ISTAGV, data%ITYPEE,   &
             work%ISTAJC, work%W_ws, work%W_el, RANGE_r )

      work%firstg = .FALSE.

!  Store the gradient value.

      DO i = 1, n
        G( i ) = work%FUVALS( data%lggfx + i )
      END DO

!  Update the counters for the report tool.

      work%nc2og = work%nc2og + 1
      status = 0
      GO TO 990

!  unsuccessful returns

  930 CONTINUE
      IF ( data%out > 0 ) WRITE( data%out,                                     &
        "( ' ** SUBROUTINE UGR: error flag raised during SIF evaluation' )" )
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_ugr = work%time_ugr + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_ugr_threadsafe_r

      END SUBROUTINE CUTEST_ugr_threadsafe_r


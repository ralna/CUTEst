! THIS VERSION: CUTEST 2.3 - 2024-10-23 AT 10:00 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-  C U T E S T  C I N T _  U O F G    S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 21st August 2013

      SUBROUTINE CUTEST_Cint_uofg_r( status, n, X, f, G, grad )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( OUT ) :: f
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: grad
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G

!  ---------------------------------------------------------------
!  compute the value of the objective function and its gradient
!  for a function initially written in Standard Input Format (SIF)

!  G     is an array which gives the value of the gradient of the
!        objective function evaluated at X. G(i) gives the partial
!        derivative of the objective function wrt variable X(i)
!  ---------------------------------------------------------------

      LOGICAL :: grad_fortran

      grad_fortran = grad
      CALL CUTEST_uofg_r( status, n, X, f, G, grad_fortran )

      RETURN

!  end of subroutine CUTEST_Cint_uofg_r

      END SUBROUTINE CUTEST_Cint_uofg_r

!-*-*-*-*-*-*-*-  C U T E S T   U O F G   S U B R O U T I N E  -*-*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 23rd December 2012

      SUBROUTINE CUTEST_uofg_r( status, n, X, f, G, grad )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( OUT ) :: f
      LOGICAL, INTENT( IN ) :: grad
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G

!  ---------------------------------------------------------------
!  compute the value of the objective function and its gradient
!  for a function initially written in Standard Input Format (SIF)

!  G     is an array which gives the value of the gradient of the
!        objective function evaluated at X. G(i) gives the partial
!        derivative of the objective function wrt variable X(i)
!  ---------------------------------------------------------------

      CALL CUTEST_uofg_threadsafe_r( CUTEST_data_global,                       &
                                     CUTEST_work_global( 1 ),                  &
                                     status, n, X, f, G, grad )
      RETURN

!  end of subroutine CUTEST_uofg_r

      END SUBROUTINE CUTEST_uofg_r

!-*-*-   C U T E S T    U O F G _ t h r e a d e d   S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 23rd December 2012

      SUBROUTINE CUTEST_uofg_threaded_r( status, n, X, f, G, grad, thread )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, thread
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( OUT ) :: f
      LOGICAL, INTENT( IN ) :: grad
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G

!  ---------------------------------------------------------------
!  compute the value of the objective function and its gradient
!  for a function initially written in Standard Input Format (SIF)

!  G     is an array which gives the value of the gradient of the
!        objective function evaluated at X. G(i) gives the partial
!        derivative of the objective function wrt variable X(i)
!  ---------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_uofg_threadsafe_r( CUTEST_data_global,                       &
                                     CUTEST_work_global( thread ),             &
                                     status, n, X, f, G, grad )
      RETURN

!  end of subroutine CUTEST_uofg_threaded_r

      END SUBROUTINE CUTEST_uofg_threaded_r

!-*-   C U T E S T    U O F G _ t h r e a d s a f e   S U B R O U T I N E   -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Ingrid Bongartz and Nick Gould

!  History -
!   fortran 77 version originally released in CUTE, February 1993
!   fortran 2003 version released in CUTEst, 20th November 2012

      SUBROUTINE CUTEST_uofg_threadsafe_r( data, work, status, n, X, f, G,     &
                                           grad )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( OUT ) :: f
      LOGICAL, INTENT( IN ) :: grad
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G

!  ---------------------------------------------------------------
!  compute the value of the objective function and its gradient
!  for a function initially written in Standard Input Format (SIF)

!  G     is an array which gives the value of the gradient of the
!        objective function evaluated at X. G(i) gives the partial
!        derivative of the objective function wrt variable X(i)
!  ---------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, j, ig, ifstat, igstat
      REAL ( KIND = rp_ ) :: ftt
      REAL :: time_in, time_out
      EXTERNAL :: RANGE_r

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  there are non-trivial group functions

      DO i = 1, MAX( data%nel, data%ng )
        work%ICALCF( i ) = i
      END DO

!  evaluate the element function values

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

!  record the derivatives of trivial groups

        IF ( data%GXEQX( ig ) ) work%GVALS( ig, 2 ) = 1.0_rp_
      END DO

!  compute the group function values

!  all group functions are trivial

      IF ( data%altriv ) THEN
        f = DOT_PRODUCT( data%GSCALE( : data%ng ), work%FT( : data%ng ) )
        work%GVALS( : data%ng, 1 ) = work%FT( : data%ng )
        work%GVALS( : data%ng, 2 ) = 1.0_rp_

!  evaluate the group function values

      ELSE
        CALL GROUP_r( work%GVALS, data%ng, work%FT, data%GPVALU, data%ng,      &
                      data%ITYPEG, data%ISTGP, work%ICALCF, data%ltypeg,       &
                      data%lstgp, data%lcalcf, data%lcalcg, data%lgpvlu,       &
                      .FALSE., igstat )
        IF ( igstat /= 0 ) GO TO 930

!  compute the objective function value

        f = 0.0_rp_
        DO ig = 1, data%ng
          IF ( data%GXEQX( ig ) ) THEN
            f = f + data%GSCALE( ig ) * work%FT( ig )
          ELSE
            f = f + data%GSCALE( ig ) * work%GVALS( ig, 1 )
          END IF
        END DO
      END IF

!  evaluate the element function gradients

      IF ( grad ) THEN
        CALL ELFUN_r( work%FUVALS, X, data%EPVALU, data%nel, data%ITYPEE,      &
                      data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,      &
                      data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,       &
                      data%lelvar, data%lntvar, data%lstadh, data%lstep,       &
                      data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,      &
                      2, ifstat )
        IF ( ifstat /= 0 ) GO TO 930

!  evaluate the group derivative values

        IF ( .NOT. data%altriv ) THEN
          CALL GROUP_r( work%GVALS, data%ng, work%FT, data%GPVALU, data%ng,    &
                        data%ITYPEG, data%ISTGP, work%ICALCF, data%ltypeg,     &
                        data%lstgp, data%lcalcf, data%lcalcg, data%lgpvlu,     &
                        .TRUE., igstat )
          IF ( igstat /= 0 ) GO TO 930
        END IF

!  compute the gradient values

      CALL CUTEST_form_gradients( n, data%ng, data%nel, data%ntotel,           &
             data%nvrels, data%nnza, data%nvargp, work%firstg, data%ICNA,      &
             data%ISTADA, data%IELING, data%ISTADG, data%ISTAEV,               &
             data%IELVAR, data%INTVAR, data%A, work%GVALS( : , 2 ),            &
             work%FUVALS, data%lnguvl, work%FUVALS( data%lggfx + 1 ),          &
             data%GSCALE, data%ESCALE, work%FUVALS( data%lgrjac + 1 ),         &
             data%GXEQX, data%INTREP, data%ISVGRP, data%ISTAGV, data%ITYPEE,   &
             work%ISTAJC, work%W_ws, work%W_el, RANGE_r )
        work%firstg = .FALSE.

!  store the gradient value

        DO i = 1, n
          G( i ) = work%FUVALS( data%lggfx + i )
        END DO
      END IF

!  update the counters for the report tool

      work%nc2of = work%nc2of + 1
      IF ( grad ) work%nc2og = work%nc2og + 1
      status = 0
      GO TO 990

!  unsuccessful returns

  930 CONTINUE
      IF ( data%out > 0 ) WRITE( data%out,                                     &
        "( ' ** SUBROUTINE UOFG: Error flag raised during SIF evaluation' )" )
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_uofg = work%time_uofg + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_uofg_threadsafe_r

      END SUBROUTINE CUTEST_uofg_threadsafe_r

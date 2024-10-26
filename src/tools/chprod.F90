! THIS VERSION: CUTEST 2.3 - 2024-10-23 AT 10:00 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-  C U T E S T   C I N T _ C H P R O D    S U B R O U T I N E  -*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 21st August 2013

      SUBROUTINE CUTEST_Cint_chprod_r( status, n, m, goth, X, Y, VECTOR,       &
                                       RESULT )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: goth
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: RESULT

!  -----------------------------------------------------------------------
!  compute the matrix-vector product between the Hessian matrix of
!  the Lagrangian function for the problem and  a given vector VECTOR.
!  The result is placed in RESULT. If goth is .TRUE. the second
!  derivatives are assumed to have already been computed. If the user is
!  unsure, set goth = .FALSE. the first time a product is required with
!  the Hessian evaluated at X and Y. X and Y are not used if goth = .TRUE.
!  -----------------------------------------------------------------------

      LOGICAL :: goth_fortran

      goth_fortran = goth
      CALL CUTEST_chprod_r( status, n, m, goth_fortran, X, Y, VECTOR, RESULT )

      RETURN

!  end of subroutine CUTEST_Cint_chprod_r

      END SUBROUTINE CUTEST_Cint_chprod_r

!-*-*-*-*-*-*-  C U T E S T    C H P R O D    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 29th December 2012

      SUBROUTINE CUTEST_chprod_r( status, n, m, goth, X, Y, VECTOR, RESULT )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL, INTENT( IN ) :: goth
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: RESULT

!  -----------------------------------------------------------------------
!  compute the matrix-vector product between the Hessian matrix of
!  the Lagrangian function for the problem and  a given vector VECTOR.
!  The result is placed in RESULT. If goth is .TRUE. the second
!  derivatives are assumed to have already been computed. If the user is
!  unsure, set goth = .FALSE. the first time a product is required with
!  the Hessian evaluated at X and Y. X and Y are not used if goth = .TRUE.
!  -----------------------------------------------------------------------

      CALL CUTEST_chprod_threadsafe_r( CUTEST_data_global,                     &
                                       CUTEST_work_global( 1 ),                &
                                       status, n, m, goth, X, Y, VECTOR,       &
                                       RESULT )
      RETURN

!  end of subroutine CUTEST_chprod_r

      END SUBROUTINE CUTEST_chprod_r

!-*-*-  C U T E S T   C H P R O D _ t h r e a d e d   S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 29th December 2012

      SUBROUTINE CUTEST_chprod_threaded_r( status, n, m, goth, X, Y,           &
                                           VECTOR, RESULT, thread )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, thread
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL, INTENT( IN ) :: goth
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: RESULT

!  -----------------------------------------------------------------------
!  compute the matrix-vector product between the Hessian matrix of
!  the Lagrangian function for the problem and  a given vector VECTOR.
!  The result is placed in RESULT. If goth is .TRUE. the second
!  derivatives are assumed to have already been computed. If the user is
!  unsure, set goth = .FALSE. the first time a product is required with
!  the Hessian evaluated at X and Y. X and Y are not used if goth = .TRUE.
!  -----------------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_chprod_threadsafe_r( CUTEST_data_global,                     &
                                       CUTEST_work_global( thread ),           &
                                       status, n, m, goth, X, Y, VECTOR,       &
                                       RESULT )
      RETURN

!  end of subroutine CUTEST_chprod_threaded_r

      END SUBROUTINE CUTEST_chprod_threaded_r

!-*-  C U T E S T   C H P R O D _ t h r e a d s a f e   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Ingrid Bongartz and Nick Gould

!  History -
!   fortran 77 version originally released as CPROD in CUTE, November 1991
!   fortran 2003 version released in CUTEst, 28th November 2012

      SUBROUTINE CUTEST_chprod_threadsafe_r( data, work, status, n, m, goth,   &
                                             X, Y, VECTOR, RESULT )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL, INTENT( IN ) :: goth
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: RESULT

!  -----------------------------------------------------------------------
!  compute the matrix-vector product between the Hessian matrix of
!  the Lagrangian function for the problem and  a given vector VECTOR.
!  The result is placed in RESULT. If goth is .TRUE. the second
!  derivatives are assumed to have already been computed. If the user is
!  unsure, set goth = .FALSE. the first time a product is required with
!  the Hessian evaluated at X and Y. X and Y are not used if goth = .TRUE.
!  -----------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, ig, j, ifstat, igstat
      REAL ( KIND = rp_ ) :: ftt
      REAL :: time_in, time_out
      EXTERNAL :: RANGE_r

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  there are non-trivial group functions

      IF ( .NOT. goth ) THEN
        DO i = 1, MAX( data%nel, data%ng )
          work%ICALCF( i ) = i
        END DO

!  evaluate the element function values

        CALL ELFUN_r( work%FUVALS, X, data%EPVALU, data%nel, data%ITYPEE,      &
                      data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,      &
                      data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,       &
                      data%lelvar, data%lntvar, data%lstadh, data%lstep,       &
                      data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,      &
                      1, ifstat )
        IF ( ifstat /= 0 ) GO TO 930

!  evaluate the element function gradient and Hessian values

        CALL ELFUN_r( work%FUVALS, X, data%EPVALU, data%nel, data%ITYPEE,      &
                      data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,      &
                      data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,       &
                      data%lelvar, data%lntvar, data%lstadh, data%lstep,       &
                      data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,      &
                      3, ifstat )
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

!  record the derivatives of trivial groups

          IF ( data%GXEQX( ig ) ) THEN
            work%GVALS( ig, 2 ) = 1.0_rp_
            work%GVALS( ig, 3 ) = 0.0_rp_
          END IF
        END DO

!  evaluate the group derivative values

        IF ( .NOT. data%altriv ) THEN
          CALL GROUP_r( work%GVALS, data%ng, work%FT, data%GPVALU, data%ng,    &
                        data%ITYPEG, data%ISTGP, work%ICALCF, data%ltypeg,     &
                        data%lstgp, data%lcalcf, data%lcalcg, data%lgpvlu,     &
                        .TRUE., igstat )
          IF ( igstat /= 0 ) GO TO 930
        END IF

!  change the group weightings to include the contributions from the
!  Lagrange multipliers.

        IF ( data%numcon > 0 ) THEN
          DO ig = 1, data%ng
            i = data%KNDOFC( ig )
            IF ( i == 0 ) THEN
              work%GSCALE_used( ig ) = data%GSCALE( ig )
            ELSE
              work%GSCALE_used( ig ) = data%GSCALE( ig ) * Y( i )
            END IF
          END DO

!  compute the gradient value

          CALL CUTEST_form_gradients( n, data%ng, data%nel, data%ntotel,       &
                 data%nvrels, data%nnza, data%nvargp, work%firstg, data%ICNA,  &
                 data%ISTADA, data%IELING, data%ISTADG, data%ISTAEV,           &
                 data%IELVAR, data%INTVAR, data%A, work%GVALS( : , 2 ),        &
                 work%FUVALS, data%lnguvl, work%FUVALS( data%lggfx + 1 ),      &
                 work%GSCALE_used, data%ESCALE, work%FUVALS( data%lgrjac + 1 ),&
                 data%GXEQX, data%INTREP, data%ISVGRP, data%ISTAGV,            &
                 data%ITYPEE, work%ISTAJC, work%W_ws, work%W_el, RANGE_r )
        ELSE
          CALL CUTEST_form_gradients( n, data%ng, data%nel, data%ntotel,       &
                 data%nvrels, data%nnza, data%nvargp, work%firstg, data%ICNA,  &
                 data%ISTADA, data%IELING, data%ISTADG, data%ISTAEV,           &
                 data%IELVAR, data%INTVAR, data%A, work%GVALS( : , 2 ),        &
                 work%FUVALS, data%lnguvl, work%FUVALS( data%lggfx + 1 ),      &
                 data%GSCALE, data%ESCALE, work%FUVALS( data%lgrjac + 1 ),     &
                 data%GXEQX, data%INTREP, data%ISVGRP, data%ISTAGV,            &
                 data%ITYPEE, work%ISTAJC, work%W_ws, work%W_el, RANGE_r )
        END IF
      END IF

!  initialize RESULT as the zero vector

      RESULT( : n ) = 0.0_rp_

!  evaluate the product

      IF ( data%numcon > 0 ) THEN
        CALL CUTEST_hessian_times_vector(                                      &
          data%n, data%ng, data%nel, data%ntotel, data%nvrels, data%nvargp,    &
          data%alllin, data%ISTAEV, data%ISTADH,                               &
          data%INTVAR, data%IELING, data%IELVAR, VECTOR, RESULT,               &
          work%GVALS( : , 2 ) , work%GVALS( : , 3 ),                           &
          work%FUVALS( data%lgrjac + 1 ),                                      &
          work%GSCALE_used, data%ESCALE, work%FUVALS( : data%lnhuvl ),         &
          data%lnhuvl, data%GXEQX, data%INTREP, data%IGCOLJ,                   &
          data%ISLGRP, data%ITYPEE, data%ISYMMH, work%ISTAJC,                  &
          work%W_ws, work%W_el, work%W_in, work%H_in, RANGE_r )
      ELSE
        CALL CUTEST_hessian_times_vector(                                      &
          data%n, data%ng, data%nel, data%ntotel, data%nvrels, data%nvargp,    &
          data%alllin, data%ISTAEV, data%ISTADH,                               &
          data%INTVAR, data%IELING, data%IELVAR, VECTOR, RESULT,               &
          work%GVALS( : , 2 ) , work%GVALS( : , 3 ),                           &
          work%FUVALS( data%lgrjac + 1 ),                                      &
          data%GSCALE, data%ESCALE, work%FUVALS( : data%lnhuvl ),              &
          data%lnhuvl, data%GXEQX, data%INTREP, data%IGCOLJ,                   &
          data%ISLGRP, data%ITYPEE, data%ISYMMH, work%ISTAJC,                  &
          work%W_ws, work%W_el, work%W_in, work%H_in, RANGE_r )
      END IF

!  update the counters for the report tool

      work%nhvpr = work%nhvpr + 1
      IF ( .NOT. goth ) THEN
        work%nc2oh = work%nc2oh + 1
        work%nc2ch = work%nc2ch + work%pnc
      END IF
      status = 0
      GO TO 990

!  unsuccessful returns

  930 CONTINUE
      IF ( data%out > 0 ) WRITE( data%out,                                     &
        "( ' ** SUBROUTINE CHPROD: error flag raised during SIF evaluation' )" )
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_chprod = work%time_chprod + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_chprod_threadsafe_r

      END SUBROUTINE CUTEST_chprod_threadsafe_r



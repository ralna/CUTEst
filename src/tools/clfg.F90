! THIS VERSION: CUTEST 2.3 - 2024-10-23 AT 10:00 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-*-  C U T E S T    C L F G    S U B R O U T I N E  -*-*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 12th October 2013

      SUBROUTINE CUTEST_Cint_clfg_r( status, n, m, X, Y, f, G, grad )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( OUT ) :: f
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: grad
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G

!  ------------------------------------------------------------------------
!  compute the value of the Lagrangian function and optionally its gradient
!  for a problem initially written in Standard Input Format (SIF).

!  f gives the value of the Lagrangian function evaluated at (X,Y)

!  G is an array which gives the value of the gradient of the
!    Lagrangian function evaluated at (X,Y). G(i) gives the partial
!    derivative of the Lagrangian function wrt variable X(i)
!  ------------------------------------------------------------------------

      LOGICAL :: grad_fortran

      grad_fortran = grad
      CALL CUTEST_clfg_r( status, n, m, X, Y, f, G, grad_fortran )
      RETURN

!  end of subroutine CUTEST_Cint_clfg_r

      END SUBROUTINE CUTEST_Cint_clfg_r

!-*-*-*-*-*-*-*-  C U T E S T    C L F G    S U B R O U T I N E  -*-*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 12th October 2013

      SUBROUTINE CUTEST_clfg_r( status, n, m, X, Y, f, G, grad )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( OUT ) :: f
      LOGICAL, INTENT( IN ) :: grad
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G

!  ------------------------------------------------------------------------
!  compute the value of the Lagrangian function and optionally its gradient
!  for a problem initially written in Standard Input Format (SIF).

!  f gives the value of the Lagrangian function evaluated at (X,Y)

!  G is an array which gives the value of the gradient of the
!    Lagrangian function evaluated at (X,Y). G(i) gives the partial
!    derivative of the Lagrangian function wrt variable X(i)
!  ------------------------------------------------------------------------

      CALL CUTEST_clfg_threadsafe_r( CUTEST_data_global,                       &
                                     CUTEST_work_global( 1 ),                  &
                                     status, n, m, X, Y, f, G, grad )
      RETURN

!  end of subroutine CUTEST_clfg_r

      END SUBROUTINE CUTEST_clfg_r

!-*-*-*-  C U T E S T   C L F G _ t h r e a d e d   S U B R O U T I N E  -*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th December 2012

      SUBROUTINE CUTEST_clfg_threaded_r( status, n, m, X, Y, f, G, grad,       &
                                         thread )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, thread
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( OUT ) :: f
      LOGICAL, INTENT( IN ) :: grad
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G

!  ------------------------------------------------------------------------
!  compute the value of the Lagrangian function and optionally its gradient
!  for a problem initially written in Standard Input Format (SIF).

!  f gives the value of the Lagrangian function evaluated at (X,Y)

!  G is an array which gives the value of the gradient of the
!    Lagrangian function evaluated at (X,Y). G(i) gives the partial
!    derivative of the Lagrangian function wrt variable X(i)
!  ------------------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_clfg_threadsafe_r( CUTEST_data_global,                       &
                                     CUTEST_work_global( thread ),             &
                                     status, n, m, X, Y, f, G, grad )
      RETURN

!  end of subroutine CUTEST_clfg_threaded_r

      END SUBROUTINE CUTEST_clfg_threaded_r

!-*-*-  C U T E S T   C L F G _ t h r e a d s a f e   S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTE, November 1991
!   fortran 2003 version released in CUTEst, 20th November 2012

      SUBROUTINE CUTEST_clfg_threadsafe_r( data, work, status, n, m, X, Y,     &
                                           f, G, grad )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( OUT ) :: f
      LOGICAL, INTENT( IN ) :: grad
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G

!  ------------------------------------------------------------------------
!  compute the value of the Lagrangian function and optionally its gradient
!  for a problem initially written in Standard Input Format (SIF).

!  f gives the value of the Lagrangian function evaluated at (X,Y)

!  G is an array which gives the value of the gradient of the
!    Lagrangian function evaluated at (X,Y). G(i) gives the partial
!    derivative of the Lagrangian function wrt variable X(i)
!  ------------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, icon, iel, k, ig, ii, ig1, j, jj
      INTEGER ( KIND = ip_ ) :: istrgv, iendgv, ifstat, igstat, l, ll
      INTEGER ( KIND = ip_ ) :: nelow, nelup, nin, nvarel
      LOGICAL :: nontrv
      REAL ( KIND = rp_ ) :: ftt, gi, scalee, gii
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

! evaluate the element function gradients

      IF ( grad )                                                              &
        CALL ELFUN_r( work%FUVALS, X, data%EPVALU, data%nel, data%ITYPEE,      &
                      data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,      &
                      data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,       &
                      data%lelvar, data%lntvar, data%lstadh, data%lstep,       &
                      data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,      &
                      2, ifstat )
      IF ( ifstat /= 0 ) GO TO 930

!  compute the group argument values ft.

      DO ig = 1, data%ng
        ftt = - data%B( ig )

!  include the contribution from the linear element.

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

!  compute the Lagrangian function value

      f = 0.0_rp_

!  there are constraints

      IF ( data%numcon > 0 ) THEN
        DO ig = 1, data%ng
          icon = data%KNDOFC( ig )
          IF ( icon == 0 ) THEN
            gii = data%GSCALE( ig )
          ELSE
            gii = Y( icon ) * data%GSCALE( ig )
          END IF
          IF ( data%GXEQX( ig ) ) THEN
            f = f + gii * work%FT( ig )
          ELSE
            f = f + gii * work%GVALS( ig, 1 )
          END IF
        END DO

!  there are no constraints, so we need not check data%KNDOFC( ig )

      ELSE
        DO ig = 1, data%ng
          IF ( data%GXEQX( ig ) ) THEN
            f = f + data%GSCALE( ig ) * work%FT( ig )
          ELSE
            f = f + data%GSCALE( ig ) * work%GVALS( ig, 1 )
          END IF
        END DO
      END IF

      IF ( grad ) THEN

!  for unconstrained problems, skip construction of the gradient
!  Call ELGRD instead

        IF ( data%numcon > 0 ) THEN

!  compute the gradient values. Initialize the gradient as zero

          G( : n ) = 0.0_rp_

!  consider the ig-th group

          DO ig = 1, data%ng
            ig1 = ig + 1
            icon = data%KNDOFC( ig )
            istrgv = data%ISTAGV( ig )
            iendgv = data%ISTAGV( ig1 ) - 1
            nelow = data%ISTADG( ig )
            nelup = data%ISTADG( ig1 ) - 1
            nontrv = .NOT. data%GXEQX( ig )

!  compute the first derivative of the group

            gi = data%GSCALE( ig )
            IF ( icon == 0 ) THEN
              gii = gi
            ELSE
              gii = gi * Y( icon )
            END IF
            IF ( nontrv ) THEN
              gi = gi * work%GVALS( ig, 2 )
              gii = gii * work%GVALS( ig, 2 )
            END IF

!  this is the first gradient evaluation or the group has nonlinear elements

            IF ( work%firstg .OR. nelow <= nelup ) THEN
              work%W_ws( data%ISVGRP( istrgv : iendgv ) ) = 0.0_rp_

!  loop over the group's nonlinear elements

              DO ii = nelow, nelup
                iel = data%IELING( ii )
                k = data%INTVAR( iel )
                l = data%ISTAEV( iel )
                nvarel = data%ISTAEV( iel + 1 ) - l
                scalee = data%ESCALE( ii )
                IF ( data%INTREP( iel ) ) THEN

!  the iel-th element has an internal representation

                  nin = data%INTVAR( iel + 1 ) - k
                  CALL RANGE_r( iel, .TRUE., work%FUVALS( k ),                 &
                                work%W_el, nvarel, nin, data%ITYPEE( iel ),    &
                                nin, nvarel )
!DIR$ IVDEP
                  DO i = 1, nvarel
                    j = data%IELVAR( l )
                    work%W_ws( j ) = work%W_ws( j ) + scalee * work%W_el( i )
                     l = l + 1
                  END DO

!  the iel-th element has no internal representation

                ELSE
!DIR$ IVDEP
                  DO i = 1, nvarel
                    j = data%IELVAR( l )
                    work%W_ws( j ) = work%W_ws( j ) + scalee * work%FUVALS( k )
                    k = k + 1 ; l = l + 1
                  END DO
                END IF
              END DO

!  include the contribution from the linear element

!DIR$ IVDEP
              DO k = data%ISTADA( ig ), data%ISTADA( ig1 ) - 1
                j = data%ICNA( k )
                work%W_ws( j ) = work%W_ws( j ) + data%A( k )
              END DO

!  allocate a gradient

!DIR$ IVDEP
              DO i = istrgv, iendgv
                ll = data%ISVGRP( i )

!  the group belongs to the objective function

                IF ( icon == 0 ) THEN
                  G( ll ) = G( ll ) + gi * work%W_ws( ll )

!  the group defines a constraint

                ELSE
                  G( ll ) = G( ll ) + gii * work%W_ws( ll )
                END IF

!  if the group is non-trivial, also store the nonzero entries of the
!  gradient of the function in GRJAC

                IF ( nontrv ) THEN
                  jj = work%ISTAJC( ll )
                  work%FUVALS( data%lgrjac + jj ) = work%W_ws( ll )

!  increment the address for the next nonzero in the column of the Jacobian
!  for variable ll

                  work%ISTAJC( ll ) = jj + 1
                END IF
              END DO

!  this is not the first gradient evaluation and there is only a linear element

            ELSE

!  allocate a gradient

!DIR$ IVDEP
              DO k = data%ISTADA( ig ), data%ISTADA( ig1 ) - 1
                ll = data%ICNA( k )

!  the group belongs to the objective function

                IF ( icon == 0 ) THEN
                  G( ll ) = G( ll ) + gi * data%A( k )

!  the group defines a constraint

                ELSE
                  G( ll ) = G( ll ) + gii * data%A( k )
                END IF
              END DO

!  the group is non-trivial; increment the starting addresses for the groups
!  used by each variable in the (unchanged) linear element to avoid resetting
!  the nonzeros in the Jacobian

              IF ( nontrv ) THEN
!DIR$ IVDEP
                DO i = istrgv, iendgv
                  ll = data%ISVGRP( i )
                  work%ISTAJC( ll ) = work%ISTAJC( ll ) + 1
                END DO
              END IF
            END IF
          END DO

!  reset the starting addresses for the lists of groups using each variable to
!  their values on entry

          DO i = n, 2, - 1
            work%ISTAJC( i ) = work%ISTAJC( i - 1 )
          END DO
          work%ISTAJC( 1 ) = 1

!  compute the gradient value

        ELSE
          CALL CUTEST_form_gradients( n, data%ng, data%nel, data%ntotel,       &
                 data%nvrels, data%nnza, data%nvargp, work%firstg, data%ICNA,  &
                 data%ISTADA, data%IELING, data%ISTADG, data%ISTAEV,           &
                 data%IELVAR, data%INTVAR, data%A, work%GVALS( : , 2 ),        &
                 work%FUVALS, data%lnguvl, work%FUVALS( data%lggfx + 1 ),      &
                 data%GSCALE, data%ESCALE, work%FUVALS( data%lgrjac + 1 ),     &
                 data%GXEQX, data%INTREP, data%ISVGRP, data%ISTAGV,            &
                 data%ITYPEE, work%ISTAJC, work%W_ws, work%W_el, RANGE_r )

!  store the gradient value

           DO i = 1, n
            G( i ) = work%FUVALS( data%lggfx + i )
          END DO
        END IF
        work%firstg = .FALSE.

!  update the counters for the report tool

        work%nc2og = work%nc2og + 1
        work%nc2cg = work%nc2cg + work%pnc
      END IF
      work%nc2of = work%nc2of + 1
      status = 0
      GO TO 990

!  unsuccessful returns

  930 CONTINUE
      IF ( data%out > 0 ) WRITE( data%out,                                     &
        "( ' ** SUBROUTINE CLFG: error flag raised during SIF evaluation' )" )
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_clfg = work%time_clfg + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_clfg_threadsafe_r

      END SUBROUTINE CUTEST_clfg_threadsafe_r

! THIS VERSION: CUTEST 2.3 - 2024-10-30 AT 08:40 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-  C U T E S T    C O F S G _ C   S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 23rd October 2024

      SUBROUTINE CUTEST_cofsg_c_r( status, n, X, f, nnzg, lg, G_val, G_var, &
                                   grad )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lg
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, nnzg
      REAL ( KIND = rp_ ), INTENT( OUT ) :: f
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: grad
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lg ) :: G_var
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lg ) :: G_val

!  ---------------------------------------------------------------------------
!  compute the value of the objective function and its sparse gradient for a
!  function initially written in Standard Input Format (SIF)

!  G_val is an array whose i-th component gives the value of the partial
!    derivative of the objective function with respect to 0-based variable
!    X(G_var(i)-1) for i = 1, ..., nnzg. All other partial derivatves are zero
!  ---------------------------------------------------------------------------

!  local variables

      LOGICAL :: grad_fortran

      grad_fortran = grad
      CALL CUTEST_cofsg_r( status, n, X, f, nnzg, lg, G_val, G_var,            &
                           grad_fortran )

      IF ( grad_fortran ) G_var( : nnzg ) = G_var( : nnzg ) - 1

      RETURN

!  end of subroutine CUTEST_cofsg_c_r

      END SUBROUTINE CUTEST_cofsg_c_r

!-*-*-*-*-  C U T E S T  C I N T _  C O F S G    S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Ingrid Bongartz and Nick Gould

!  History -
!   fortran 2003 version released in CUTEst,  21st August 2013

      SUBROUTINE CUTEST_Cint_cofsg_r( status, n, X, f, nnzg, lg, G_val, G_var, &
                                      grad )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lg
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, nnzg
      REAL ( KIND = rp_ ), INTENT( OUT ) :: f
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: grad
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lg ) :: G_var
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lg ) :: G_val

!  -------------------------------------------------------------------------
!  compute the value of the objective function and its sparse gradient for a
!  function initially written in Standard Input Format (SIF)

!  G_val is an array whose i-th component gives the value of the partial
!    derivative of the objective function with respect to variable
!    X(G_var(i)) for i = 1, ..., nnzg. All other partial derivatves are zero
!  -------------------------------------------------------------------------

      LOGICAL :: grad_fortran

      grad_fortran = grad
      CALL CUTEST_cofsg_r( status, n, X, f, nnzg, lg, G_val, G_var,            &
                           grad_fortran )

      RETURN

!  end of subroutine CUTEST_Cint_cofsg_r

      END SUBROUTINE CUTEST_Cint_cofsg_r

!-*-*-*-*-*-*-*-  C U T E S T    C O F S G    S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Ingrid Bongartz and Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th February 2013

      SUBROUTINE CUTEST_cofsg_r( status, n, X, f, nnzg, lg, G_val, G_var, grad )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lg
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, nnzg
      REAL ( KIND = rp_ ), INTENT( OUT ) :: f
      LOGICAL, INTENT( IN ) :: grad
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lg ) :: G_var
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lg ) :: G_val

!  -------------------------------------------------------------------------
!  compute the value of the objective function and its sparse gradient for a
!  function initially written in Standard Input Format (SIF)

!  G_val is an array whose i-th component gives the value of the partial
!    derivative of the objective function with respect to variable
!    X(G_var(i)) for i = 1, ..., nnzg. All other partial derivatves are zero
!  -------------------------------------------------------------------------

      CALL CUTEST_cofsg_threadsafe_r( CUTEST_data_global,                      &
                                      CUTEST_work_global( 1 ),                 &
                                      status, n, X, f,                         &
                                      nnzg, lg, G_val, G_var, grad )
      RETURN

!  end of subroutine CUTEST_cofsg_r

      END SUBROUTINE CUTEST_cofsg_r

!-*-*-*-  C U T E S T   C O F G _ t h r e a d e d   S U B R O U T I N E  -*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Ingrid Bongartz and Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th December 2012

      SUBROUTINE CUTEST_cofsg_threaded_r( status, n, X, f,                     &
                                          nnzg, lg, G_val, G_var, grad, thread )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lg, thread
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, nnzg
      REAL ( KIND = rp_ ), INTENT( OUT ) :: f
      LOGICAL, INTENT( IN ) :: grad
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lg ) :: G_var
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lg ) :: G_val

!  -------------------------------------------------------------------------
!  compute the value of the objective function and its sparse gradient for a
!  function initially written in Standard Input Format (SIF)

!  G_val is an array whose i-th component gives the value of the partial
!    derivative of the objective function with respect to variable
!    X(G_var(i)) for i = 1, ..., nnzg. All other partial derivatves are zero
!  -------------------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_cofsg_threadsafe_r( CUTEST_data_global,                      &
                                      CUTEST_work_global( thread ),            &
                                      status, n, X, f,                         &
                                      nnzg, lg, G_val, G_var, grad )
      RETURN

!  end of subroutine CUTEST_cofsg_threaded_r

      END SUBROUTINE CUTEST_cofsg_threaded_r

!-*-*-  C U T E S T   C O F G _ t h r e a d s a f e   S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Ingrid Bongartz and Nick Gould

!  History -
!   fortran 77 version originally released in CUTE, April 1992
!   fortran 2003 version released in CUTEst, 28th November 2012

      SUBROUTINE CUTEST_cofsg_threadsafe_r( data, work, status, n, X, f,       &
                                            nnzg, lg, G_val, G_var, grad )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lg
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, nnzg
      REAL ( KIND = rp_ ), INTENT( OUT ) :: f
      LOGICAL, INTENT( IN ) :: grad
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lg ) :: G_var
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lg ) :: G_val

!  -------------------------------------------------------------------------
!  compute the value of the objective function and its sparse gradient for a
!  function initially written in Standard Input Format (SIF)

!  G_val is an array whose i-th component gives the value of the partial
!    derivative of the objective function with respect to variable
!    X(G_var(i)) for i = 1, ..., nnzg. All other partial derivatves are zero
!  -------------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, j, iel, ig, ii, ig1, l, icon, icnt
      INTEGER ( KIND = ip_ ) :: ifstat, igstat, istrgv, iendgv, k, ll
      INTEGER ( KIND = ip_ ) :: nin, nvarel, nelow, nelup
      REAL ( KIND = rp_ ) :: ftt, gi, scalee
      REAL :: time_in, time_out

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  identify which elements are included in objective function. Use LOGIC
!  to keep track of elements already included

      work%LOGIC( : data%nel ) = .FALSE.

!  Use ISWKSP to flag which variables have nonzero partial derivatives

      IF ( grad ) THEN
        work%ISWKSP( : data%n ) = 0
        nnzg = 0
      END IF

!  now identify elements in objective function groups

      icnt = 0
      DO ig = 1, data%ng
        IF ( data%KNDOFC( ig ) == 0 ) THEN
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

!  evaluate the element function derivatives

      IF ( grad )                                                              &
        CALL ELFUN_r( work%FUVALS, X, data%EPVALU, icnt, data%ITYPEE,          &
                      data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,      &
                      data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,       &
                      data%lelvar, data%lntvar, data%lstadh, data%lstep,       &
                      data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,      &
                      2, ifstat )
      IF ( ifstat /= 0 ) GO TO 930

!  compute the group argument values ft

      IF ( data%numcon > 0 ) THEN
        DO ig = 1, data%ng
          ftt = 0.0_rp_

!  consider only those groups in the objective function

          IF ( data%KNDOFC( ig ) == 0 ) THEN
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

!  record the derivatives of trivial groups

            IF ( data%GXEQX( ig ) ) work%GVALS( ig, 2 ) = 1.0_rp_
          END IF
          work%FT( ig ) = ftt
        END DO
      ELSE

!  there are no constraints, so we need not check data%KNDOFC(ig)

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

!  record the derivatives of trivial groups

          IF ( data%GXEQX( ig ) ) work%GVALS( ig, 2 ) = 1.0_rp_
          work%FT( ig ) = ftt
        END DO
      END IF

!  compute the group function values

!  all group functions are trivial

      IF ( data%altriv ) THEN
        work%GVALS( : data%ng, 1 ) = work%FT( : data%ng )
        work%GVALS( : data%ng, 2 ) = 1.0_rp_
      ELSE

!  evaluate the group function values. Only evaluate groups belonging to the
!  objective function

        icnt = 0
        DO ig = 1, data%ng
          IF ( data%KNDOFC( ig ) == 0 ) THEN
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

!  compute the objective function value

      f = 0.0_rp_

!  there are constraints

      IF ( data%numcon > 0 ) THEN
        DO ig = 1, data%ng
          IF ( data%KNDOFC( ig ) == 0 ) THEN
            IF ( data%GXEQX( ig ) ) THEN
              f = f + data%GSCALE( ig ) * work%FT( ig )
            ELSE
              f = f + data%GSCALE( ig ) * work%GVALS( ig, 1 )
            END IF
          END IF
        END DO
      ELSE

!  there are no constraints, so we need not check data%KNDOFC( ig )

        DO ig = 1, data%ng
          IF ( data%GXEQX( ig ) ) THEN
            f = f + data%GSCALE( ig ) * work%FT( ig )
          ELSE
            f = f + data%GSCALE( ig ) * work%GVALS( ig, 1 )
          END IF
        END DO
      END IF

!  evaluate the group derivative values

      IF ( grad ) THEN
        IF ( .NOT. data%altriv ) THEN
          CALL GROUP_r( work%GVALS, data%ng, work%FT, data%GPVALU, icnt,       &
                        data%ITYPEG, data%ISTGP, work%ICALCF, data%ltypeg,     &
                        data%lstgp, data%lcalcf, data%lcalcg, data%lgpvlu,     &
                        .TRUE., igstat )
          IF ( igstat /= 0 ) GO TO 930
        END IF

!  consider the IG-th group

        DO ig = 1, data%ng
          icon = data%KNDOFC( ig )

!  consider only those groups in the objective function

          IF ( icon > 0 ) CYCLE
          ig1 = ig + 1
          istrgv = data%ISTAGV( ig )
          iendgv = data%ISTAGV( ig1 ) - 1
          nelow = data%ISTADG( ig )
          nelup = data%ISTADG( ig1 ) - 1

!  compute the first derivative of the group

          gi = data%GSCALE( ig )
          IF ( .NOT. data%GXEQX( ig ) ) gi = gi  * work%GVALS( ig, 2 )

!  the group has nonlinear elements

          IF ( nelow <= nelup ) THEN
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
                CALL RANGE_r( iel, .TRUE., work%FUVALS( k ), work%W_el,        &
                              nvarel, nin, data%ITYPEE( iel ), nin, nvarel )
!DIR$ IVDEP
                DO i = 1, nvarel
                  j = data%IELVAR( l )
                  work%W_ws( j ) = work%W_ws( j ) + scalee * work%W_el( i )
                  l = l + 1
                END DO
              ELSE

!  the iel-th element has no internal representation

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

!  include the contributions from only the first n variables

              IF ( ll <= n ) THEN
                IF ( work%ISWKSP( ll ) == 0 ) THEN
                  nnzg = nnzg + 1
                  work%ISWKSP( ll ) = nnzg
                  G_var( nnzg ) = ll
                  G_val( nnzg ) = gi * work%W_ws( ll )
                ELSE
                  G_val( work%ISWKSP( ll ) )                                   &
                    = G_val( work%ISWKSP( ll ) ) + gi * work%W_ws( ll )
                END IF
              END IF
            END DO

!  the group has only linear elements

          ELSE

!  allocate a gradient

!DIR$ IVDEP
            DO k = data%ISTADA( ig ), data%ISTADA( ig1 ) - 1
              ll = data%ICNA( k )

!  include the contributions from linear elements for only the first n
!  variables

              IF ( ll <= n ) THEN
                IF ( work%ISWKSP( ll ) == 0 ) THEN
                  nnzg = nnzg + 1
                  work%ISWKSP( ll ) = nnzg
                  G_var( nnzg ) = ll
                  G_val( nnzg ) = gi * data%A( k )
                ELSE
                  G_val( work%ISWKSP( ll ) )                                   &
                    = G_val( work%ISWKSP( ll ) ) + gi * data%A( k )
                END IF
              END IF
            END DO
          END IF
        END DO

        work%nbprod = 0
        work%ISWKSP( : data%n ) = 0
      END IF

!  update the counters for the report tool

      work%nc2of = work%nc2of + 1
      IF ( grad ) work%nc2og = work%nc2og + 1
      status = 0
      GO TO 990

!  unsuccessful returns

  930 CONTINUE
      IF ( data%out > 0 ) WRITE( data%out,                                     &
        "( ' ** SUBROUTINE COFSG: error flag raised during SIF evaluation' )" )
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_cofsg = work%time_cofsg + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_cofsg_threadsafe_r

      END SUBROUTINE CUTEST_cofsg_threadsafe_r


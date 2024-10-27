! THIS VERSION: CUTEST 2.3 - 2024-10-27 AT 08:40 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-  C U T E S T    C C F G _ C   S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 23rd October 2024

      SUBROUTINE CUTEST_ccfg_c_r( status, n, m, X, C, jtrans,                  &
                                  lcjac1, lcjac2, CJAC, grad )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lcjac1, lcjac2
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: C
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lcjac1 * lcjac2 ) :: CJAC
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: jtrans, grad

! ------------------------------------------------------------------------
!  compute the values of the constraint functions and their gradients
!  for constraints initially written in Standard Input Format (SIF).
!  The Jacobian must be stored in a dense format.
!  (Subroutine CCFSG performs the same calculations for a sparse Jacobian)

!  lcjac1, If jtrans may be both .TRUE. and .FALSE on different calls,
!  lcjac2  lcjac1 and lcjac2 should be at least max(m,n). If jtrans 
!          can only be .TRUE., lcjac1 should be at least n and lcjac2 at 
!          least m. If jtrans can only be .FALSE., lcjac1 should be at 
!          least m and lcjac2 at least n.

!  CJAC  is a one-dimensional array of dimension lcjac1 * lcjac2
!        which gives the value of the Jacobian matrix of the
!        constraint functions, or its transpose, evaluated at X.
!        If JTRANS is .TRUE., the i,j-th component of the array
!        will contain the 0-based i-th derivative of the o-based j-th 
!        constraint function. Otherwise, if JTRANS is .FALSE., the i,j-th
!        component of the array will contain the 0-based j-th derivative
!        of the 0-based i-th constraint function.
! ------------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, j, l
      LOGICAL :: jtrans_fortran, grad_fortran

      grad_fortran = grad

!  create 2D Jacobiab storage if needed

      IF ( grad_fortran ) THEN
        IF ( .NOT. CUTEST_work_global( 1 )%jacobian_2d_setup_complete ) THEN
          ALLOCATE( CUTEST_work_global( 1 )%J_2d( lcjac1, lcjac2 ),            &
                    STAT = status )
          IF ( status /= 0 ) RETURN
          CUTEST_work_global( 1 )%jacobian_2d_setup_complete = .TRUE.
        END IF
      END IF

!  set the values of the constraint and (if required) 2D Jacobian

      jtrans_fortran = jtrans
      CALL CUTEST_ccfg_r( status, n, m, X, C, jtrans_fortran, lcjac1, lcjac2,  &
                          CUTEST_work_global( 1 )%J_2d, grad_fortran )

!  transfer the 2D Jacobian array stored by columns to a 1D array stored by rows

      IF ( grad_fortran ) THEN
        l = 0
        IF ( jtrans_fortran ) THEN
          DO i = 1, n
            DO j = 1, m
              l = l + 1
              CJAC( l ) = CUTEST_work_global( 1 )%J_2d( i, j )
            END DO
          END DO
        ELSE
          DO i = 1, m
            DO j = 1, n
              l = l + 1
              CJAC( l ) = CUTEST_work_global( 1 )%J_2d( i, j )
            END DO
          END DO
        END IF
      END IF


      RETURN

!  end of subroutine CUTEST_ccfg_c_r

      END SUBROUTINE CUTEST_ccfg_c_r

!-*-*-*-*-*-  C U T E S T   C I N T _  C C F G    S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 29th December 2012

      SUBROUTINE CUTEST_Cint_ccfg_r( status, n, m, X, C, jtrans,               &
                                     lcjac1, lcjac2, CJAC, grad )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lcjac1, lcjac2
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: C
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lcjac1, lcjac2 ) :: CJAC
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: jtrans, grad

! ------------------------------------------------------------------------
!  compute the values of the constraint functions and their gradients
!  for constraints initially written in Standard Input Format (SIF).
!  The Jacobian must be stored in a dense format.
!  (Subroutine CCFSG performs the same calculations for a sparse Jacobian)

!  CJAC  is a two-dimensional array of dimension ( lcjac1, lcjac2 )
!        which gives the value of the Jacobian matrix of the
!        constraint functions, or its transpose, evaluated at X.
!        If JTRANS is .TRUE., the i,j-th component of the array
!        will contain the i-th derivative of the j-th constraint
!        function. Otherwise, if JTRANS is .FALSE., the i,j-th
!        component of the array will contain the j-th derivative
!        of the i-th constraint function.
! ------------------------------------------------------------------------

      LOGICAL :: jtrans_fortran, grad_fortran

      jtrans_fortran = jtrans
      grad_fortran = grad
      CALL CUTEST_ccfg_r( status, n, m, X, C, jtrans_fortran,                  &
                          lcjac1, lcjac2, CJAC, grad_fortran )

      RETURN

!  end of subroutine CUTEST_Cint_ccfg_r

      END SUBROUTINE CUTEST_Cint_ccfg_r

!-*-*-*-*-*-*-*-  C U T E S T    C C F G    S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 29th December 2012

      SUBROUTINE CUTEST_ccfg_r( status, n, m, X, C, jtrans,                    &
                                lcjac1, lcjac2, CJAC, grad )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lcjac1, lcjac2
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: C
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lcjac1, lcjac2 ) :: CJAC
      LOGICAL, INTENT( IN ) :: jtrans, grad

! ------------------------------------------------------------------------
!  compute the values of the constraint functions and their gradients
!  for constraints initially written in Standard Input Format (SIF).
!  The Jacobian must be stored in a dense format.
!  (Subroutine CCFSG performs the same calculations for a sparse Jacobian)

!  CJAC  is a two-dimensional array of dimension ( lcjac1, lcjac2 )
!        which gives the value of the Jacobian matrix of the
!        constraint functions, or its transpose, evaluated at X.
!        If JTRANS is .TRUE., the i,j-th component of the array
!        will contain the i-th derivative of the j-th constraint
!        function. Otherwise, if JTRANS is .FALSE., the i,j-th
!        component of the array will contain the j-th derivative
!        of the i-th constraint function.
! ------------------------------------------------------------------------

      CALL CUTEST_ccfg_threadsafe_r( CUTEST_data_global,                       &
                                     CUTEST_work_global( 1 ),                  &
                                     status, n, m, X, C,                       &
                                     jtrans, lcjac1, lcjac2, CJAC, grad )
      RETURN

!  end of subroutine CUTEST_ccfg_r

      END SUBROUTINE CUTEST_ccfg_r

!-*-*-*-  C U T E S T   C C F G _ t h r e a d e d   S U B R O U T I N E  -*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 29th December 2012

      SUBROUTINE CUTEST_ccfg_threaded_r( status, n, m, X, C, jtrans,           &
                                         lcjac1, lcjac2, CJAC, grad, thread )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lcjac1, lcjac2, thread
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: C
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lcjac1, lcjac2 ) :: CJAC
      LOGICAL, INTENT( IN ) :: jtrans, grad

! ------------------------------------------------------------------------
!  compute the values of the constraint functions and their gradients
!  for constraints initially written in Standard Input Format (SIF).
!  The Jacobian must be stored in a dense format.
!  (Subroutine CCFSG performs the same calculations for a sparse Jacobian)

!  CJAC  is a two-dimensional array of dimension ( lcjac1, lcjac2 )
!        which gives the value of the Jacobian matrix of the
!        constraint functions, or its transpose, evaluated at X.
!        If JTRANS is .TRUE., the i,j-th component of the array
!        will contain the i-th derivative of the j-th constraint
!        function. Otherwise, if JTRANS is .FALSE., the i,j-th
!        component of the array will contain the j-th derivative
!        of the i-th constraint function.
! ------------------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_ccfg_threadsafe_r( CUTEST_data_global,                       &
                                     CUTEST_work_global( thread ),             &
                                     status, n, m, X, C,                       &
                                     jtrans, lcjac1, lcjac2, CJAC, grad )
      RETURN

!  end of subroutine CUTEST_ccfg_threaded_r

      END SUBROUTINE CUTEST_ccfg_threaded_r

!-*-*-  C U T E S T   C C F G _ t h r e a d s a f e   S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Ingrid Bongartz and Nick Gould

!  History -
!   fortran 77 version originally released in CUTE, April 1992
!   fortran 2003 version released in CUTEst, 21st November 2012

      SUBROUTINE CUTEST_ccfg_threadsafe_r( data, work, status, n, m, X, C,     &
                                           jtrans, lcjac1, lcjac2, CJAC, grad )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lcjac1, lcjac2
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: C
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lcjac1, lcjac2 ) :: CJAC
      LOGICAL, INTENT( IN ) :: jtrans, grad

! ------------------------------------------------------------------------
!  compute the values of the constraint functions and their gradients
!  for constraints initially written in Standard Input Format (SIF).
!  The Jacobian must be stored in a dense format.
!  (Subroutine CCFSG performs the same calculations for a sparse Jacobian)

!  CJAC  is a two-dimensional array of dimension ( lcjac1, lcjac2 )
!        which gives the value of the Jacobian matrix of the
!        constraint functions, or its transpose, evaluated at X.
!        If JTRANS is .TRUE., the i,j-th component of the array
!        will contain the i-th derivative of the j-th constraint
!        function. Otherwise, if JTRANS is .FALSE., the i,j-th
!        component of the array will contain the j-th derivative
!        of the i-th constraint function.
! ------------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, j, iel, k, ig, ii, ig1, l, ll, icon
      INTEGER ( KIND = ip_ ) :: nin, nvarel, nelow
      INTEGER ( KIND = ip_ ) :: icnt, ifstat, igstat, nelup, istrgv, iendgv
      REAL ( KIND = rp_ ) :: ftt, gi, scalee
      REAL :: time_in, time_out

      IF ( work%record_times ) CALL CPU_TIME( time_in )

      IF ( data%numcon == 0 ) GO TO 990

!  check input parameters

      IF ( grad ) THEN
        IF ( jtrans ) THEN
          IF ( lcjac1 < n .OR. lcjac2 < m ) THEN
            IF ( lcjac1 < n .AND. data%out > 0 ) WRITE( data%out, 2000 )
            IF ( lcjac2 < m .AND. data%out > 0 ) WRITE( data%out, 2010 )
            status = 2 ; GO TO 990
          END IF
        ELSE
          IF ( lcjac1 < m .OR. lcjac2 < n ) THEN
            IF ( lcjac1 < m .AND. data%out > 0 ) WRITE( data%out, 2000 )
            IF ( lcjac2 < n .AND. data%out > 0 ) WRITE( data%out, 2010 )
            status = 2 ; GO TO 990
          END IF
        END IF
      END IF

!  identify which elements are included in constraints. Use logical work
!  vector to keep track of elements already included

      work%LOGIC( 1 : data%nel ) = .FALSE.

!  Now identify elements in first m constraint groups

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

      DO ig = 1, data%ng
         ftt = 0.0_rp_

!  consider only those groups in the constraints

         icon = data%KNDOFC( ig )
         IF ( icon > 0 .AND. icon <= m ) THEN
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

!  compute the group function values

!  all group functions are trivial

      IF ( data%altriv ) THEN
        work%GVALS( : data%ng, 1 ) = work%FT( : data%ng )
        work%GVALS( : data%ng, 2 ) = 1.0_rp_

!  evaluate the group function values. Evaluate groups belonging to the first
!  m constraints only

      ELSE
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

!  increment the constraint function evaluation counter

      work%nc2cf = work%nc2cf + work%pnc

!  increment the constraint gradient evaluation counter

      IF ( grad ) THEN
        work%nc2cg = work%nc2cg + work%pnc

!  evaluate the group derivative values

        IF ( .NOT. data%altriv ) THEN
          CALL GROUP_r( work%GVALS, data%ng, work%FT, data%GPVALU, icnt,       &
                        data%ITYPEG, data%ISTGP, work%ICALCF, data%ltypeg,     &
                        data%lstgp, data%lcalcf, data%lcalcg, data%lgpvlu,     &
                        .TRUE., igstat )
          IF ( igstat /= 0 ) GO TO 930
        END IF

!  compute the gradient values.  Initialize the Jacobian as zero

        IF ( JTRANS ) THEN
           CJAC( 1 : n, 1 : m ) = 0.0_rp_
        ELSE
           CJAC( 1 : m, 1 : n ) = 0.0_rp_
        END IF

!  consider the ig-th group

        DO ig = 1, data%ng
          icon = data%KNDOFC( ig )

!  consider only those groups in the first m constraints

          IF ( icon == 0 .OR. icon > m ) CYCLE
          ig1 = ig + 1
          istrgv = data%ISTAGV( ig )
          iendgv = data%ISTAGV( ig1 ) - 1
          nelow = data%ISTADG( ig ) ; nelup = data%ISTADG( ig1 ) - 1

!  compute the first derivative of the group

          gi = data%GSCALE( ig )
          IF ( .NOT. data%GXEQX( ig ) ) gi = gi  * work%GVALS( ig, 2 )

!  the group has nonlinear elements

          IF ( nelow <= nelup ) THEN
            work%W_ws( data%ISVGRP( istrgv : iendgv ) ) = 0.0_rp_

!  loop over the group's nonlinear elements

            DO ii = nelow, nelup
              iel = data%IELING( ii )
              k = data%INTVAR( iel ) ; l = data%ISTAEV( iel )
              nvarel = data%ISTAEV( iel + 1 ) - l
              scalee = data%ESCALE( ii )

!  the iel-th element has an internal representation

              IF ( data%INTREP( iel ) ) THEN
                nin = data%INTVAR( iel + 1 ) - k
                CALL RANGE_r( iel, .TRUE., work%FUVALS( k ), work%W_el,        &
                              nvarel, nin, data%ITYPEE( iel ), nin, nvarel )
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

!  include contributions from the first n variables only

              IF ( ll <= n ) THEN
                IF ( jtrans ) THEN
                  CJAC( ll, icon ) = gi * work%W_ws( ll )
                ELSE
                  CJAC( icon, ll ) = gi * work%W_ws( ll )
                END IF
              END IF
            END DO

!  the group has only linear elements

          ELSE

!  allocate a gradient

!DIR$ IVDEP
            DO k = data%ISTADA( ig ), data%ISTADA( ig1 ) - 1
              ll = data%ICNA( k )

!  include contributions from the first n variables only

              IF ( ll <= n ) THEN
                IF ( JTRANS ) THEN
                  CJAC( ll, icon ) = gi * data%A( k )
                ELSE
                  CJAC( icon, ll ) = gi * data%A( k )
                END IF
              END IF
            END DO
          END IF
        END DO
      END IF
      status = 0
      GO TO 990

!  unsuccessful returns

  930 CONTINUE
      IF ( data%out > 0 ) WRITE( data%out,                                     &
        "( ' ** SUBROUTINE CCFG: error flag raised during SIF evaluation' )" )
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_ccfg = work%time_ccfg + time_out - time_in
      END IF
      RETURN

!  non-executable statements

 2000 FORMAT( ' ** SUBROUTINE CCFG: Increase the leading dimension of CJAC' )
 2010 FORMAT( ' ** SUBROUTINE CCFG: Increase the second dimension of CJAC' )

!  end of subroutine CUTEST_ccfg_threadsafe_r

      END SUBROUTINE CUTEST_ccfg_threadsafe_r


! THIS VERSION: CUTEST 2.3 - 2024-10-27 AT 09:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-*-  C U T E S T    C G R _ C   S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 20th October 2024

      SUBROUTINE CUTEST_cgr_c_r( status, n, m, X, Y, grlagf, G, jtrans,        &
                                 lj1, lj2, J_val )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lj1, lj2
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: grlagf, jtrans
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lj1 * lj2 ) :: J_val

!  ----------------------------------------------------------------------
!  compute both the gradients of the objective, or Lagrangian, and
!  general constraint functions of a problem initially written in
!  Standard Input Format (SIF).

!  G	 is an array which gives the value of the gradient of the
!	 objective function evaluated at X (grlagf = .FALSE.) or of
!        the Lagrangian function evaluated at X and Y (grlagf = .TRUE.)

!  lj1,  If jtrans may be both .TRUE. and .FALSE on different calls,
!  lj2   lj1 and lj2 should be at least max(m,n). If jtrans can only be
!        .TRUE., lj1 should be at least n and lj2 at least m. If jtrans
!        can only be .FALSE., lj1 should be at least m and lj2 at least n.

!  J_val is a one-dimensional array of dimension lj1 * lj2
!	 which gives the value of the Jacobian matrix of the
!	 constraint functions, or its transpose, evaluated at X.
!	 If jtrans is .TRUE., the i,j-th component of the array
!        will contain the i-th 0-based derivative of the j-th 0-based 
!        constraint function. Otherwise, if jtrans is .FALSE., the i,j-th
!        component of the array will contain the 0-based j-th derivative
!        of the i-th 0-based constraint function.
!  -----------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, j, l
      LOGICAL :: grlagf_fortran, jtrans_fortran

!  create 2D Jacobiab storage if needed

      IF ( .NOT. CUTEST_work_global( 1 )%jacobian_2d_setup_complete ) THEN
        ALLOCATE( CUTEST_work_global( 1 )%J_2d( lj1, lj2 ), STAT = status )
        IF ( status /= 0 ) RETURN
        CUTEST_work_global( 1 )%jacobian_2d_setup_complete = .TRUE.
      END IF

      grlagf_fortran = grlagf
      jtrans_fortran = jtrans
      CALL CUTEST_cgr_r( status, n, m, X, Y, grlagf_fortran, G,                &
                         jtrans_fortran, lj1, lj2,                             &
                         CUTEST_work_global( 1 )%J_2d )

!  transfer the 2D Jacobian array stored by columns to a 1D array stored by rows

      l = 0
      IF ( jtrans_fortran ) THEN
        DO i = 1, n
          DO j = 1, m
            l = l + 1
            J_val( l ) = CUTEST_work_global( 1 )%J_2d( i, j )
          END DO
        END DO
      ELSE
        DO i = 1, m
          DO j = 1, n
            l = l + 1
            J_val( l ) = CUTEST_work_global( 1 )%J_2d( i, j )
          END DO
        END DO
      END IF

      RETURN

!  end of subroutine CUTEST_cgr_c_r

      END SUBROUTINE CUTEST_cgr_c_r

!-*-*-*-*-*-  C U T E S T   C I N T _  C G R    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 21st August 2013

      SUBROUTINE CUTEST_Cint_cgr_r( status, n, m, X, Y, grlagf, G, jtrans,     &
                                    lj1, lj2, J_val )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lj1, lj2
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: grlagf, jtrans
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lj1, lj2 ) :: J_val

!  ----------------------------------------------------------------
!  compute both the gradients of the objective, or Lagrangian, and
!  general constraint functions of a problem initially written in
!  Standard Input Format (SIF).

!  G	 is an array which gives the value of the gradient of the
!	 objective function evaluated at X (grlagf = .FALSE.) or of
!        the Lagrangian function evaluated at X and Y (grlagf = .TRUE.)

!  J_val is a two-dimensional array of dimension (lj1, lj2)
!	 which gives the value of the Jacobian matrix of the
!	 constraint functions, or its transpose, evaluated at X.
!	 If jtrans is .TRUE., the i,j-th component of the array
!        will contain the i-th derivative of the j-th constraint
!        function. Otherwise, if jtrans is .FALSE., the i,j-th
!        component of the array will contain the j-th derivative
!        of the i-th constraint function.
!  ----------------------------------------------------------------

      LOGICAL :: grlagf_fortran, jtrans_fortran

      grlagf_fortran = grlagf
      jtrans_fortran = jtrans
      CALL CUTEST_cgr_r( status, n, m, X, Y, grlagf_fortran, G,                &
                         jtrans_fortran, lj1, lj2, J_val )

      RETURN

!  end of subroutine CUTEST_Cint_cgr_r

      END SUBROUTINE CUTEST_Cint_cgr_r

!-*-*-*-*-*-*-*-  C U T E S T    C G R    S U B R O U T I N E  -*-*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th December 2012

      SUBROUTINE CUTEST_cgr_r( status, n, m, X, Y, grlagf, G, jtrans,          &
                               lj1, lj2, J_val )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lj1, lj2
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL, INTENT( IN ) :: grlagf, jtrans
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lj1, lj2 ) :: J_val

!  ----------------------------------------------------------------
!  compute both the gradients of the objective, or Lagrangian, and
!  general constraint functions of a problem initially written in
!  Standard Input Format (SIF).

!  G	 is an array which gives the value of the gradient of the
!	 objective function evaluated at X (grlagf = .FALSE.) or of
!        the Lagrangian function evaluated at X and Y (grlagf = .TRUE.)

!  J_val is a two-dimensional array of dimension ( lj1, lj2 )
!	 which gives the value of the Jacobian matrix of the
!	 constraint functions, or its transpose, evaluated at X.
!	 If jtrans is .TRUE., the i,j-th component of the array
!        will contain the i-th derivative of the j-th constraint
!        function. Otherwise, if jtrans is .FALSE., the i,j-th
!        component of the array will contain the j-th derivative
!        of the i-th constraint function.
!  ----------------------------------------------------------------

      CALL CUTEST_cgr_threadsafe_r( CUTEST_data_global,                        &
                                    CUTEST_work_global( 1 ),                   &
                                    status, n, m, X, Y, grlagf, G, jtrans,     &
                                    lj1, lj2, J_val )
      RETURN

!  end of subroutine CUTEST_cgr_r

      END SUBROUTINE CUTEST_cgr_r

!-*-*-*-  C U T E S T    C G R _ t h r e a d e d   S U B R O U T I N E  -*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th December 2012

      SUBROUTINE CUTEST_cgr_threaded_r( status, n, m, X, Y, grlagf, G, jtrans, &
                                        lj1, lj2, J_val, thread )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lj1, lj2, thread
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL, INTENT( IN ) :: grlagf, jtrans
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lj1, lj2 ) :: J_val

!  ----------------------------------------------------------------
!  compute both the gradients of the objective, or Lagrangian, and
!  general constraint functions of a problem initially written in
!  Standard Input Format (SIF).

!  G	 is an array which gives the value of the gradient of the
!	 objective function evaluated at X (grlagf = .FALSE.) or of
!        the Lagrangian function evaluated at X and Y (grlagf = .TRUE.)

!  J_val is a two-dimensional array of dimension ( lj1, lj2 )
!	 which gives the value of the Jacobian matrix of the
!	 constraint functions, or its transpose, evaluated at X.
!	 If jtrans is .TRUE., the i,j-th component of the array
!        will contain the i-th derivative of the j-th constraint
!        function. Otherwise, if jtrans is .FALSE., the i,j-th
!        component of the array will contain the j-th derivative
!        of the i-th constraint function.
!  ----------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_cgr_threadsafe_r( CUTEST_data_global,                        &
                                    CUTEST_work_global( thread ),              &
                                    status, n, m, X, Y, grlagf, G, jtrans,     &
                                    lj1, lj2, J_val )
      RETURN

!  end of subroutine CUTEST_cgr_threaded_r

      END SUBROUTINE CUTEST_cgr_threaded_r

!-*-*-  C U T E S T    C G R _ t h r e a d s a f e   S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTE, November 1991
!   fortran 2003 version released in CUTEst, 20th November 2012

      SUBROUTINE CUTEST_cgr_threadsafe_r( data, work, status, n, m, X, Y,      &
                                          grlagf, G, jtrans, lj1, lj2, J_val )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lj1, lj2
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL, INTENT( IN ) :: grlagf, jtrans
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lj1, lj2 ) :: J_val

!  ----------------------------------------------------------------
!  compute both the gradients of the objective, or Lagrangian, and
!  general constraint functions of a problem initially written in
!  Standard Input Format (SIF).

!  G	 is an array which gives the value of the gradient of the
!	 objective function evaluated at X (grlagf = .FALSE.) or of
!        the Lagrangian function evaluated at X and Y (grlagf = .TRUE.)

!  J_val	 is a two-dimensional array of dimension ( lj1, lj2 )
!	 which gives the value of the Jacobian matrix of the
!	 constraint functions, or its transpose, evaluated at X.
!	 If jtrans is .TRUE., the i,j-th component of the array
!        will contain the i-th derivative of the j-th constraint
!        function. Otherwise, if jtrans is .FALSE., the i,j-th
!        component of the array will contain the j-th derivative
!        of the i-th constraint function.
!  ----------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, j, icon, iel, k, ig, ii, ig1, l, jj, ll
      INTEGER ( KIND = ip_ ) :: istrgv, iendgv, ifstat, igstat
      INTEGER ( KIND = ip_ ) :: nelow, nelup,  nin, nvarel
      LOGICAL :: nontrv
      REAL ( KIND = rp_ ) :: ftt, gi, scalee, gii
      REAL :: time_in, time_out
      EXTERNAL :: RANGE_r

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  Check input parameters.

!  dimension-checking.

      IF ( data%numcon > 0 ) THEN
        IF ( JTRANS ) THEN
          IF ( lj1 < n .OR. lj2 < m ) THEN
            IF ( lj1 < n .AND. data%out > 0 ) WRITE( data%out, 2000 )
            IF ( lj2 < m .AND. data%out > 0 ) WRITE( data%out, 2010 )
              status = 2 ; GO TO 990
          END IF
        ELSE
          IF ( lj1 < m .OR. lj2 < n ) THEN
            IF ( lj1 < m .AND. data%out > 0 ) WRITE( data%out, 2000 )
            IF ( lj2 < n .AND. data%out > 0 ) WRITE( data%out, 2010 )
              status = 2 ; GO TO 990
         END IF
        END IF
      END IF

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

! evaluate the element function derivatives

      CALL ELFUN_r( work%FUVALS, X, data%EPVALU, data%nel, data%ITYPEE,        &
                    data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,        &
                    data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,         &
                    data%lelvar, data%lntvar, data%lstadh, data%lstep,         &
                    data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,        &
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

!  for unconstrained problems, skip construction of gradient and Jacobian.
!  Call ELGRD instead

      IF ( data%numcon > 0 ) THEN

!  compute the gradient values. Initialize the gradient and Jacobian (or its
!  transpose) as zero

        G( : n ) = 0.0_rp_
        IF ( jtrans ) THEN
          J_val( : n, : m ) = 0.0_rp_
        ELSE
          J_val( : m, : n ) = 0.0_rp_
        END IF

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
            IF ( grlagf ) gii = gi * Y( data%KNDOFC( ig ) )
          END IF
          IF ( nontrv ) THEN
            gi = gi  * work%GVALS( ig, 2 )
            IF ( grlagf ) gii = gii * work%GVALS( ig, 2 )
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
                CALL RANGE_r( iel, .TRUE., work%FUVALS( k ),                   &
                              work%W_el, nvarel, nin, data%ITYPEE( iel ),      &
                              nin, nvarel )
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

!  the group belongs to the objective function

              IF ( icon == 0 ) THEN
                G( ll ) = G( ll ) + gi * work%W_ws( ll )

!  the group defines a constraint

              ELSE
                IF ( jtrans ) THEN
                  J_val( ll, icon ) =   gi * work%W_ws( ll )
                ELSE
                  J_val( icon, ll ) =   gi * work%W_ws( ll )
                END IF
                IF ( grlagf ) G( ll ) = G( ll ) + gii * work%W_ws( ll )
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
                IF ( jtrans ) THEN
                  J_val( ll, icon ) = gi * data%A( k )
                ELSE
                  J_val( icon, ll ) = gi * data%A( k )
                END IF
                IF ( grlagf ) G( ll ) = G( ll ) + gii * data%A( k )
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
      ELSE

!  compute the gradient value

        CALL CUTEST_form_gradients( n, data%ng, data%nel, data%ntotel,         &
               data%nvrels, data%nnza, data%nvargp, work%firstg, data%ICNA,    &
               data%ISTADA, data%IELING, data%ISTADG, data%ISTAEV,             &
               data%IELVAR, data%INTVAR, data%A, work%GVALS( : , 2 ),          &
               work%FUVALS, data%lnguvl, work%FUVALS( data%lggfx + 1 ),        &
               data%GSCALE, data%ESCALE, work%FUVALS( data%lgrjac + 1 ),       &
               data%GXEQX, data%INTREP, data%ISVGRP, data%ISTAGV, data%ITYPEE, &
               work%ISTAJC, work%W_ws, work%W_el, RANGE_r )

!  store the gradient value

        DO i = 1, n
          G( i ) = work%FUVALS( data%lggfx + i )
        END DO
      END IF
      work%firstg = .FALSE.

!  update the counters for the report tool

      work%nc2og = work%nc2og + 1
      work%nc2cg = work%nc2cg + work%pnc
      status = 0
      GO TO 990

!  unsuccessful returns

  930 CONTINUE
      IF ( data%out > 0 ) WRITE( data%out,                                     &
        "( ' ** SUBROUTINE CGR: error flag raised during SIF evaluation' )" )
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_cgr = work%time_cgr + time_out - time_in
      END IF
      RETURN

!  non-executable statements

 2000 FORMAT( ' ** SUBROUTINE CGR: Increase the leading dimension of J_val' )
 2010 FORMAT( ' ** SUBROUTINE CGR: Increase the second dimension of J_val' )

!  end of subroutine CUTEST_cgr_threadsafe_r

      END SUBROUTINE CUTEST_cgr_threadsafe_r

! THIS VERSION: CUTEST 2.3 - 2024-10-22 AT 11:20 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-  C U T E S T    C O H P R O D S _ C   S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 22nd October 2024

      SUBROUTINE CUTEST_cohprods_c_r( status, n, goth, X, VECTOR,              &
                                      nnzohp, lohp, RESULT, IND )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lohp
      INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: nnzohp
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: goth
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( lohp ) :: IND
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lohp ) :: RESULT

!  ---------------------------------------------------------------------------
!  compute the matrix-vector product H(x) between the Hessian matrices H(x)
!  of the objective function for the problem and a given vector v stored in
!  VECTOR. The nonzero entries of the resulting products H(x) v and their
!  0-based indices occur in RESULT(k),IND(k), k = 1,...,nnzohp. If goth is 
!  .TRUE., the second derivatives are assumed to have already been computed,
!  and that nnzohp and IND have been set by a previous call at X.
!  If the user is unsure, set goth = .FALSE. the first time a product is
!  required with the Hessians evaluated at X. X is not used if goth = .TRUE.
!  ---------------------------------------------------------------------------

      LOGICAL :: goth_fortran

      goth_fortran = goth
      IF ( goth_fortran ) IND( : nnzohp ) = IND( : nnzohp ) + 1

      CALL CUTEST_cohprods_r( status, n, goth_fortran, X, VECTOR,              &
                               nnzohp, lohp, RESULT, IND )

      IND( : nnzohp ) = IND( : nnzohp ) - 1

      RETURN

!  end of subroutine CUTEST_cohprods_c_r

      END SUBROUTINE CUTEST_cohprods_c_r

!-*-*-  C U T E S T   C I N T _ C O H P R O D S    S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   moden fortran version released in CUTEst, 29th October 2023

      SUBROUTINE CUTEST_Cint_cohprods_r( status, n, goth, X, VECTOR,           &
                                         nnzohp, lohp, RESULT, IND )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lohp
      INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: nnzohp
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: goth
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( lohp ) :: IND
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lohp ) :: RESULT

!  ---------------------------------------------------------------------------
!  compute the matrix-vector product H(x) between the Hessian matrices H(x)
!  of the objective function for the problem and a given vector v stored in
!  VECTOR. The nonzero entries of the resulting products H(x) v and their
!  indices occur in RESULT(k),IND(k), k = 1,...,nnzohp. If goth is .TRUE.
!  the second derivatives are assumed to have already been computed, and
!  that nnzohp and IND have been set by a previous call at X.
!  If the user is unsure, set goth = .FALSE. the first time a product is
!  required with the Hessians evaluated at X. X is not used if goth = .TRUE.
!  ---------------------------------------------------------------------------

      LOGICAL :: goth_fortran

      goth_fortran = goth
      CALL CUTEST_cohprods_r( status, n, goth_fortran, X, VECTOR,              &
                               nnzohp, lohp, RESULT, IND )
      RETURN

!  end of subroutine CUTEST_Cint_cohprods_r

      END SUBROUTINE CUTEST_Cint_cohprods_r

!-*-*-*-*-*-  C U T E S T    C O H P R O D S    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   moden fortran version released in CUTEst, 29th October 2023

      SUBROUTINE CUTEST_cohprods_r( status, n, goth, X, VECTOR,                &
                                    nnzohp, lohp, RESULT, IND )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lohp
      INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: nnzohp
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL, INTENT( IN ) :: goth
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lohp ) :: IND
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lohp ) :: RESULT

!  ---------------------------------------------------------------------------
!  compute the matrix-vector product H(x) between the Hessian matrices H(x)
!  of the objective function for the problem and a given vector v stored in
!  VECTOR. The nonzero entries of the resulting products H(x) v and their
!  indices occur in RESULT(k),IND(k), k = 1,...,nnzohp. If goth is .TRUE.
!  the second derivatives are assumed to have already been computed, and
!  that nnzohp and IND have been set by a previous call at X.
!  If the user is unsure, set goth = .FALSE. the first time a product is
!  required with the Hessians evaluated at X. X is not used if goth = .TRUE.
!  ---------------------------------------------------------------------------

      CALL CUTEST_cohprods_threadsafe_r( CUTEST_data_global,                   &
                                         CUTEST_work_global( 1 ),              &
                                         status, n, goth, X, VECTOR,           &
                                         nnzohp, lohp, RESULT, IND )
      RETURN

!  end of subroutine CUTEST_cohprods_r

      END SUBROUTINE CUTEST_cohprods_r

!-*-  C U T E S T   C O H P R O D S _ t h r e a d e d   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   moden fortran version released in CUTEst, 29th October 2023

      SUBROUTINE CUTEST_cohprods_threaded_r( status, n, goth, X, VECTOR,       &
                                             nnzohp, lohp, RESULT, IND, thread )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lohp, thread
      INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: nnzohp
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL, INTENT( IN ) :: goth
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lohp ) :: IND
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lohp ) :: RESULT

!  ---------------------------------------------------------------------------
!  compute the matrix-vector product H(x) between the Hessian matrices H(x)
!  of the objective function for the problem and a given vector v stored in
!  VECTOR. The nonzero entries of the resulting products H(x) v and their
!  indices occur in RESULT(k),IND(k), k = 1,...,nnzohp. If goth is .TRUE.
!  the second derivatives are assumed to have already been computed, and
!  that nnzohp and IND have been set by a previous call at X.
!  If the user is unsure, set goth = .FALSE. the first time a product is
!  required with the Hessians evaluated at X. X is not used if goth = .TRUE.
!  ---------------------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_cohprods_threadsafe_r( CUTEST_data_global,                   &
                                         CUTEST_work_global( thread ),         &
                                         status, n, goth, X, VECTOR,           &
                                         nnzohp, lohp, RESULT, IND )
      RETURN

!  end of subroutine CUTEST_cohprods_threaded_r

      END SUBROUTINE CUTEST_cohprods_threaded_r

!-  C U T E S T   C O H P R O D S _ t h r e a d s a f e   S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Nick Gould

!  History -
!   moden fortran version released in CUTEst, 29th October 2023

      SUBROUTINE CUTEST_cohprods_threadsafe_r( data, work, status, n, goth,    &
                                               X, VECTOR, nnzohp, lohp,        &
                                               RESULT, IND )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lohp
      INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: nnzohp
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL, INTENT( IN ) :: goth
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lohp ) :: IND
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lohp ) :: RESULT

!  ---------------------------------------------------------------------------
!  compute the matrix-vector product H(x) between the Hessian matrices H(x)
!  of the objective function for the problem and a given vector v stored in
!  VECTOR. The nonzero entries of the resulting products H(x) v and their
!  indices occur in RESULT(k),IND(k), k = 1,...,nnzohp. If goth is .TRUE.
!  the second derivatives are assumed to have already been computed, and
!  that nnzohp and IND have been set by a previous call at X.
!  If the user is unsure, set goth = .FALSE. the first time a product is
!  required with the Hessians evaluated at X. X is not used if goth = .TRUE.
!  ---------------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, iel, iell, ielhst, ifstat, igstat
      INTEGER ( KIND = ip_ ) :: ig, ii, irow, lthvar, nvarel, nin
      INTEGER ( KIND = ip_ ) :: ijhess, j, jcol, k, l, ll
      REAL ( KIND = rp_ ) :: ftt, gdash, g2dash, gi, pi, prod, scalee
      REAL :: time_in, time_out
      LOGICAL :: nullwk
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

!  compute the gradient value

        CALL CUTEST_form_gradients( n, data%ng, data%nel, data%ntotel,         &
               data%nvrels, data%nnza, data%nvargp, work%firstg, data%ICNA,    &
               data%ISTADA, data%IELING, data%ISTADG, data%ISTAEV,             &
               data%IELVAR, data%INTVAR, data%A, work%GVALS( : , 2 ),          &
               work%FUVALS, data%lnguvl, work%FUVALS( data%lggfx + 1 ),        &
               data%GSCALE, data%ESCALE, work%FUVALS( data%lgrjac + 1 ),       &
               data%GXEQX, data%INTREP, data%ISVGRP, data%ISTAGV,              &
               data%ITYPEE, work%ISTAJC, work%W_ws, work%W_el, RANGE_r )

!  set the indices for the nonzeros for the objective Hessian product

        CALL CUTEST_cohprodsp_r( status, nnzohp, lohp, IND )
        work%nnzohp = nnzohp
      ELSE
        nnzohp = work%nnzohp
      END IF

!  reset IUSED and W_ws to zero

      work%IUSED( IND( : nnzohp ) ) = 0
      work%W_ws( IND( : nnzohp ) ) = 0.0_rp_
      work%G_temp( IND( : nnzohp ) ) = 0.0_rp_

!  loop over all the groups, but ignore those that are constraints

      DO ig = 1, data%ng
        IF ( data%KNDOFC( ig ) /= 0 ) CYCLE
        IF (  data%GSCALE( ig ) == 0.0_rp_ ) CYCLE

!  store the nonzeros of the Hessian-vector product in W_ws

!  =========================== rank-one terms ============================

!  if the ig-th group is non-trivial, form the product of VECTOR with the
!  first order term, grad h_ig * g''(h_ig) * grad(trans) h_ig, where
!  h_ig is the ig-th group function

!  form the gradient of the ig-th group, grad h_ig

        g2dash = data%GSCALE( ig ) * work%GVALS( ig, 3 )
        IF ( .NOT. data%GXEQX( ig ) .AND. g2dash /= 0.0_rp_ ) THEN

!  consider any nonlinear elements for the group

          DO iell = data%ISTADG( ig ), data%ISTADG( ig + 1 ) - 1
            iel = data%IELING( iell )
            k = data%INTVAR( iel )
            l = data%ISTAEV( iel )
            nvarel = data%ISTAEV( iel + 1 ) - l
            scalee = data%ESCALE( iell )

!  the iel-th element has an internal representation

            IF ( data%INTREP( iel ) ) THEN
              nin = data%INTVAR( iel + 1 ) - k
              CALL RANGE_r( iel, .TRUE., work%FUVALS( k : k + nin - 1 ),       &
                          work%H_el, nvarel, nin, data%ITYPEE( iel ),          &
                          nin, nvarel )
              DO i = 1, nvarel
                j = data%IELVAR( l )
                work%G_temp( j ) = work%G_temp( j ) + scalee * work%H_el( i )
                l = l + 1
              END DO

!  the iel-th element has no internal representation

            ELSE
              DO i = 1, nvarel
                j = data%IELVAR( l )
                work%G_temp( j )                                               &
                  = work%G_temp( j ) + scalee * work%FUVALS( k )
                k = k + 1 ; l = l + 1
              END DO
            END IF
          END DO

!  include the contribution from the linear element

          DO k = data%ISTADA( ig ), data%ISTADA( ig + 1 ) - 1
            j = data%ICNA( k )
            work%G_temp( j ) = work%G_temp( j ) + data%A( k )
          END DO

!  form the inner product of the group gradient, grad h_ig, with VECTOR

          prod = 0.0_rp_
          DO l = data%ISTAGV( ig ), data%ISTAGV( ig + 1 ) - 1
            j = data%ISVGRP( l )
            prod = prod + work%G_temp( j ) * VECTOR( j )
          END DO

          IF ( prod /= 0.0_rp_ ) THEN

!  form g''(h_ig) * grad^T h_ig VECTOR

            prod = prod * g2dash

!  form the scaled group gradient
!    grad h_ig * ( g''(h_ig) * grad(trans) h_ig VECTOR )

            DO l = data%ISTAGV( ig ), data%ISTAGV( ig + 1 ) - 1
              j = data%ISVGRP( l )
              work%W_ws( j ) = work%W_ws( j ) + prod * work%G_temp( j )

!  reset the nonzeros of g_temp to zero

              work%G_temp( j ) = 0.0_rp_
            END DO
          ELSE
             work%G_temp( data%ISVGRP( data%ISTAGV( ig ) :                    &
                                         data%ISTAGV( ig + 1 ) - 1 ) ) = 0.0_rp_
          END IF
        END IF

!  ======================= second-order terms =======================

!  now consider the product of VECTOR with the second order term
!  g'(h_ig) * Hess h_ig

        IF ( data%GXEQX( ig ) ) THEN
          gdash = data%GSCALE( ig )
        ELSE
          gdash = data%GSCALE( ig ) * work%GVALS( ig, 2 )
        END IF

!  consider any nonlinear elements for the group

        DO iell = data%ISTADG( ig ), data%ISTADG( ig + 1 ) - 1
          iel = data%IELING( iell )
          nvarel = data%ISTAEV( iel + 1 ) - data%ISTAEV( iel )
          gi = data%ESCALE( iell ) * gdash

!  the iel-th element Hessian has an internal representation. Copy the
!  elemental variables into W

          IF ( data%INTREP( iel ) ) THEN
            nullwk = .TRUE.
            ll = data%ISTAEV( iel )
!DIR$ IVDEP
            DO ii = 1, nvarel
              pi = VECTOR( data%IELVAR( ll ) )
              work%W_el( ii ) = pi
              IF ( pi /= 0.0_rp_ ) nullwk = .FALSE.
              ll = ll + 1
            END DO
            IF ( nullwk ) CYCLE

!  find the internal variables, W_in

            nin = data%INTVAR( iel + 1 ) - data%INTVAR( iel )
            CALL RANGE_r( iel, .FALSE., work%W_el, work%W_in, nvarel, nin,     &
                        data%ITYPEE( iel ), nvarel, nin )

!  multiply the internal variables by the element Hessian and put the
!  product in H_in. Consider the first column of the element Hessian

            ielhst = data%ISTADH( iel )
            pi = gi * work%W_in( 1 )
            work%H_in( : nin )                                                &
              = pi * work%FUVALS( data%ISYMMH( 1, : nin ) + ielhst )

!  now consider the remaining columns of the element Hessian

            DO jcol = 2, nin
              pi = gi * work%W_in( jcol )
              IF ( pi /= 0.0_rp_ ) THEN
                work%H_in( : nin ) = work%H_in( : nin ) +                      &
                  pi * work%FUVALS( data%ISYMMH( jcol, : nin ) + ielhst )
              END IF
            END DO

!  scatter the product back onto the elemental variables, W

            CALL RANGE_r( iel, .TRUE., work%H_in, work%W_el, nvarel, nin,      &
                        data%ITYPEE( iel ), nin, nvarel )

!  add the scattered product to Q

            ll = data%ISTAEV( iel )
!DIR$ IVDEP
            DO ii = 1, nvarel
              l = data%IELVAR( ll )
              work%W_ws( l ) = work%W_ws( l ) + work%W_el( ii )
              ll = ll + 1
            END DO

!  the iel-th element Hessian has no internal representation

          ELSE
            lthvar = data%ISTAEV( iel ) - 1
            ielhst = data%ISTADH( iel )
            DO jcol = 1, nvarel
              pi = gi * VECTOR( data%IELVAR( lthvar + jcol ) )
              IF ( pi /= 0.0_rp_ ) THEN
!DIR$ IVDEP
                DO irow = 1, nvarel
                  ijhess = data%ISYMMH( jcol, irow ) + ielhst
                  l = data%IELVAR( lthvar + irow )
                  work%W_ws( l ) = work%W_ws( l ) + pi * work%FUVALS( ijhess )
                END DO
              END IF
            END DO
          END IF
        END DO
      END DO

!  copy the nonzeros back to the sparse array, and reset W_ws to zero

      RESULT( 1 : nnzohp ) = work%W_ws( IND( 1 : nnzohp ) )
      work%W_ws( IND( 1 : nnzohp ) ) = 0.0_rp_

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
        "( ' ** SUBROUTINE COHPRODS: error flag raised during SIF evaluation')")
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_cohprods = work%time_cohprods + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_cohprods_threadsafe_r

      END SUBROUTINE CUTEST_cohprods_threadsafe_r

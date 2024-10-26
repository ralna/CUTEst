! THIS VERSION: CUTEST 2.3 - 2024-10-20 AT 11:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-  C U T E S T    C C H P R O D S _ C   S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 20th October 2024

      SUBROUTINE CUTEST_cchprods_c_r( status, n, m, goth, X, VECTOR,           &
                                      lchp, CHP_val, CHP_ind, CHP_ptr )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lchp
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: goth
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( m + 1 ) :: CHP_ptr
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( lchp ) :: CHP_ind
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lchp ) :: CHP_val

!  ---------------------------------------------------------------------------
!  compute the matrix-vector products H_i(x) v, i = 1, ..., m, between each of
!  the Hessian matrices H_i(x) of the constraint functions for the problem and
!  a given vector v stored in VECTOR. The nonzero entries of the resulting
!  products H_i(x) v and their 0-base indices occur in (CHP_val(k),CHP_ind), k =
!  CHP_ptr(i),..., CHP_ptr(i+1)-1, i = 1, ..., m. If goth is .TRUE. the second
!  derivatives, CHP_ind, and CHP_ptr are assumed to have already been computed.
!  If the user is unsure, set goth = .FALSE. the first time a product is
!  required with the Hessians evaluated at X. X is not used if goth = .TRUE.
!  ---------------------------------------------------------------------------

      LOGICAL :: goth_fortran

      goth_fortran = goth
      IF ( goth_fortran ) THEN
        CHP_ptr( : m + 1 ) = CHP_ptr( : m + 1 ) + 1
        CHP_ind( : CHP_ptr( m + 1 ) - 1 )                                      &
          = CHP_ind( : CHP_ptr( m + 1 ) - 1 ) + 1
      END IF

      CALL CUTEST_cchprods_r( status, n, m, goth_fortran, X, VECTOR, lchp,     &
                              CHP_val, CHP_ind, CHP_ptr )

      CHP_ind( : CHP_ptr( m + 1 ) - 1 ) = CHP_ind( : CHP_ptr( m + 1 ) - 1 ) - 1
      CHP_ptr( : m + 1 ) = CHP_ptr( : m + 1 ) - 1

      RETURN

!  end of subroutine CUTEST_cchprods_c_r

      END SUBROUTINE CUTEST_cchprods_c_r

!-*-*-  C U T E S T   C I N T _ C C H P R O D S    S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 24th November 2015

      SUBROUTINE CUTEST_Cint_cchprods_r( status, n, m, goth, X, VECTOR,        &
                                         lchp, CHP_val, CHP_ind, CHP_ptr )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lchp
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: goth
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( m + 1 ) :: CHP_ptr
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( lchp ) :: CHP_ind
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lchp ) :: CHP_val

!  ---------------------------------------------------------------------------
!  compute the matrix-vector products H_i(x) v, i = 1, ..., m, between each of
!  the Hessian matrices H_i(x) of the constraint functions for the problem and
!  a given vector v stored in VECTOR. The nonzero entries of the resulting
!  products H_i(x) v and their indices occur in (CHP_val(k),CHP_ind), k =
!  CHP_ptr(i),..., CHP_ptr(i+1)-1, i = 1, ..., m. If goth is .TRUE. the second
!  derivatives, CHP_ind, and CHP_ptr are assumed to have already been computed.
!  If the user is unsure, set goth = .FALSE. the first time a product is
!  required with the Hessians evaluated at X. X is not used if goth = .TRUE.
!  ---------------------------------------------------------------------------

      LOGICAL :: goth_fortran

      goth_fortran = goth
      CALL CUTEST_cchprods_r( status, n, m, goth_fortran, X, VECTOR, lchp,     &
                              CHP_val, CHP_ind, CHP_ptr )
      RETURN

!  end of subroutine CUTEST_Cint_cchprods_r

      END SUBROUTINE CUTEST_Cint_cchprods_r

!-*-*-*-*-*-  C U T E S T    C C H P R O D S    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 24th November 2015

      SUBROUTINE CUTEST_cchprods_r( status, n, m, goth, X, VECTOR,             &
                                    lchp, CHP_val, CHP_ind, CHP_ptr )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lchp
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL, INTENT( IN ) :: goth
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( m + 1 ) :: CHP_ptr
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( lchp ) :: CHP_ind
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lchp ) :: CHP_val

!  ---------------------------------------------------------------------------
!  compute the matrix-vector products H_i(x) v, i = 1, ..., m, between each of
!  the Hessian matrices H_i(x) of the constraint functions for the problem and
!  a given vector v stored in VECTOR. The nonzero entries of the resulting
!  products H_i(x) v and their indices occur in (CHP_val(k),CHP_ind), k =
!  CHP_ptr(i),..., CHP_ptr(i+1)-1, i = 1, ..., m. If goth is .TRUE. the second
!  derivatives, CHP_ind, and CHP_ptr are assumed to have already been computed.
!  If the user is unsure, set goth = .FALSE. the first time a product is
!  required with the Hessians evaluated at X. X is not used if goth = .TRUE.
!  ---------------------------------------------------------------------------

      CALL CUTEST_cchprods_threadsafe_r( CUTEST_data_global,                   &
                                         CUTEST_work_global( 1 ),              &
                                         status, n, m, goth, X, VECTOR, lchp,  &
                                         CHP_val, CHP_ind, CHP_ptr )
      RETURN

!  end of subroutine CUTEST_cchprods_r

      END SUBROUTINE CUTEST_cchprods_r

!-*-  C U T E S T   C C H P R O D S _ t h r e a d e d   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 24th November 2015

      SUBROUTINE CUTEST_cchprods_threaded_r( status, n, m, goth, X, VECTOR,    &
                                             lchp, CHP_val, CHP_ind, CHP_ptr,  &
                                             thread )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lchp, thread
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL, INTENT( IN ) :: goth
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( m + 1 ) :: CHP_ptr
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( lchp ) :: CHP_ind
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lchp ) :: CHP_val

!  ---------------------------------------------------------------------------
!  compute the matrix-vector products H_i(x) v, i = 1, ..., m, between each of
!  the Hessian matrices H_i(x) of the constraint functions for the problem and
!  a given vector v stored in VECTOR. The nonzero entries of the resulting
!  products H_i(x) v and their indices occur in (CHP_val(k),CHP_ind), k =
!  CHP_ptr(i),..., CHP_ptr(i+1)-1, i = 1, ..., m. If goth is .TRUE. the second
!  derivatives, CHP_ind, and CHP_ptr are assumed to have already been computed.
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

      CALL CUTEST_cchprods_threadsafe_r( CUTEST_data_global,                   &
                                         CUTEST_work_global( thread ),         &
                                         status, n, m, goth, X, VECTOR, lchp,  &
                                         CHP_val, CHP_ind, CHP_ptr )
      RETURN

!  end of subroutine CUTEST_cchprods_threaded_r

      END SUBROUTINE CUTEST_cchprods_threaded_r

!-  C U T E S T   C C H P R O D S _ t h r e a d s a f e   S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 24th November 2015

      SUBROUTINE CUTEST_cchprods_threadsafe_r( data, work, status, n, m, goth, &
                                               X, VECTOR, lchp, CHP_val,       &
                                               CHP_ind, CHP_ptr)
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lchp
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      LOGICAL, INTENT( IN ) :: goth
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( m + 1 ) :: CHP_ptr
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( lchp ) :: CHP_ind
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lchp ) :: CHP_val

!  ---------------------------------------------------------------------------
!  compute the matrix-vector products H_i(x) v, i = 1, ..., m, between each of
!  the Hessian matrices H_i(x) of the constraint functions for the problem and
!  a given vector v stored in VECTOR. The nonzero entries of the resulting
!  products H_i(x) v and their indices occur in (CHP_val(k),CHP_ind), k =
!  CHP_ptr(i),..., CHP_ptr(i+1)-1, i = 1, ..., m. If goth is .TRUE. the second
!  derivatives, CHP_ind, and CHP_ptr are assumed to have already been computed.
!  If the user is unsure, set goth = .FALSE. the first time a product is
!  required with the Hessians evaluated at X. X is not used if goth = .TRUE.
!  ---------------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, ic, iel, iell, ielhst, nvarel, nin
      INTEGER ( KIND = ip_ ) :: ifstat, igstat, ig, ii, irow
      INTEGER ( KIND = ip_ ) :: ijhess, j, jcol, k, l, ll, ls, lthvar
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

!  set the indices for the nonzeros for each constraint Hessian product in turn

        ls = 1
        DO ic = 1, m
          ig = data%CGROUP( ic )
          CHP_ptr( ic ) = ls
          DO k = data%ISTAGV( ig ), data%ISTAGV( ig + 1 ) - 1
            CHP_ind( ls ) = data%ISVGRP( k )
            ls = ls + 1
          END DO
        END DO
        CHP_ptr( m + 1 ) = ls
      END IF

!  loop over the constraints

      DO ic = 1, m

!  the i-th constraint is the ig-th group

        ig = data%CGROUP( ic )

!  store the nonzeros of the m-th Hessian-vector product in W_ws

!  =========================== rank-one terms ============================

!  if the ig-th group is non-trivial, form the product of VECTOR with the
!  first order term, grad h_ig * g''(h_ig) * grad(trans) h_ig, where
!  h_ig is the ig-th group function

!  form the gradient of the ig-th group, grad h_ig

        IF ( .NOT. data%GXEQX( ig ) .AND. data%GSCALE( ig ) /= 0.0_rp_ ) THEN
          g2dash = data%GSCALE( ig ) * work%GVALS( ig, 3 )

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
                            work%H_el, nvarel, nin, data%ITYPEE( iel ),        &
                            nin, nvarel )
              DO i = 1, nvarel
                j = data%IELVAR( l )
                work%W_ws( j ) = work%W_ws( j ) + scalee * work%H_el( i )
                l = l + 1
              END DO

!  the iel-th element has no internal representation

            ELSE
              DO i = 1, nvarel
                j = data%IELVAR( l )
                work%W_ws( j ) = work%W_ws( j ) + scalee * work%FUVALS( k )
                k = k + 1 ; l = l + 1
              END DO
            END IF
          END DO

!  include the contribution from the linear element

          DO k = data%ISTADA( ig ), data%ISTADA( ig + 1 ) - 1
            j = data%ICNA( k )
            work%W_ws( j ) = work%W_ws( j ) + data%A( k )
          END DO

!  form the inner product of the group gradient, grad h_ig, with VECTOR

          prod = 0.0_rp_
!         DO l = CHP_ptr( ic ), CHP_ptr( ic + 1 ) - 1
!           j = CHP_ind( l )
          DO l = data%ISTAGV( ig ), data%ISTAGV( ig + 1 ) - 1
            j = data%ISVGRP( l )
            prod = prod + work%W_ws( j ) * VECTOR( j )
          END DO

!  form g''(h_ig) * grad^T h_ig VECTOR

          prod = prod * g2dash

!  form the scaled group gradient
!    grad h_ig * ( g''(h_ig) * grad(trans) h_ig VECTOR )

!         DO l = CHP_ptr( ic ), CHP_ptr( ic + 1 ) - 1
!           j = CHP_ind( l )
          DO l = data%ISTAGV( ig ), data%ISTAGV( ig + 1 ) - 1
            j = data%ISVGRP( l )
            work%W_ws( j ) = prod * work%W_ws( j )
          END DO
        ELSE
          work%W_ws( data%ISVGRP( data%ISTAGV( ig ) :                          &
                                  data%ISTAGV( ig + 1 ) - 1 ) ) = 0.0_rp_
        END IF

!  ======================= second-order terms =======================

!  now consider the product of VECTOR with the second order term
!  g'(h_ig) * Hess h_ig

        IF (  data%GSCALE( ig ) /= 0.0_rp_ ) THEN
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
               CALL RANGE_r( iel, .FALSE., work%W_el, work%W_in, nvarel, nin,  &
                             data%ITYPEE( iel ), nvarel, nin )

!  multiply the internal variables by the element Hessian and put the
!  product in H_in. Consider the first column of the element Hessian

               ielhst = data%ISTADH( iel )
               pi = gi * work%W_in( 1 )
               work%H_in( : nin )                                              &
                 = pi * work%FUVALS( data%ISYMMH( 1, : nin ) + ielhst )

!  now consider the remaining columns of the element Hessian

               DO jcol = 2, nin
                 pi = gi * work%W_in( jcol )
                 IF ( pi /= 0.0_rp_ ) THEN
                   work%H_in( : nin ) = work%H_in( : nin ) +                   &
                     pi * work%FUVALS( data%ISYMMH( jcol, : nin ) + ielhst )
                 END IF
               END DO

!  scatter the product back onto the elemental variables, W

               CALL RANGE_r( iel, .TRUE., work%H_in, work%W_el, nvarel, nin,   &
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
        END IF

!  copy the nonzeros back to the ith column of H_i s

        DO l = CHP_ptr( ic ), CHP_ptr( ic + 1 ) - 1
          CHP_val( l ) = work%W_ws( CHP_ind( l ) )
!       DO l = data%ISTAGV( ig ), data%ISTAGV( ig + 1 ) - 1
!         CHP_val( l ) = work%W_ws( data%ISVGRP( l ) )
        END DO
      END DO

!  evaluate the product

!     CALL CUTEST_hessian_times_vector(                                        &
!       data%n, data%ng, data%nel, data%ntotel, data%nvrels, data%nvargp,      &
!       data%alllin, data%ISTAEV, data%ISTADH,                                 &
!       data%INTVAR, data%IELING, data%IELVAR, VECTOR, RESULT,                 &
!       work%GVALS( : , 2 ) , work%GVALS( : , 3 ),                             &
!       work%FUVALS( data%lgrjac + 1 ),                                        &
!       work%GSCALE_used, data%ESCALE, work%FUVALS( : data%lnhuvl ),           &
!       data%lnhuvl, data%GXEQX, data%INTREP, data%IGCOLJ,                     &
!       data%ISLGRP, data%ITYPEE, data%ISYMMH, work%ISTAJC,                    &
!       work%W_ws, work%W_el, work%W_in, work%H_in, RANGE_r )

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
        "( ' ** SUBROUTINE CCHPRODS: error flag raised during SIF evaluation')")
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_cchprods = work%time_cchprods + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_cchprods_threadsafe_r

      END SUBROUTINE CUTEST_cchprods_threadsafe_r

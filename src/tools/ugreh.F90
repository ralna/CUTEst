! THIS VERSION: CUTEST 2.3 - 2024-10-20 AT 15:40 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-  C U T E S T    U G R E H _ C   S U B R O U T I N E  -*-*-*-*-*--

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 20th October 2024

      SUBROUTINE CUTEST_ugreh_c_r( status, n, X, G, ne, lhe_ptr,               &
                                   HE_row_ptr, HE_val_ptr, lhe_row,            &
                                   HE_row, lhe_val, HE_val, byrows )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lhe_ptr, lhe_row, lhe_val
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: ne, status
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: byrows
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lhe_ptr ) :: HE_row_ptr
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lhe_ptr ) :: HE_val_ptr
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lhe_row ) :: HE_row
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lhe_val ) :: HE_val

!  ----------------------------------------------------------------------------
!  compute the gradient and Hessian matrix of a group partially separable
!  function initially written in Standard Input Format (SIF)

!  the matrix is represented in "finite element format", i.e.,

!           ne
!      H = sum H_e,
!          e=1

!  where each element H_i involves a small subset of the rows of H. H is stored
!  as a list of the row indices involved in each element and the upper triangle
!  of H_e (stored by rows or columns). Specifically,

!  ne (integer) number of elements
!  HE_row (integer array) a list of the 0-based row indices involved which 
!          each element. Those for element e directly proceed those for
!          element e + 1, e = 1, ..., ne-1
!  HE_row_ptr (integer array) pointers to the 0-based position in HE_row of 
!          the first row index in each element. HE_row_ptr(ne+1) points to the
!          0-based first empty location in HE_row
!  HE_val (real array) a list of the nonzeros in the upper triangle of
!          H_e, stored by rows, or by columns, for each element. Those
!          for element i directly proceed those for element, e + 1,
!          e = 1, ..., ne-1
!  HE_val_ptr (integer array) pointers to the 0-based position in HE_val of
!          the first nonzero in each element. HE_val_ptr(ne+1) points to the 
!          0-based first empty location in HE_val
!  byrows (booll) must be set .TRUE. if the upper triangle of each H_e is
!          to be stored by rows, and .FALSE. if it is to be stored by columns
!  ----------------------------------------------------------------------------

      LOGICAL :: byrows_fortran

      byrows_fortran = byrows
      CALL CUTEST_ugreh_r( status, n, X, G,                                    &
                           ne, lhe_ptr, HE_row_ptr, HE_val_ptr, lhe_row,       &
                           HE_row, lhe_val, HE_val, byrows_fortran )

      HE_row( : HE_row_ptr( ne + 1 ) - 1 )                                     &
        = HE_row( : HE_row_ptr( ne + 1 ) - 1 ) - 1
      HE_row_ptr( : ne + 1 ) = HE_row_ptr( : ne + 1 ) - 1
      HE_val_ptr( : ne + 1 ) = HE_val_ptr( : ne + 1 ) - 1

      RETURN

!  end of subroutine CUTEST_ugreh_c_r

      END SUBROUTINE CUTEST_ugreh_c_r

!-*-*-*-*-  C U T E S T  C I N T _  U G R E H    S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 21st August 2013

      SUBROUTINE CUTEST_cint_ugreh_r( status, n, X, G, ne, lhe_ptr,            &
                                     HE_row_ptr, HE_val_ptr, lhe_row,          &
                                     HE_row, lhe_val, HE_val, byrows )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lhe_ptr, lhe_row, lhe_val
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: ne, status
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: byrows
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lhe_ptr ) :: HE_row_ptr
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lhe_ptr ) :: HE_val_ptr
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lhe_row ) :: HE_row
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lhe_val ) :: HE_val

!  ----------------------------------------------------------------------------
!  compute the gradient and Hessian matrix of a group partially separable
!  function initially written in Standard Input Format (SIF)

!  the matrix is represented in "finite element format", i.e.,

!           ne
!      H = sum H_e,
!          e=1

!  where each element H_i involves a small subset of the rows of H. H is stored
!  as a list of the row indices involved in each element and the upper triangle
!  of H_e (stored by rows or columns). Specifically,

!  ne (integer) number of elements
!  HE_row (integer array) a list of the row indices involved which each
!          element. Those for element e directly proceed those for
!          element e + 1, e = 1, ..., ne-1
!  HE_row_ptr (integer array) pointers to the position in HE_row of the first
!          row index in each element. HE_row_ptr(ne+1) points to the first
!          empty location in IRPNHI
!  HE_val (real array) a list of the nonzeros in the upper triangle of
!          H_e, stored by rows, or by columns, for each element. Those
!          for element i directly proceed those for element, e + 1,
!          e = 1, ..., ne-1
!  HE_val_ptr (integer array) pointers to the position in HE_val of the first
!          nonzero in each element. HE_val_ptr(ne+1) points to the first
!          empty location in HE_val
!  byrows (booll) must be set .TRUE. if the upper triangle of each H_e is
!          to be stored by rows, and .FALSE. if it is to be stored by columns
!  ----------------------------------------------------------------------------

      LOGICAL :: byrows_fortran

      byrows_fortran = byrows
      CALL CUTEST_ugreh_r( status, n, X, G,                                    &
                           ne, lhe_ptr, HE_row_ptr, HE_val_ptr, lhe_row,       &
                           HE_row, lhe_val, HE_val, byrows_fortran )

      RETURN

!  end of subroutine CUTEST_cint_ugreh_r

      END SUBROUTINE CUTEST_cint_ugreh_r

!-*-*-*-*-*-*-  C U T E S T    U G R E H    S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th December 2012

      SUBROUTINE CUTEST_ugreh_r( status, n, X, G, ne,                          &
                                 lhe_ptr, HE_row_ptr, HE_val_ptr, lhe_row,     &
                                 HE_row, lhe_val, HE_val, byrows )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lhe_ptr, lhe_row, lhe_val
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: ne, status
      LOGICAL, INTENT( IN ) :: byrows
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lhe_ptr ) :: HE_row_ptr
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lhe_ptr ) :: HE_val_ptr
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lhe_row ) :: HE_row
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lhe_val ) :: HE_val

!  ----------------------------------------------------------------------------
!  compute the gradient and Hessian matrix of a group partially separable
!  function initially written in Standard Input Format (SIF)

!  the matrix is represented in "finite element format", i.e.,

!           ne
!      H = sum H_e,
!          e=1

!  where each element H_i involves a small subset of the rows of H. H is stored
!  as a list of the row indices involved in each element and the upper triangle
!  of H_e (stored by rows or columns). Specifically,

!  ne (integer) number of elements
!  HE_row (integer array) a list of the row indices involved which each
!          element. Those for element e directly proceed those for
!          element e + 1, e = 1, ..., ne-1
!  HE_row_ptr (integer array) pointers to the position in HE_row of the first
!          row index in each element. HE_row_ptr(ne+1) points to the first
!          empty location in IRPNHI
!  HE_val (real array) a list of the nonzeros in the upper triangle of
!          H_e, stored by rows, or by columns, for each element. Those
!          for element i directly proceed those for element, e + 1,
!          e = 1, ..., ne-1
!  HE_val_ptr (integer array) pointers to the position in HE_val of the first
!          nonzero in each element. HE_val_ptr(ne+1) points to the first
!          empty location in HE_val
!  byrows (logical) must be set .TRUE. if the upper triangle of each H_e is
!          to be stored by rows, and .FALSE. if it is to be stored by columns
!  ----------------------------------------------------------------------------

      CALL CUTEST_ugreh_threadsafe_r( CUTEST_data_global,                      &
                                      CUTEST_work_global( 1 ),                 &
                                      status, n, X, G, ne, lhe_ptr,            &
                                      HE_row_ptr, HE_val_ptr, lhe_row,         &
                                      HE_row, lhe_val, HE_val, byrows )
      RETURN

!  end of subroutine CUTEST_ugreh_r

      END SUBROUTINE CUTEST_ugreh_r

!-*-*-  C U T E S T    U G R E H _ t h r e a d e d   S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th December 2012

      SUBROUTINE CUTEST_ugreh_threaded_r( status, n, X, G, ne, lhe_ptr,        &
                                          HE_row_ptr, HE_val_ptr, lhe_row,     &
                                          HE_row, lhe_val, HE_val, byrows,     &
                                          thread)
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lhe_ptr, lhe_row, lhe_val
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: thread
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: ne, status
      LOGICAL, INTENT( IN ) :: byrows
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lhe_ptr ) :: HE_row_ptr
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lhe_ptr ) :: HE_val_ptr
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lhe_row ) :: HE_row
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lhe_val ) :: HE_val

!  ----------------------------------------------------------------------------
!  compute the gradient and Hessian matrix of a group partially separable
!  function initially written in Standard Input Format (SIF)

!  the matrix is represented in "finite element format", i.e.,

!           ne
!      H = sum H_e,
!          e=1

!  where each element H_i involves a small subset of the rows of H. H is stored
!  as a list of the row indices involved in each element and the upper triangle
!  of H_e (stored by rows or columns). Specifically,

!  ne (integer) number of elements
!  HE_row (integer array) a list of the row indices involved which each
!          element. Those for element e directly proceed those for
!          element e + 1, e = 1, ..., ne-1
!  HE_row_ptr (integer array) pointers to the position in HE_row of the first
!          row index in each element. HE_row_ptr(ne+1) points to the first
!          empty location in IRPNHI
!  HE_val (real array) a list of the nonzeros in the upper triangle of
!          H_e, stored by rows, or by columns, for each element. Those
!          for element i directly proceed those for element, e + 1,
!          e = 1, ..., ne-1
!  HE_val_ptr (integer array) pointers to the position in HE_val of the first
!          nonzero in each element. HE_val_ptr(ne+1) points to the first
!          empty location in HE_val
!  byrows (logical) must be set .TRUE. if the upper triangle of each H_e is
!          to be stored by rows, and .FALSE. if it is to be stored by columns
!  ----------------------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_ugreh_threadsafe_r( CUTEST_data_global,                      &
                                      CUTEST_work_global( thread ),            &
                                      status, n, X, G, ne, lhe_ptr,            &
                                      HE_row_ptr, HE_val_ptr, lhe_row,         &
                                      HE_row, lhe_val, HE_val, byrows )
      RETURN

!  end of subroutine CUTEST_ugreh_threaded_r

      END SUBROUTINE CUTEST_ugreh_threaded_r

!-*-  C U T E S T    U G R E H _ t h r e a d s a f e   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTEr, November 1994
!   fortran 2003 version released in CUTEst, 27th November 2012

      SUBROUTINE CUTEST_ugreh_threadsafe_r( data, work, status, n, X, G,       &
                                            ne, lhe_ptr, HE_row_ptr,           &
                                            HE_val_ptr, lhe_row,               &
                                            HE_row, lhe_val, HE_val, byrows )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lhe_ptr, lhe_row, lhe_val
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: ne, status
      LOGICAL, INTENT( IN ) :: byrows
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lhe_ptr ) :: HE_row_ptr
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lhe_ptr ) :: HE_val_ptr
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lhe_row ) :: HE_row
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lhe_val ) :: HE_val

!  ----------------------------------------------------------------------------
!  compute the gradient and Hessian matrix of a group partially separable
!  function initially written in Standard Input Format (SIF)

!  the matrix is represented in "finite element format", i.e.,

!           ne
!      H = sum H_e,
!          e=1

!  where each element H_i involves a small subset of the rows of H. H is stored
!  as a list of the row indices involved in each element and the upper triangle
!  of H_e (stored by rows or columns). Specifically,

!  ne (integer) number of elements
!  HE_row (integer array) a list of the row indices involved which each
!          element. Those for element e directly proceed those for
!          element e + 1, e = 1, ..., ne-1
!  HE_row_ptr (integer array) pointers to the position in HE_row of the first
!          row index in each element. HE_row_ptr(ne+1) points to the first
!          empty location in IRPNHI
!  HE_val (real array) a list of the nonzeros in the upper triangle of
!          H_e, stored by rows, or by columns, for each element. Those
!          for element i directly proceed those for element, e + 1,
!          e = 1, ..., ne-1
!  HE_val_ptr (integer array) pointers to the position in HE_val of the first
!          nonzero in each element. HE_val_ptr(ne+1) points to the first
!          empty location in HE_val
!  byrows (logical) must be set .TRUE. if the upper triangle of each H_e is
!          to be stored by rows, and .FALSE. if it is to be stored by columns
!  ----------------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, ig, j, ifstat, igstat
      INTEGER ( KIND = ip_ ) :: alloc_status, lhe_row_int, lhe_val_int
      REAL ( KIND = rp_ ) :: ftt
      REAL :: time_in, time_out
      CHARACTER ( LEN = 80 ) :: bad_alloc = REPEAT( ' ', 80 )
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

!  evaluate the element function gradients and Hessians

      CALL ELFUN_r( work%FUVALS, X, data%EPVALU, data%nel, data%ITYPEE,        &
                    data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,        &
                    data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,         &
                    data%lelvar, data%lntvar, data%lstadh, data%lstep,         &
                    data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,        &
                    3, ifstat )
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

        IF ( data%GXEQX( ig ) ) THEN
          work%GVALS( ig, 2 ) = 1.0_rp_
          work%GVALS( ig, 3 ) = 0.0_rp_
        END IF
      END DO

!  evaluate the group derivative values

      IF ( .NOT. data%altriv ) THEN
        CALL GROUP_r( work%GVALS, data%ng, work%FT, data%GPVALU, data%ng,      &
                      data%ITYPEG, data%ISTGP, work%ICALCF, data%ltypeg,       &
                      data%lstgp, data%lcalcf, data%lcalcg, data%lgpvlu,       &
                      .TRUE., igstat )
        IF ( igstat /= 0 ) GO TO 930
      END IF

!  compute the gradient value

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

!  assemble the Hessian

      lhe_row_int = lhe_row ; lhe_val_int = lhe_val
      CALL CUTEST_assemble_element_hessian(                                    &
                        data%ng, data%nel,data% ntotel, data%nvrels,           &
                        data%nnza, data%maxsel, data%nvargp,                   &
                        data%lnguvl, data%lnhuvl, data%ISTADH, data%ICNA,      &
                        data%ISTADA, data%INTVAR, data%IELVAR,                 &
                        data%IELING, data%ISTADG, data%ISTAEV,                 &
                        data%ISTAGV, data%ISVGRP, data%ITYPEE,                 &
                        data%A, work%FUVALS, work%FUVALS,                      &
                        work%GVALS( : , 2 ), work%GVALS( : , 3 ),              &
                        data%GSCALE, data%ESCALE, data%GXEQX, data%INTREP,     &
                        work%ISWKSP, work%W_ws, work%W_el, work%W_in,          &
                        work%H_el, work%H_in, RANGE_r,                         &
                        ne, lhe_ptr, lhe_row_int,                              &
                        lhe_val_int, work%H_row, HE_row_ptr, work%H_val,       &
                        HE_val_ptr, byrows, 0_ip_, data%out, data%out,         &
                        work%io_buffer, alloc_status, bad_alloc, status )

!  check for errors in the assembly

      IF ( status > 0 ) GO TO 990

!  check that HE_row and HE_val are large enough

      IF ( lhe_row < HE_row_ptr( ne + 1 ) - 1 ) THEN
        IF ( data%out > 0 ) WRITE( data%out, "( ' ** SUBROUTINE UGREH: ',      &
       &  'Increase the dimension of HE_row to ',  I0 )" )                     &
             HE_row_ptr( ne + 1 ) - 1
        status = 2 ; GO TO 990
      END IF

      IF ( lhe_val < HE_val_ptr( ne + 1 ) - 1 ) THEN
        IF ( data%out > 0 ) WRITE( data%out, "( ' ** SUBROUTINE UGREH: ',      &
       &  'Increase the dimension of HE_val to ',  I0 )" )                     &
             HE_val_ptr( ne + 1 ) - 1
        status = 2 ; GO TO 990
      END IF

!  record the element Hessian

      HE_row( : HE_row_ptr( ne + 1 ) - 1 )                                     &
         = work%H_row( : HE_row_ptr( ne + 1 ) - 1 )
      HE_val( : HE_val_ptr( ne + 1 ) - 1 )                                     &
         = work%H_val( : HE_val_ptr( ne + 1 ) - 1 )

!  update the counters for the report tool

      work%nc2og = work%nc2og + 1
      work%nc2oh = work%nc2oh + 1
      status = 0
      GO TO 990

!  unsuccessful returns

  930 CONTINUE
      IF ( data%out > 0 ) WRITE( data%out,                                     &
        "( ' ** SUBROUTINE UGREH: error flag raised during SIF evaluation' )" )
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_ugreh = work%time_ugreh + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_ugreh_threadsafe_r

      END SUBROUTINE CUTEST_ugreh_threadsafe_r

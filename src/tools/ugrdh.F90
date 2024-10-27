! THIS VERSION: CUTEST 2.3 - 2024-10-27 AT 09:20 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-  C U T E S T    U G R D H _ C   S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 20th October 2024

      SUBROUTINE CUTEST_ugrdh_c_r( status, n, X, G, lh1, H )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lh1
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh1 * n ) :: H

!  ----------------------------------------------------------------------
!  compute the gradient and Hessian matrix of a group partially
!  separable function. The Hessian is stored as a 1D lh1 * n array by rows
!  ----------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, j, l

!  create 2D Hessian storage if needed

      IF ( .NOT. CUTEST_work_global( 1 )%hessian_2d_setup_complete ) THEN
        ALLOCATE( CUTEST_work_global( 1 )%H_2d( n, n ), STAT = status )
        IF ( status /= 0 ) RETURN
        CUTEST_work_global( 1 )%hessian_2d_setup_complete = .TRUE.
      END IF

      CALL CUTEST_ugrdh_threadsafe_r( CUTEST_data_global,                      &
                                      CUTEST_work_global( 1 ),                 &
                                      status, n, X, G, n,                      &
                                      CUTEST_work_global( 1 )%H_2d )

!  transfer the 2D Hessian array stored by columns to a 1D array stored by rows

      l = 0
      DO i = 1, n
        DO j = 1, n
          l = l + 1
          H( l ) = CUTEST_work_global( 1 )%H_2d( i, j )
        END DO
      END DO

      RETURN

!  end of subroutine CUTEST_ugrdh_c_r

      END SUBROUTINE CUTEST_ugrdh_c_r

!-*-*-*-*-*-*-  C U T E S T    U G R D H    S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th December 2012

      SUBROUTINE CUTEST_ugrdh_r( status, n, X, G, lh1, H )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lh1
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh1, n ) :: H

!  ---------------------------------------------------------------------
!  compute the gradient and Hessian matrix of a group partially
!  separable function. The Hessian is stored as a dense symmetric matrix
!  ---------------------------------------------------------------------

      CALL CUTEST_ugrdh_threadsafe_r( CUTEST_data_global,                      &
                                      CUTEST_work_global( 1 ),                 &
                                      status, n, X, G, lh1, H )
      RETURN

!  end of subroutine CUTEST_ugrdh_r

      END SUBROUTINE CUTEST_ugrdh_r

!-*-*-  C U T E S T    U G R D H _ t h r e a d e d   S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th December 2012

      SUBROUTINE CUTEST_ugrdh_threaded_r( status, n, X, G, lh1, H, thread )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lh1, thread
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh1, n ) :: H

!  ---------------------------------------------------------------------
!  compute the gradient and Hessian matrix of a group partially
!  separable function. The Hessian is stored as a dense symmetric matrix
!  ---------------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_ugrdh_threadsafe_r( CUTEST_data_global,                      &
                                      CUTEST_work_global( thread ),            &
                                      status, n, X, G, lh1, H )
      RETURN

!  end of subroutine CUTEST_ugrdh_threaded_r

      END SUBROUTINE CUTEST_ugrdh_threaded_r

!-*-  C U T E S T    U G R D H _ t h r e a d s a f e   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTE, December 1990
!   fortran 2003 version released in CUTEst, 23rd November 2012

      SUBROUTINE CUTEST_ugrdh_threadsafe_r( data, work, status, n, X, G,       &
                                            lh1, H )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lh1
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh1, n ) :: H

!  ---------------------------------------------------------------------
!  compute the gradient and Hessian matrix of a group partially
!  separable function. The Hessian is stored as a dense symmetric matrix
!  ---------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, ig, j, k, nnzh, ifstat, igstat, alloc_status
      REAL ( KIND = rp_ ) :: ftt
      REAL :: time_in, time_out
      CHARACTER ( LEN = 80 ) :: bad_alloc = REPEAT( ' ', 80 )
      EXTERNAL :: RANGE_r

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  check input parameters

      IF ( lh1 < n ) THEN
        WRITE( data%out, "( ' ** SUBROUTINE UGRDH: Increase the leading ',     &
       &  'dimension of H to ', I0 )" ) n
        status = 2 ; GO TO 990
      END IF

!  there are non-trivial group functions.

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

!  evaluate the element function values

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

!  transfer the gradient value

      G( : n ) = work%FUVALS( data%lggfx + 1 : data%lggfx + n )

!  assemble the Hessian

      CALL CUTEST_assemble_hessian(                                            &
             n, data%ng, data%nel, data%ntotel, data%nvrels, data%nnza,        &
             data%maxsel, data%nvargp, data%ISTADH,                            &
             data%ICNA, data%ISTADA, data%INTVAR, data%IELVAR, data%IELING,    &
             data%ISTADG, data%ISTAEV, data%ISTAGV, data%ISVGRP, data%A,       &
             work%FUVALS, data%lnguvl, work%FUVALS, data%lnhuvl,               &
             work%GVALS( : , 2 ), work%GVALS( :  , 3 ), data%GSCALE,           &
             data%ESCALE, data%GXEQX, data%ITYPEE, data%INTREP, RANGE_r,       &
             0_ip_, data%out, data%out, .TRUE., .FALSE.,                       &
             n, status, alloc_status, bad_alloc,                               &
             work%array_status, work%lh_row, work%lh_col, work%lh_val,         &
             work%H_row, work%H_col, work%H_val, work%ROW_start,               &
             work%POS_in_H, work%USED, work%FILLED,                            &
             work%lrowst, work%lpos, work%lused, work%lfilled,                 &
             work%W_ws, work%W_el, work%W_in, work%H_el, work%H_in,            &
             nnzh = nnzh )

!  check for errors in the assembly

      IF ( status > 0 ) GO TO 990

!  initialize the dense matrix

      H( : n, : n ) = 0.0_rp_

!  transfer the matrix from co-ordinate to dense storage and symmetrize it

      DO k = 1, nnzh
        i = work%H_row( k ) ; j = work%H_col( k )
        H( i, j ) = work%H_val( k ) ; H( j, i ) = work%H_val( k )
      END DO

!  update the counters for the report tool

      work%nc2og = work%nc2og + 1
      work%nc2oh = work%nc2oh + 1
      status = 0
      GO TO 990

!  unsuccessful returns

  930 CONTINUE
      IF ( data%out > 0 ) WRITE( data%out,                                     &
        "( ' ** SUBROUTINE UGRDH: error flag raised during SIF evaluation' )" )
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_ugrdh = work%time_ugrdh + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_ugrdh_threadsafe_r

      END SUBROUTINE CUTEST_ugrdh_threadsafe_r

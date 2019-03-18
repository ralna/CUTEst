! THIS VERSION: CUTEST 1.4 - 26/02/2016 AT 09:00 GMT.

!-*-*-*-*-*-*-  C U T E S T    U B A N D H    S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th December 2012

      SUBROUTINE CUTEST_ubandh( status, n, X, semibandwidth, H_band,           &
                                lbandh, max_semibandwidth )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER, INTENT( IN ) :: n, semibandwidth, lbandh
      INTEGER, INTENT( OUT ) :: status, max_semibandwidth
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = wp ), INTENT( OUT ), DIMENSION( 0 : lbandh, n ) :: H_band

!  ----------------------------------------------------------------------
!  compute the portion of the Hessian matrix of a group partially
!  separable function which lies within a band of given semi-bandwidth.
!  The diagonal and subdiagonal entries in column i are stored in
!  locations BAND( j, i ), where j = 0 for the diagonal and j > 0 for
!  the j-th subdiagonal entry, j = 1, ..., MIN( semibandwidth, n - i )
!  and semibandwidth is the requested semi-bandwidth. max_semibandwidth
!  gives the true semibandwidth, i.e, all entries outside the band with
!  this semibandwidth are zero
!  -----------------------------------------------------------------------

      CALL CUTEST_ubandh_threadsafe( CUTEST_data_global,                       &
                                     CUTEST_work_global( 1 ),                  &
                                     status, n, X, semibandwidth, H_band,      &
                                     lbandh, max_semibandwidth )
      RETURN

!  end of subroutine CUTEST_ubandh

      END SUBROUTINE CUTEST_ubandh

!-*-  C U T E S T    U B A N D H _ t h r e a d e d    S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th December 2012

      SUBROUTINE CUTEST_ubandh_threaded( status, n, X, semibandwidth, H_band,  &
                                         lbandh, max_semibandwidth, thread )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER, INTENT( IN ) :: n, semibandwidth, lbandh, thread
      INTEGER, INTENT( OUT ) :: status, max_semibandwidth
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = wp ), INTENT( OUT ), DIMENSION( 0 : lbandh, n ) :: H_band

!  ----------------------------------------------------------------------
!  compute the portion of the Hessian matrix of a group partially
!  separable function which lies within a band of given semi-bandwidth.
!  The diagonal and subdiagonal entries in column i are stored in
!  locations BAND( j, i ), where j = 0 for the diagonal and j > 0 for
!  the j-th subdiagonal entry, j = 1, ..., MIN( semibandwidth, n - i )
!  and semibandwidth is the requested semi-bandwidth. max_semibandwidth
!  gives the true semibandwidth, i.e, all entries outside the band with
!  this semibandwidth are zero
!  -----------------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_ubandh_threadsafe( CUTEST_data_global,                       &
                                     CUTEST_work_global( thread ),             &
                                     status, n, X, semibandwidth, H_band,      &
                                     lbandh, max_semibandwidth )
      RETURN

!  end of subroutine CUTEST_ubandh_threaded

      END SUBROUTINE CUTEST_ubandh_threaded

!-   C U T E S T    U B A N D H _ t h r e a d s a f e   S U B R O U T I N E   -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTE, August 1993
!   fortran 2003 version released in CUTEst, 23rd November 2012

      SUBROUTINE CUTEST_ubandh_threadsafe( data, work, status, n, X,           &
                                           semibandwidth, H_band, lbandh,      &
                                           max_semibandwidth )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER, INTENT( IN ) :: n, semibandwidth, lbandh
      INTEGER, INTENT( OUT ) :: status, max_semibandwidth
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = wp ), INTENT( OUT ), DIMENSION( 0 : lbandh, n ) :: H_band

!  ----------------------------------------------------------------------
!  compute the portion of the Hessian matrix of a group partially
!  separable function which lies within a band of given semi-bandwidth.
!  The diagonal and subdiagonal entries in column i are stored in
!  locations BAND( j, i ), where j = 0 for the diagonal and j > 0 for
!  the j-th subdiagonal entry, j = 1, ..., MIN( semibandwidth, n - i )
!  and semibandwidth is the requested semi-bandwidth. max_semibandwidth
!  gives the true semibandwidth, i.e, all entries outside the band with
!  this semibandwidth are zero
!  -----------------------------------------------------------------------

!  local variables

      INTEGER :: i, ig, j, nsemiw, ifstat, igstat, alloc_status
      REAL ( KIND = wp ) :: ftt
      REAL :: time_in, time_out
      CHARACTER ( LEN = 80 ) :: bad_alloc = REPEAT( ' ', 80 )
      EXTERNAL :: RANGE

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  check that there is room for the band matrix

      nsemiw = MAX( 0, MIN( semibandwidth, n - 1 ) )
      IF ( lbandh < nsemiw ) THEN
        IF ( data%out > 0 ) WRITE( data%out, "( ' ** SUBROUTINE UBANDH: ',     &
      &   'array dimension lbandh should be larger than semibandwidth' )" )
        status = 2 ; GO TO 990
      END IF

!  there are non-trivial group functions

      DO i = 1, MAX( data%nel, data%ng )
        work%ICALCF( i ) = i
      END DO

!  evaluate the element function values

      CALL ELFUN( work%FUVALS, X, data%EPVALU, data%nel, data%ITYPEE,          &
                  data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,          &
                  data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,           &
                  data%lelvar, data%lntvar, data%lstadh, data%lstep,           &
                  data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,          &
                  1, ifstat )
      IF ( ifstat /= 0 ) GO TO 930

!  evaluate the element function values

      CALL ELFUN( work%FUVALS, X, data%EPVALU, data%nel, data%ITYPEE,          &
                  data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,          &
                  data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,           &
                  data%lelvar, data%lntvar, data%lstadh, data%lstep,           &
                  data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,          &
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

!  Record the derivatives of trivial groups

        IF ( data%GXEQX( ig ) ) THEN
          work%GVALS( ig, 2 ) = 1.0_wp
          work%GVALS( ig, 3 ) = 0.0_wp
        END IF
      END DO

!  evaluate the group derivative values

        IF ( .NOT. data%altriv ) THEN
          CALL GROUP( work%GVALS, data%ng, work%FT, data%GPVALU, data%ng,      &
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
             work%ISTAJC, work%W_ws, work%W_el, RANGE )
      work%firstg = .FALSE.

!  assemble the Hessian

      CALL CUTEST_assemble_hessian(                                            &
             n, data%ng, data%nel, data%ntotel, data%nvrels, data%nnza,        &
             data%maxsel, data%nvargp, data%ISTADH,                            &
             data%ICNA, data%ISTADA, data%INTVAR, data%IELVAR, data%IELING,    &
             data%ISTADG, data%ISTAEV, data%ISTAGV, data%ISVGRP, data%A,       &
             work%FUVALS, data%lnguvl, work%FUVALS, data%lnhuvl,               &
             work%GVALS( : , 2 ), work%GVALS( :  , 3 ), data%GSCALE,           &
             data%ESCALE, data%GXEQX, data%ITYPEE, data%INTREP, RANGE,         &
             0, data%out, data%out, .FALSE., .TRUE.,                           &
             nsemiw, status, alloc_status, bad_alloc,                          &
             work%array_status, work%lh_row, work%lh_col, work%lh_val,         &
             work%H_row, work%H_col, work%H_val, work%ROW_start,               &
             work%POS_in_H, work%USED, work%FILLED,                            &
             work%lrowst, work%lpos, work%lused, work%lfilled,                 &
             work%W_ws, work%W_el, work%W_in, work%H_el, work%H_in,            &
             maxsbw = max_semibandwidth,                                       &
             DIAG = H_band( 0, : n ), OFFDIA = H_band( 1 : nsemiw, : n  ) )

      GO TO 990

!  unsuccessful returns

  930 CONTINUE
      IF ( data%out > 0 ) WRITE( data%out,                                     &
        "( ' ** SUBROUTINE UBANDH: error flag raised during SIF evaluation' )" )
      status = 3
      RETURN

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_ubandh = work%time_ubandh + time_out - time_in
      END IF

!  end of subroutine CUTEST_ubandh_threadsafe

      END SUBROUTINE CUTEST_ubandh_threadsafe


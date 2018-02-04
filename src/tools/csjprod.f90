! THIS VERSION: CUTEST 1.4 - 26/02/2016 AT 08:00 GMT.

!-*-*-*-*-  C U T E S T   C I N T _ C S J P R O D    S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 1st October 2014

      SUBROUTINE CUTEST_Cint_csjprod( status, n, m, gotj, jtrans, X,           &
                                      nnz_vector, INDEX_nz_vector,             &
                                      VECTOR, lvector,                         &
                                      nnz_result, INDEX_nz_result,             &
                                      RESULT, lresult )
      USE CUTEST
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER, INTENT( IN ) :: n, m, nnz_vector, lvector, lresult
      INTEGER, INTENT( OUT ) :: status, nnz_result
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: gotj, jtrans
      INTEGER, DIMENSION( nnz_vector ), INTENT( IN ) :: INDEX_nz_vector
      INTEGER, DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( lvector ) :: VECTOR
      REAL ( KIND = wp ), INTENT( OUT ), DIMENSION( lresult ) :: RESULT

!  -------------------------------------------------------------------------
!  compute the matrix-vector product between the constraint Jacobian matrix
!  (or its transpose if jtrans is .TRUE.) for the problem and a given sparse
!  vector VECTOR. The result is placed in RESULT. If gotj is .TRUE. the
!  Jacobian is assumed to have already been computed. If the user is unsure,
!  set gotj = .FALSE. the first time a product is required with the Jacobian
!  evaluated at X. X is not used if gotj = .TRUE. Only the components
!  INDEX_nz_vector(1:nnz_vector) of VECTOR(1:lvector) are nonzero, and the
!  remaining components of VECTOR need not have been be set. On exit, only
!  the components INDEX_nz_result(1:nnz_result) of RESULT(1:lresult) are
!  nonzero, and the remaining components of RESULT may not have been set.
!  -------------------------------------------------------------------------

      LOGICAL :: gotj_fortran, jtrans_fortran

      gotj_fortran = gotj
      jtrans_fortran = jtrans
      CALL CUTEST_csjprod( status, n, m, gotj_fortran, jtrans_fortran, X,      &
                           nnz_vector, INDEX_nz_vector, VECTOR, lvector,       &
                           nnz_result, INDEX_nz_result, RESULT, lresult )

      RETURN

!  end of subroutine CUTEST_Cint_csjprod

      END SUBROUTINE CUTEST_Cint_csjprod

!-*-*-*-*-*-*-  C U T E S T    C S J P R O D    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 1st October 2014

      SUBROUTINE CUTEST_csjprod( status, n, m, gotj, jtrans, X,                &
                                 nnz_vector, INDEX_nz_vector, VECTOR, lvector, &
                                 nnz_result, INDEX_nz_result, RESULT, lresult )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER, INTENT( IN ) :: n, m, nnz_vector, lvector, lresult
      INTEGER, INTENT( OUT ) :: status, nnz_result
      LOGICAL, INTENT( IN ) :: gotj, jtrans
      INTEGER, DIMENSION( nnz_vector ), INTENT( IN ) :: INDEX_nz_vector
      INTEGER, DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( lvector ) :: VECTOR
      REAL ( KIND = wp ), INTENT( OUT ), DIMENSION( lresult ) :: RESULT

!  -----------------------------------------------------------------------
!  compute the matrix-vector product between the Hessian matrix of
!  the Lagrangian function for the problem and a given sparse vector
!  VECTOR. The result is placed in RESULT. If gotj is .TRUE. the second
!  derivatives are assumed to have already been computed. If the user is
!  unsure, set gotj = .FALSE. the first time a product is required with
!  the Hessian evaluated at X and Y. X and Y are not used if gotj = .TRUE.
!  Only the components INDEX_nz_vector(1:nnz_vector) of VECTOR are nonzero,
!  and the remaining components of VECTOR need not have been be set. On
!  exit, only the components INDEX_nz_result(1:nnz_result) of RESULT are
!  nonzero, and the remaining components of RESULT may not have been set.
!  -----------------------------------------------------------------------

      CALL CUTEST_csjprod_threadsafe( CUTEST_data_global,                      &
                                      CUTEST_work_global( 1 ),                 &
                                      status, n, m, gotj, jtrans, X,           &
                                      nnz_vector, INDEX_nz_vector,             &
                                      VECTOR, lvector,                         &
                                      nnz_result, INDEX_nz_result,             &
                                      RESULT, lresult )
      RETURN

!  end of subroutine CUTEST_csjprod

      END SUBROUTINE CUTEST_csjprod

!-*-*-  C U T E S T   C S J P R O D _ t h r e a d e d   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 1st October 2014

      SUBROUTINE CUTEST_csjprod_threaded( status, n, m, gotj, jtrans, X,       &
                                          nnz_vector, INDEX_nz_vector,         &
                                          VECTOR, lvector,                     &
                                          nnz_result, INDEX_nz_result,         &
                                          RESULT, lresult, thread )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER, INTENT( IN ) :: n, m, nnz_vector, lvector, lresult, thread
      INTEGER, INTENT( OUT ) :: status, nnz_result
      LOGICAL, INTENT( IN ) :: gotj, jtrans
      INTEGER, DIMENSION( nnz_vector ), INTENT( IN ) :: INDEX_nz_vector
      INTEGER, DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( lvector ) :: VECTOR
      REAL ( KIND = wp ), INTENT( OUT ), DIMENSION( lresult ) :: RESULT

!  -----------------------------------------------------------------------
!  compute the matrix-vector product between the Hessian matrix of
!  the Lagrangian function for the problem and a given sparse vector
!  VECTOR. The result is placed in RESULT. If gotj is .TRUE. the second
!  derivatives are assumed to have already been computed. If the user is
!  unsure, set gotj = .FALSE. the first time a product is required with
!  the Hessian evaluated at X and Y. X and Y are not used if gotj = .TRUE.
!  Only the components INDEX_nz_vector(1:nnz_vector) of VECTOR are nonzero,
!  and the remaining components of VECTOR need not have been be set. On
!  exit, only the components INDEX_nz_result(1:nnz_result) of RESULT are
!  nonzero, and the remaining components of RESULT may not have been set.
!  -----------------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_csjprod_threadsafe( CUTEST_data_global,                      &
                                      CUTEST_work_global( thread ),            &
                                      status, n, m, gotj, jtrans, X,           &
                                      nnz_vector, INDEX_nz_vector,             &
                                      VECTOR, lvector,                         &
                                      nnz_result, INDEX_nz_result,             &
                                      RESULT, lresult )
      RETURN

!  end of subroutine CUTEST_csjprod_threaded

      END SUBROUTINE CUTEST_csjprod_threaded

!-*-  C U T E S T   C S J P R O D _ t h r e a d s a f e   S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Ingrid Bongartz and Nick Gould

!  History -
!   fortran 77 version originally released as CPROD in CUTE, November 1991
!   fortran 2003 version released in CUTEst, 1st October 2014

      SUBROUTINE CUTEST_csjprod_threadsafe( data, work, status, n, m,          &
                                            gotj, jtrans, X,                   &
                                            nnz_vector, INDEX_nz_vector,       &
                                            VECTOR, lvector,                   &
                                            nnz_result, INDEX_nz_result,       &
                                            RESULT, lresult )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER, INTENT( IN ) :: n, m, nnz_vector, lvector, lresult
      INTEGER, INTENT( OUT ) :: status, nnz_result
      LOGICAL, INTENT( IN ) :: gotj, jtrans
      INTEGER, DIMENSION( nnz_vector ), INTENT( IN ) :: INDEX_nz_vector
      INTEGER, DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( lvector ) :: VECTOR
      REAL ( KIND = wp ), INTENT( OUT ), DIMENSION( lresult ) :: RESULT

!  -------------------------------------------------------------------------
!  compute the matrix-vector product between the constraint Jacobian matrix
!  (or its transpose if jtrans is .TRUE.) for the problem and a given sparse
!  vector VECTOR. The result is placed in RESULT. If gotj is .TRUE. the
!  Jacobian is assumed to have already been computed. If the user is unsure,
!  set gotj = .FALSE. the first time a product is required with the Jacobian
!  evaluated at X. X is not used if gotj = .TRUE. Only the components
!  INDEX_nz_vector(1:nnz_vector) of VECTOR(1:lvector) are nonzero, and the
!  remaining components of VECTOR need not have been be set. On exit, only
!  the components INDEX_nz_result(1:nnz_result) of RESULT(1:lresult) are
!  nonzero, and the remaining components of RESULT may not have been set.
!  -------------------------------------------------------------------------

!  local variables

      INTEGER :: i, ig, j, k, l, ifstat, igstat
      REAL ( KIND = wp ) :: ftt, pi
      REAL ( KIND = wp ) :: time_in, time_out
      EXTERNAL :: RANGE

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  check input data

      IF ( ( jtrans .AND. lvector < m ) .OR.                                   &
             ( .NOT. jtrans .AND. lvector < n ) ) THEN
         IF ( data%out > 0 ) WRITE( data%out,                                  &
           "( ' ** SUBROUTINE CSJPROD: Increase the size of VECTOR' )" )
         status = 2 ; GO TO 990
      END IF
      IF ( ( jtrans .AND. lresult < n ) .OR.                                   &
             ( .NOT. jtrans .AND. lresult < m ) ) THEN
         IF ( data%out > 0 ) WRITE( data%out,                                  &
           "( ' ** SUBROUTINE CSJPROD: Increase the size of RESULT' )" )
         status = 2 ; GO TO 990
      END IF

!  there are non-trivial group functions

      IF ( .NOT. gotj ) THEN
        DO i = 1, MAX( data%nel, data%ng )
          work%ICALCF( i ) = i
        END DO

!  evaluate the element function values

        CALL ELFUN( work%FUVALS, X, data%EPVALU, data%nel, data%ITYPEE,        &
                    data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,        &
                    data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,         &
                    data%lelvar, data%lntvar, data%lstadh, data%lstep,         &
                    data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,        &
                    1, ifstat )
        IF ( ifstat /= 0 ) GO TO 930

!  evaluate the element function gradient and Hessian values

        CALL ELFUN( work%FUVALS, X, data%EPVALU, data%nel, data%ITYPEE,        &
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

!  change the group weightings to include the contributions from the
!  Lagrange multipliers.

        IF ( data%numcon > 0 ) THEN
          DO ig = 1, data%ng
            i = data%KNDOFC( ig )
!           IF ( i == 0 ) THEN
              work%GSCALE_used( ig ) = data%GSCALE( ig )
!           ELSE
!             work%GSCALE_used( ig ) = data%GSCALE( ig ) * Y( i )
!           END IF
          END DO

!  compute the gradient value

          CALL CUTEST_form_gradients( n, data%ng, data%nel, data%ntotel,       &
                 data%nvrels, data%nnza, data%nvargp, work%firstg, data%ICNA,  &
                 data%ISTADA, data%IELING, data%ISTADG, data%ISTAEV,           &
                 data%IELVAR, data%INTVAR, data%A, work%GVALS( : , 2 ),        &
                 work%FUVALS, data%lnguvl, work%FUVALS( data%lggfx + 1 ),      &
                 work%GSCALE_used, data%ESCALE, work%FUVALS( data%lgrjac + 1 ),&
                 data%GXEQX, data%INTREP, data%ISVGRP, data%ISTAGV,            &
                 data%ITYPEE, work%ISTAJC, work%W_ws, work%W_el, RANGE )
        ELSE
          CALL CUTEST_form_gradients( n, data%ng, data%nel, data%ntotel,       &
                 data%nvrels, data%nnza, data%nvargp, work%firstg, data%ICNA,  &
                 data%ISTADA, data%IELING, data%ISTADG, data%ISTAEV,           &
                 data%IELVAR, data%INTVAR, data%A, work%GVALS( : , 2 ),        &
                 work%FUVALS, data%lnguvl, work%FUVALS( data%lggfx + 1 ),      &
                 data%GSCALE, data%ESCALE, work%FUVALS( data%lgrjac + 1 ),     &
                 data%GXEQX, data%INTREP, data%ISVGRP, data%ISTAGV,            &
                 data%ITYPEE, work%ISTAJC, work%W_ws, work%W_el, RANGE )
        END IF
      END IF

     nnz_result = 0

!  form the product sparse result = J(transpose) * sparse vector

     IF ( jtrans ) THEN
       DO l = 1, nnz_vector
         i = INDEX_nz_vector( l )
         pi = VECTOR( i )
         ig = data%CGROUP( i )
!DIR$ IVDEP
         DO k = data%ISTAGV( ig ), data%ISTAGV( ig + 1 ) - 1
           j = data%ISVGRP( k )
           IF ( work%IUSED( j ) == 0 ) THEN
             RESULT( j ) = pi * work%FUVALS( data%lgrjac + data%IVALJR( k ) )
             work%IUSED( j ) = 1
             nnz_result = nnz_result + 1
             INDEX_nz_result( nnz_result ) = j
           ELSE
             RESULT( j ) = RESULT( j ) +                                       &
               pi * work%FUVALS( data%lgrjac + data%IVALJR( k ) )
           END IF
         END DO
       END DO

!  form the product sparse result = J * sparse vector

     ELSE
       DO l = 1, nnz_vector
         j = INDEX_nz_vector( l )
         pi = VECTOR( j )
!DIR$ IVDEP
         DO k = work%ISTAJC( j ), work%ISTAJC( j + 1 ) - 1
           i = data%KNDOFC( data%IGCOLJ( k ) )
           IF ( i /= 0 ) THEN
             IF ( work%IUSED( i ) == 0 ) THEN
               RESULT( i ) = pi * work%FUVALS( data%lgrjac + k )
               work%IUSED( i ) = 1
               nnz_result = nnz_result + 1
               INDEX_nz_result( nnz_result ) = i
             ELSE
               RESULT( i ) = RESULT( i ) + pi * work%FUVALS( data%lgrjac + k )
             END IF
           END IF
         END DO
       END DO
     END IF

!  Reset IUSED to zero

     work%IUSED( INDEX_nz_result( : nnz_result ) ) = 0

!  update the counters for the report tool

      work%njvpr = work%njvpr + 1
      IF ( .NOT. gotj ) THEN
        work%nc2cg = work%nc2cg + work%pnc
      END IF
      status = 0
      GO TO 990

!  unsuccessful returns

  930 CONTINUE
      IF ( data%out > 0 ) WRITE( data%out,                                     &
        "( ' ** SUBROUTINE CSJPROD: error flag raised during SIF evaluation')" )
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
         CALL CPU_TIME( time_out )
         work%time_csjprod = work%time_csjprod + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_csjprod_threadsafe

      END SUBROUTINE CUTEST_csjprod_threadsafe



! THIS VERSION: CUTEST 2.3 - 2024-10-22 AT 11:10 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-  C U T E S T    C S J P R O D _ C   S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 22nd October 2024

      SUBROUTINE CUTEST_csjprod_c_r( status, n, m, gotj, jtrans, X,           &
                                     nnz_vector, INDEX_nz_vector,             &
                                     VECTOR, lvector,                         &
                                     nnz_result, INDEX_nz_result,             &
                                     RESULT, lresult )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, nnz_vector, lvector, lresult
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, nnz_result
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: gotj, jtrans
      INTEGER ( KIND = ip_ ), DIMENSION( nnz_vector ),                         &
                              INTENT( INOUT ) :: INDEX_nz_vector
      INTEGER ( KIND = ip_ ), DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( lvector ) :: VECTOR
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lresult ) :: RESULT

!  -------------------------------------------------------------------------
!  compute the matrix-vector product between the constraint Jacobian matrix
!  (or its transpose if jtrans is .TRUE.) for the problem and a given sparse
!  vector VECTOR. The result is placed in RESULT. If gotj is .TRUE. the
!  Jacobian is assumed to have already been computed. If the user is unsure,
!  set gotj = .FALSE. the first time a product is required with the Jacobian
!  evaluated at X. X is not used if gotj = .TRUE. Only the components 0-based
!  INDEX_nz_vector(1:nnz_vector) of VECTOR(1:lvector) are nonzero, and the
!  remaining components of VECTOR need not have been be set. On exit, only
!  the 0-based components INDEX_nz_result(1:nnz_result) of RESULT(1:lresult)
!  are nonzero, and the remaining components of RESULT may not have been set.
!  -------------------------------------------------------------------------

      LOGICAL :: gotj_fortran, jtrans_fortran

      gotj_fortran = gotj
      jtrans_fortran = jtrans
      INDEX_nz_vector( 1 : nnz_vector ) = INDEX_nz_vector( 1 : nnz_vector ) + 1

      CALL CUTEST_csjprod_r( status, n, m, gotj_fortran, jtrans_fortran, X,    &
                             nnz_vector, INDEX_nz_vector, VECTOR, lvector,     &
                             nnz_result, INDEX_nz_result, RESULT, lresult )

      INDEX_nz_vector( 1 : nnz_vector ) = INDEX_nz_vector( 1 : nnz_vector ) - 1
      INDEX_nz_result( 1 : nnz_result ) = INDEX_nz_result( 1 : nnz_result ) - 1

      RETURN

!  end of subroutine CUTEST_csjprod_c_r

      END SUBROUTINE CUTEST_csjprod_c_r

!-*-*-*-*-  C U T E S T   C I N T _ C S J P R O D    S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 1st October 2014

      SUBROUTINE CUTEST_Cint_csjprod_r( status, n, m, gotj, jtrans, X,         &
                                        nnz_vector, INDEX_nz_vector,           &
                                        VECTOR, lvector,                       &
                                        nnz_result, INDEX_nz_result,           &
                                        RESULT, lresult )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, nnz_vector, lvector, lresult
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, nnz_result
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: gotj, jtrans
      INTEGER ( KIND = ip_ ), DIMENSION( nnz_vector ),                         &
                              INTENT( IN ) :: INDEX_nz_vector
      INTEGER ( KIND = ip_ ), DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( lvector ) :: VECTOR
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lresult ) :: RESULT

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
      CALL CUTEST_csjprod_r( status, n, m, gotj_fortran, jtrans_fortran, X,    &
                             nnz_vector, INDEX_nz_vector, VECTOR, lvector,     &
                             nnz_result, INDEX_nz_result, RESULT, lresult )

      RETURN

!  end of subroutine CUTEST_Cint_csjprod_r

      END SUBROUTINE CUTEST_Cint_csjprod_r

!-*-*-*-*-*-*-  C U T E S T    C S J P R O D    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 1st October 2014

      SUBROUTINE CUTEST_csjprod_r( status, n, m, gotj, jtrans, X,              &
                                   nnz_vector, INDEX_nz_vector,                &
                                   VECTOR, lvector,                            &
                                   nnz_result, INDEX_nz_result,                &
                                   RESULT, lresult )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, nnz_vector, lvector, lresult
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, nnz_result
      LOGICAL, INTENT( IN ) :: gotj, jtrans
      INTEGER ( KIND = ip_ ), DIMENSION( nnz_vector ),                         &
                              INTENT( IN ) :: INDEX_nz_vector
      INTEGER ( KIND = ip_ ), DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( lvector ) :: VECTOR
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lresult ) :: RESULT

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

      CALL CUTEST_csjprod_threadsafe_r( CUTEST_data_global,                    &
                                        CUTEST_work_global( 1 ),               &
                                        status, n, m, gotj, jtrans, X,         &
                                        nnz_vector, INDEX_nz_vector,           &
                                        VECTOR, lvector,                       &
                                        nnz_result, INDEX_nz_result,           &
                                        RESULT, lresult )
      RETURN

!  end of subroutine CUTEST_csjprod_r

      END SUBROUTINE CUTEST_csjprod_r

!-*-*-  C U T E S T   C S J P R O D _ t h r e a d e d   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 1st October 2014

      SUBROUTINE CUTEST_csjprod_threaded_r( status, n, m, gotj, jtrans, X,     &
                                            nnz_vector, INDEX_nz_vector,       &
                                            VECTOR, lvector,                   &
                                            nnz_result, INDEX_nz_result,       &
                                            RESULT, lresult, thread )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, nnz_vector
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: lvector, lresult, thread
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, nnz_result
      LOGICAL, INTENT( IN ) :: gotj, jtrans
      INTEGER ( KIND = ip_ ), DIMENSION( nnz_vector ),                         &
                              INTENT( IN ) :: INDEX_nz_vector
      INTEGER ( KIND = ip_ ), DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( lvector ) :: VECTOR
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lresult ) :: RESULT

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

      CALL CUTEST_csjprod_threadsafe_r( CUTEST_data_global,                    &
                                        CUTEST_work_global( thread ),          &
                                        status, n, m, gotj, jtrans, X,         &
                                        nnz_vector, INDEX_nz_vector,           &
                                        VECTOR, lvector,                       &
                                        nnz_result, INDEX_nz_result,           &
                                        RESULT, lresult )
      RETURN

!  end of subroutine CUTEST_csjprod_threaded_r

      END SUBROUTINE CUTEST_csjprod_threaded_r

!-*-  C U T E S T   C S J P R O D _ t h r e a d s a f e   S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Ingrid Bongartz and Nick Gould

!  History -
!   fortran 77 version originally released as CPROD in CUTE, November 1991
!   fortran 2003 version released in CUTEst, 1st October 2014

      SUBROUTINE CUTEST_csjprod_threadsafe_r( data, work, status, n, m,        &
                                              gotj, jtrans, X,                 &
                                              nnz_vector, INDEX_nz_vector,     &
                                              VECTOR, lvector,                 &
                                              nnz_result, INDEX_nz_result,     &
                                              RESULT, lresult )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, nnz_vector, lvector, lresult
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, nnz_result
      LOGICAL, INTENT( IN ) :: gotj, jtrans
      INTEGER ( KIND = ip_ ), DIMENSION( nnz_vector ),                         &
                              INTENT( IN ) :: INDEX_nz_vector
      INTEGER ( KIND = ip_ ), DIMENSION( MAX( m, n ) ),                        &
                              INTENT( OUT ) :: INDEX_nz_result
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( lvector ) :: VECTOR
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lresult ) :: RESULT

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

      INTEGER ( KIND = ip_ ) :: i, icon, ifstat, igstat, ig, ig1, ii, iel, iv
      INTEGER ( KIND = ip_ ) :: j, k, l, nvarel, nin
      REAL ( KIND = rp_ ) :: ftt, pi, prod, scalee
      REAL :: time_in, time_out
      LOGICAL :: skip

      IF ( work%record_times ) CALL CPU_TIME( time_in )
      IF ( data%numcon == 0 ) GO TO 990

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

        CALL ELFUN_r( work%FUVALS, X, data%EPVALU, data%nel, data%ITYPEE,      &
                      data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,      &
                      data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,       &
                      data%lelvar, data%lntvar, data%lstadh, data%lstep,       &
                      data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,      &
                      1, ifstat )
      IF ( ifstat /= 0 ) GO TO 930

!  evaluate the element function values

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
            ftt = ftt + data%ESCALE( j ) * work%FUVALS( data%IELING( J ) )
          END DO
          work%FT( ig ) = ftt

!  record the derivatives of trivial groups

          IF ( data%GXEQX( ig ) ) work%GVALS( ig, 2 ) = 1.0_rp_
        END DO

!  evaluate the group derivative values

        IF ( .NOT. data%altriv ) THEN
          CALL GROUP_r( work%GVALS, data%ng, work%FT, data%GPVALU, data%ng,    &
                        data%ITYPEG, data%ISTGP, work%ICALCF, data%ltypeg,     &
                        data%lstgp, data%lcalcf, data%lcalcg, data%lgpvlu,     &
                        .TRUE., igstat )
         IF ( igstat /= 0 ) GO TO 930
        END IF
      END IF

      nnz_result = 0

!  form the product result = J(transpose) vector

      IF ( jtrans ) THEN

!  consider the ig-th group

        DO iv = 1, nnz_vector
          icon = INDEX_nz_vector( iv )
          pi = VECTOR( icon )
          ig = data%CGROUP( icon )
          ig1 = ig + 1

!  compute the product of vector(i) with the (scaled) group derivative

          IF ( data%GXEQX( ig ) ) THEN
            prod = VECTOR( icon ) * data%GSCALE( ig )
          ELSE
            prod = VECTOR( icon ) * data%GSCALE( ig ) * work%GVALS( ig, 2 )
          END IF

!  loop over the group's nonlinear elements

          DO ii = data%ISTADG( ig ), data%ISTADG( ig1 ) - 1
            iel = data%IELING( ii )
            k = data%INTVAR( iel )
            l = data%ISTAEV( iel )
            nvarel = data%ISTAEV( iel + 1 ) - l
            scalee = data%ESCALE( ii ) * prod

!  the iel-th element has an internal representation

            IF ( data%INTREP( iel ) ) THEN
              nin = data%INTVAR( iel + 1 ) - k
              CALL RANGE_r( iel, .TRUE., work%FUVALS( k ), work%W_el,          &
                          nvarel, nin, data%ITYPEE( iel ), nin, nvarel )
              DO i = 1, nvarel
                j = data%IELVAR( l )
                IF (  work%IUSED( j ) == 0 ) THEN
                  RESULT( j ) = scalee * work%W_el( i )
                  work%IUSED( j ) = 1
                  nnz_result = nnz_result + 1
                  INDEX_nz_result( nnz_result ) = j
                ELSE
                  RESULT( j ) = RESULT( j ) + scalee * work%W_el( i )
                END IF
                l = l + 1
              END DO

!  the iel-th element has no internal representation

            ELSE
              DO i = 1, nvarel
                j = data%IELVAR( l )
                IF (  work%IUSED( j ) == 0 ) THEN
                  RESULT( j ) = scalee * work%FUVALS( k )
                  work%IUSED( j ) = 1
                  nnz_result = nnz_result + 1
                  INDEX_nz_result( nnz_result ) = j
                ELSE
                  RESULT( j ) = RESULT( j ) + scalee * work%FUVALS( k )
                END IF
                k = k + 1 ; l = l + 1
               END DO
            END IF
          END DO

!  include the contribution from the linear element

          DO k = data%ISTADA( ig ), data%ISTADA( ig1 ) - 1
            j = data%ICNA( k )
            IF (  work%IUSED( j ) == 0 ) THEN
              RESULT( j ) = data%A( k ) * prod
              work%IUSED( j ) = 1
              nnz_result = nnz_result + 1
              INDEX_nz_result( nnz_result ) = j
            ELSE
              RESULT( j ) = RESULT( j ) + data%A( k ) * prod
            END IF
          END DO
        END DO

!  reset IUSED to zero

       work%IUSED( INDEX_nz_result( : nnz_result ) ) = 0

!  form the product result = J vector

      ELSE

!  record nonzero components of vector in IUSED

         work%IUSED( INDEX_nz_vector( : nnz_vector ) ) = 1

!  consider each constrant group in turn

        DO icon = 1, data%numcon
          ig = data%CGROUP( icon )
          ig1 = ig + 1

!  check whether there is a nonzero product 

          skip = .TRUE.
          DO i = data%ISTAGV( ig ), data%ISTAGV( ig + 1 ) - 1
            IF ( work%IUSED( data%ISVGRP( i ) ) == 0 ) CYCLE
            skip = .FALSE.
            EXIT
          END DO
          IF ( skip ) CYCLE

!  compute the first derivative of the group

          prod = 0.0_rp_

!  loop over the group's nonlinear elements

          DO ii = data%ISTADG( ig ), data%ISTADG( ig1 ) - 1
            iel = data%IELING( ii )
            k = data%INTVAR( iel )
            l = data%ISTAEV( iel )
            nvarel = data%ISTAEV( iel + 1 ) - l
            scalee = data%ESCALE( ii )

!  the iel-th element has an internal representation

            IF ( data%INTREP( iel ) ) THEN
              nin = data%INTVAR( iel + 1 ) - k
              CALL RANGE_r( iel, .TRUE., work%FUVALS( k ), work%W_el,          &
                            nvarel, nin, data%ITYPEE( iel ), nin, nvarel )
              DO i = 1, nvarel
               IF ( work%IUSED( data%IELVAR( l ) ) == 1 ) prod = prod          &
                  + VECTOR( data%IELVAR( l ) ) * scalee * work%W_el( i )
                l = l + 1
              END DO

!  the iel-th element has no internal representation

              ELSE
              DO i = 1, nvarel
               IF ( work%IUSED( data%IELVAR( l ) ) == 1 ) prod = prod          &
                  + VECTOR( data%IELVAR( l ) ) * scalee * work%FUVALS( k )
                k = k + 1 ; l = l + 1
              END DO
            END IF
          END DO

!  include the contribution from the linear element

          DO k = data%ISTADA( ig ), data%ISTADA( ig1 ) - 1
            IF ( work%IUSED( data%ICNA( k ) ) == 1 ) prod = prod               &
               + VECTOR( data%ICNA( k ) ) * data%A( k )
          END DO

!  multiply the product by the (scaled) group derivative

          IF ( data%GXEQX( ig ) ) THEN
            RESULT( icon ) = prod * data%GSCALE( ig )
          ELSE
            RESULT( icon ) = prod * data%GSCALE( ig ) * work%GVALS( ig, 2 )
          END IF
          nnz_result = nnz_result + 1
          INDEX_nz_result( nnz_result ) = icon
        END DO

!  reset IUSED to zero

         work%IUSED( INDEX_nz_vector( : nnz_vector ) ) = 0

      END IF

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

!  end of subroutine CUTEST_csjprod_threadsafe_r

      END SUBROUTINE CUTEST_csjprod_threadsafe_r

! THIS VERSION: CUTEST 1.4 - 26/02/2016 AT 08:00 GMT.

!-*-*-*-*-  C U T E S T   C I N T _ C S H P R O D    S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 3rd September 2014

      SUBROUTINE CUTEST_Cint_cshprod( status, n, m, goth, X, Y,                &
                                      nnz_vector, INDEX_nz_vector, VECTOR,     &
                                      nnz_result, INDEX_nz_result, RESULT )
      USE CUTEST
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER, INTENT( IN ) :: n, m, nnz_vector
      INTEGER, INTENT( OUT ) :: status, nnz_result
      LOGICAL ( KIND = C_Bool ), INTENT( IN ) :: goth
      INTEGER, DIMENSION( nnz_vector ), INTENT( IN ) :: INDEX_nz_vector
      INTEGER, DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( m ) :: Y
      REAL ( KIND = wp ), INTENT( OUT ), DIMENSION( n ) :: RESULT

!  -----------------------------------------------------------------------
!  compute the matrix-vector product between the Hessian matrix of
!  the Lagrangian function for the problem and a given sparse vector
!  VECTOR. The result is placed in RESULT. If goth is .TRUE. the second
!  derivatives are assumed to have already been computed. If the user is
!  unsure, set goth = .FALSE. the first time a product is required with
!  the Hessian evaluated at X and Y. X and Y are not used if goth = .TRUE.
!  Only the components INDEX_nz_vector(1:nnz_vector) of VECTOR are nonzero,
!  and the remaining components of VECTOR need not have been be set. On
!  exit, only the components INDEX_nz_result(1:nnz_result) of RESULT are
!  nonzero, and the remaining components of RESULT may not have been set.
!  -----------------------------------------------------------------------

      LOGICAL :: goth_fortran

      goth_fortran = goth
      CALL CUTEST_cshprod( status, n, m, goth_fortran, X, Y,                   &
                           nnz_vector, INDEX_nz_vector, VECTOR,                &
                           nnz_result, INDEX_nz_result, RESULT )

      RETURN

!  end of subroutine CUTEST_Cint_cshprod

      END SUBROUTINE CUTEST_Cint_cshprod

!-*-*-*-*-*-*-  C U T E S T    C S H P R O D    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 3rd September 2014

      SUBROUTINE CUTEST_cshprod( status, n, m, goth, X, Y,                     &
                                 nnz_vector, INDEX_nz_vector, VECTOR,          &
                                 nnz_result, INDEX_nz_result, RESULT )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER, INTENT( IN ) :: n, m, nnz_vector
      INTEGER, INTENT( OUT ) :: status, nnz_result
      LOGICAL, INTENT( IN ) :: goth
      INTEGER, DIMENSION( nnz_vector ), INTENT( IN ) :: INDEX_nz_vector
      INTEGER, DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( m ) :: Y
      REAL ( KIND = wp ), INTENT( OUT ), DIMENSION( n ) :: RESULT

!  -----------------------------------------------------------------------
!  compute the matrix-vector product between the Hessian matrix of
!  the Lagrangian function for the problem and a given sparse vector
!  VECTOR. The result is placed in RESULT. If goth is .TRUE. the second
!  derivatives are assumed to have already been computed. If the user is
!  unsure, set goth = .FALSE. the first time a product is required with
!  the Hessian evaluated at X and Y. X and Y are not used if goth = .TRUE.
!  Only the components INDEX_nz_vector(1:nnz_vector) of VECTOR are nonzero,
!  and the remaining components of VECTOR need not have been be set. On
!  exit, only the components INDEX_nz_result(1:nnz_result) of RESULT are
!  nonzero, and the remaining components of RESULT may not have been set.
!  -----------------------------------------------------------------------

      CALL CUTEST_cshprod_threadsafe( CUTEST_data_global,                      &
                                      CUTEST_work_global( 1 ),                 &
                                      status, n, m, goth, X, Y,                &
                                      nnz_vector, INDEX_nz_vector, VECTOR,     &
                                      nnz_result, INDEX_nz_result, RESULT )
      RETURN

!  end of subroutine CUTEST_cshprod

      END SUBROUTINE CUTEST_cshprod

!-*-*-  C U T E S T   C S H P R O D _ t h r e a d e d   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 3rd September 2014

      SUBROUTINE CUTEST_cshprod_threaded( status, n, m, goth, X, Y,            &
                                          nnz_vector, INDEX_nz_vector, VECTOR, &
                                          nnz_result, INDEX_nz_result, RESULT, &
                                          thread )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER, INTENT( IN ) :: n, m, nnz_vector, thread
      INTEGER, INTENT( OUT ) :: status, nnz_result
      LOGICAL, INTENT( IN ) :: goth
      INTEGER, DIMENSION( nnz_vector ), INTENT( IN ) :: INDEX_nz_vector
      INTEGER, DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( m ) :: Y
      REAL ( KIND = wp ), INTENT( OUT ), DIMENSION( n ) :: RESULT

!  -----------------------------------------------------------------------
!  compute the matrix-vector product between the Hessian matrix of
!  the Lagrangian function for the problem and a given sparse vector
!  VECTOR. The result is placed in RESULT. If goth is .TRUE. the second
!  derivatives are assumed to have already been computed. If the user is
!  unsure, set goth = .FALSE. the first time a product is required with
!  the Hessian evaluated at X and Y. X and Y are not used if goth = .TRUE.
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

      CALL CUTEST_cshprod_threadsafe( CUTEST_data_global,                      &
                                      CUTEST_work_global( thread ),            &
                                      status, n, m, goth, X, Y,                &
                                      nnz_vector, INDEX_nz_vector, VECTOR,     &
                                      nnz_result, INDEX_nz_result, RESULT )
      RETURN

!  end of subroutine CUTEST_cshprod_threaded

      END SUBROUTINE CUTEST_cshprod_threaded

!-*-  C U T E S T   C S H P R O D _ t h r e a d s a f e   S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Ingrid Bongartz and Nick Gould

!  History -
!   fortran 77 version originally released as CPROD in CUTE, November 1991
!   fortran 2003 version released in CUTEst, 3rd September 2014

      SUBROUTINE CUTEST_cshprod_threadsafe( data, work, status, n, m, goth,    &
                                            X, Y, nnz_vector, INDEX_nz_vector, &
                                            VECTOR, nnz_result,                &
                                            INDEX_nz_result, RESULT )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER, INTENT( IN ) :: n, m, nnz_vector
      INTEGER, INTENT( OUT ) :: status, nnz_result
      LOGICAL, INTENT( IN ) :: goth
      INTEGER, DIMENSION( nnz_vector ), INTENT( IN ) :: INDEX_nz_vector
      INTEGER, DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
      REAL ( KIND = wp ), INTENT( IN ), DIMENSION( m ) :: Y
      REAL ( KIND = wp ), INTENT( OUT ), DIMENSION( n ) :: RESULT

!  -----------------------------------------------------------------------
!  compute the matrix-vector product between the Hessian matrix of
!  the Lagrangian function for the problem and a given sparse vector
!  VECTOR. The result is placed in RESULT. If goth is .TRUE. the second
!  derivatives are assumed to have already been computed. If the user is
!  unsure, set goth = .FALSE. the first time a product is required with
!  the Hessian evaluated at X and Y. X and Y are not used if goth = .TRUE.
!  Only the components INDEX_nz_vector(1:nnz_vector) of VECTOR are nonzero,
!  and the remaining components of VECTOR need not have been be set. On
!  exit, only the components INDEX_nz_result(1:nnz_result) of RESULT are
!  nonzero, and the remaining components of RESULT may not have been set.
!  -----------------------------------------------------------------------

!  local variables

      INTEGER :: i, ig, j, ifstat, igstat
      REAL ( KIND = wp ) :: ftt
      REAL :: time_in, time_out
      EXTERNAL :: RANGE

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  there are non-trivial group functions

      IF ( .NOT. goth ) THEN
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
            IF ( i == 0 ) THEN
              work%GSCALE_used( ig ) = data%GSCALE( ig )
            ELSE
              work%GSCALE_used( ig ) = data%GSCALE( ig ) * Y( i )
            END IF
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

!  evaluate the product

      work%nbprod = work%nbprod + 1
      IF ( data%numcon > 0 ) THEN
        CALL CUTEST_hessian_times_sp_vector(                                   &
            data%n, data%ng, data%nel, data%ntotel, data%nvrels, data%nvargp,  &
            1, nnz_vector, nnz_result, work%nbprod, data%alllin,               &
            INDEX_nz_vector, data%ISTAEV, data%ISTADH, data%INTVAR,            &
            data%IELING, data%IELVAR, work%ISWKSP, INDEX_nz_result,            &
            VECTOR, RESULT, work%GVALS( : , 2 ) , work%GVALS( : , 3 ),         &
            work%FUVALS( data%lgrjac + 1 ), work%GSCALE_used, data%ESCALE,     &
            work%FUVALS( : data%lnhuvl ), data%lnhuvl, data%GXEQX,             &
            data%INTREP, data%IGCOLJ, data%ISLGRP, data%ISVGRP, data%ISTAGV,   &
            data%IVALJR, data%ITYPEE, data%ISYMMH, work%ISTAJC, work%IUSED,    &
            data%LIST_elements, data%LINK_elem_uses_var, work%NZ_components_w, &
            work%W_ws, work%W_el, work%W_in, work%H_in, RANGE )
      ELSE
        CALL CUTEST_hessian_times_sp_vector(                                   &
            data%n, data%ng, data%nel, data%ntotel, data%nvrels, data%nvargp,  &
            1, nnz_vector, nnz_result, work%nbprod, data%alllin,               &
            INDEX_nz_vector, data%ISTAEV, data%ISTADH, data%INTVAR,            &
            data%IELING, data%IELVAR, work%ISWKSP, INDEX_nz_result,            &
            VECTOR, RESULT, work%GVALS( : , 2 ) , work%GVALS( : , 3 ),         &
            work%FUVALS( data%lgrjac + 1 ), data%GSCALE, data%ESCALE,          &
            work%FUVALS( : data%lnhuvl ), data%lnhuvl, data%GXEQX,             &
            data%INTREP, data%IGCOLJ, data%ISLGRP, data%ISVGRP, data%ISTAGV,   &
            data%IVALJR, data%ITYPEE, data%ISYMMH, work%ISTAJC, work%IUSED,    &
            data%LIST_elements, data%LINK_elem_uses_var, work%NZ_components_w, &
            work%W_ws, work%W_el, work%W_in, work%H_in, RANGE )
      END IF

!  update the counters for the report tool

      work%nhvpr = work%nhvpr + 1
      IF ( .NOT. goth ) THEN
        work%nc2oh = work%nc2oh + 1
        work%nc2ch = work%nc2ch + work%pnc
      END IF
      GO TO 990

!  unsuccessful returns

  930 CONTINUE
      IF ( data%out > 0 ) WRITE( data%out,                                     &
        "( ' ** SUBROUTINE CSHPROD: error flag raised during SIF evaluation')" )
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_cshprod = work%time_cshprod + time_out - time_in
      END IF
      status = 0
      RETURN

!  end of subroutine CUTEST_cshprod_threadsafe

      END SUBROUTINE CUTEST_cshprod_threadsafe



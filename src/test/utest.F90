! THIS VERSION: CUTEST 2.3 - 2024-10-24 AT 07:10 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!- C U T E S T  t e s t _ u n c o n s t r a i n e d _ t o o l s  P R O G R A M -

    PROGRAM CUTEST_test_unconstrained_tools

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released November 2012

      USE CUTEST_KINDS_precision
      USE CUTEST_INTERFACE_precision

!----------------------
!   P a r a m e t e r s
!----------------------

      INTEGER ( KIND = ip_ ), PARAMETER :: input = 55
      INTEGER ( KIND = ip_ ), PARAMETER :: out = 6
      INTEGER ( KIND = ip_ ), PARAMETER :: buffer = 77
      REAL ( KIND = rp_ ), PARAMETER :: zero = 0.0_rp_
      REAL ( KIND = rp_ ), PARAMETER :: one = 1.0_rp_

!--------------------------------
!   L o c a l   V a r i a b l e s
!--------------------------------

      INTEGER ( KIND = ip_ ) :: n, HE_nel, HE_val_ne, HE_row_ne, status
      INTEGER ( KIND = ip_ ) :: l_h2_1, l_h, lhe_ptr, H_ne, lhe_val, lhe_row
      INTEGER ( KIND = ip_ ) :: nnz_vector, nnz_result, maxsbw, alloc_stat
      INTEGER ( KIND = ip_ ) :: nsemib, lbandh
      REAL ( KIND = rp_ ) :: f
      LOGICAL :: grad, byrows, goth
      CHARACTER ( len = 10 ) ::  p_name
      CHARACTER ( len = 30 ) :: classification
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: X_type
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: H_row, H_col
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: HE_row, HE_row_ptr
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: HE_val_ptr
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: INDEX_nz_vector
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: INDEX_nz_result
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, X_l, X_u, G
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: H_val, HE_val
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: VECTOR, RESULT
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : , : ) :: H2_val, H_band
      CHARACTER ( len = 10 ), ALLOCATABLE, DIMENSION( : ) :: X_names
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 4 )

!  open the problem data file

      OPEN ( input, FILE = 'u_OUTSDIF.d', FORM = 'FORMATTED', STATUS = 'OLD' )

!  allocate basic arrays

      WRITE( out, "( ' Call CUTEST_udimen ' )" )
      CALL CUTEST_udimen_r( status, input, n )
      WRITE( out, "( ' * n = ', I0 )" ) n
      l_h2_1 = n
      ALLOCATE( X( n ), X_l( n ), X_u( n ), G( n ), VECTOR( n ), RESULT( n ),  &
                X_names( n ), X_type( n ), INDEX_nz_vector( n ),               &
                INDEX_nz_result( n ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990
      ALLOCATE( H2_val( l_h2_1, n ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

!  obtain the classification

      WRITE( out, "( ' CALL CUTEST_classification ' )" )
      CALL CUTEST_classification_r( status, input, classification )
      IF ( status == 0 ) THEN
        WRITE( out, "( ' classification is ', A )" ) TRIM( classification )
      ELSE
        WRITE( out, "( ' no compatible SIF file in current directory' )" ) 
      END IF

!  set up SIF data

      WRITE( out, "( ' Call CUTEST_usetup ' )" )
      CALL CUTEST_usetup_r( status, input, out, buffer, n, X, X_l, X_u )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_X( out, n, X, X_l, X_u )

!X = (/ 1.1_wp, 2.2_wp, 3.3_wp, 4.4_rp_ /)

!  obtain variable and problem names

      WRITE( out, "( ' Call CUTEST_unames' )" )
      CALL CUTEST_unames_r( status, n, p_name, X_names )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_p_name( out, p_name )
      CALL WRITE_X_names( out, n, X_names )

!  obtain problem name

      WRITE( out, "( ' Call CUTEST_probname' )" )
      CALL CUTEST_probname_r( status, p_name )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_p_name( out, p_name )

!  obtain variable names

      WRITE( out, "( ' Call CUTEST_varnames' )" )
      CALL CUTEST_varnames_r( status, n, X_names )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_X_names( out, n, X_names )

!  obtain variable types

      WRITE( out, "( ' Call CUTEST_uvartype' )" )
      CALL CUTEST_uvartype_r( status, n, X_type )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_X_type( out, n, X_type )

!  compute the objective function value

      WRITE( out, "( ' Call CUTEST_ufn' )" )
      CALL CUTEST_ufn_r( status, n, X, f )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_f( out, f )

!  compute the gradient value

      WRITE( out, "( ' Call CUTEST_ugr' )" )
      CALL CUTEST_ugr_r( status, n, X, G )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_G( out, n, G )

!  compute the objective function and gradient values

      grad = .TRUE.
      WRITE( out, "( ' Call CUTEST_uofg with grad = .TRUE.' )" )
      CALL CUTEST_uofg_r( status, n, X, f, G, grad )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_f( out, f )
      CALL WRITE_G( out, n, G )

      grad = .FALSE.
      WRITE( out, "( ' Call CUTEST_uofg with grad = .FALSE.' )" )
      CALL CUTEST_uofg_r( status, n, X, f, G, grad )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_f( out, f )

!  compute the dense Hessian value

      WRITE( out, "( ' Call CUTEST_udh' )" )
      CALL CUTEST_udh_r( status, n, X, l_h2_1, H2_val )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )

!  compute the gradient and dense Hessian values

      WRITE( out, "( ' Call CUTEST_ugrdh' )" )
      CALL CUTEST_ugrdh_r( status, n, X, G, l_h2_1, H2_val )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_G( out, n, G )
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )

!  compute the number of nonzeros in the sparse Hessian

      WRITE( out, "( ' Call CUTEST_udimsh' )" )
      CALL CUTEST_udimsh_r( status, H_ne )
      IF ( status /= 0 ) GO to 900
      WRITE( out, "( ' * H_ne = ', I0 )" ) H_ne

      l_h = H_ne
      ALLOCATE( H_val( l_h ), H_row( l_h ), H_col( l_h ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

!  compute the sparsity pattern of the Hessian

      WRITE( out, "( ' Call CUTEST_ushp' )" )
      CALL CUTEST_ushp_r( status, n, H_ne, l_h, H_row, H_col )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_H_sparsity_pattern( out, H_ne, l_h, H_row, H_col )

!  compute the sparse Hessian value

      WRITE( out, "( ' Call CUTEST_ush' )" )
      CALL CUTEST_ush_r( status, n, X, H_ne, l_h, H_val, H_row, H_col )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )

!  compute the gradient and sparse Hessian values

      WRITE( out, "( ' Call CUTEST_ugrsh' )" )
      CALL CUTEST_ugrsh_r( status, n, X, G, H_ne, l_h, H_val, H_row, H_col )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_G( out, n, G )
      CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )

!  compute the number of nonzeros in the element Hessian

      WRITE( out, "( ' Call CUTEST_udimse' )" )
      CALL CUTEST_udimse_r( status, HE_nel, HE_val_ne, HE_row_ne )
      IF ( status /= 0 ) GO to 900
      WRITE( out, "( ' * H_nel = ', I0, ' HE_val_ne = ', I0,                   &
     &                 ' HE_row_ne = ', I0 )" ) HE_nel, HE_val_ne, HE_row_ne

      lhe_ptr = HE_nel + 1
      lhe_val = HE_val_ne
      lhe_row = HE_row_ne
      ALLOCATE( HE_row_ptr( lhe_ptr ), HE_val_ptr( lhe_ptr ),                  &
                HE_row( lhe_row ), HE_val( lhe_val ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

!  compute the element Hessian value

      byrows = .FALSE.
      WRITE( out, "( ' Call CUTEST_ueh with byrows = .FALSE.' )" )
      CALL CUTEST_ueh_r( status, n, X, HE_nel, lhe_ptr, HE_row_ptr,            &
                HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val, byrows )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )
      byrows = .TRUE.
      WRITE( out, "( ' Call CUTEST_ueh with byrows = .TRUE.' )" )
      CALL CUTEST_ueh_r( status, n, X, HE_nel, lhe_ptr, HE_row_ptr,            &
                HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val, byrows )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )

!  compute the gradient and element Hessian values

      byrows = .FALSE.
      WRITE( out, "( ' Call CUTEST_ugreh with byrows = .FALSE' )" )
      CALL CUTEST_ugreh_r( status, n, X, G, HE_nel, lhe_ptr, HE_row_ptr,       &
                  HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val, byrows )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_G( out, n, G )
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )
      byrows = .TRUE.
      WRITE( out, "( ' Call CUTEST_ugreh with byrows = .TRUE.' )" )
      CALL CUTEST_ugreh_r( status, n, X, G, HE_nel, lhe_ptr, HE_row_ptr,       &
                  HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val, byrows )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_G( out, n, G )
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )

!  compute a Hessian-vector product

      VECTOR( 1 ) = one ; VECTOR( 2 : n ) = zero
      goth = .FALSE.
      WRITE( out, "( ' Call CUTEST_uhprod with goth = .FALSE.' )" )
      CALL CUTEST_uhprod_r( status, n, goth, X, VECTOR, RESULT )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_RESULT( out, n, VECTOR, RESULT )
      goth = .TRUE.
      WRITE( out, "( ' Call CUTEST_uhprod with goth = .TRUE.' )" )
      CALL CUTEST_uhprod_r( status, n, goth, X, VECTOR, RESULT )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_RESULT( out, n, VECTOR, RESULT )

!  compute a sparse Hessian-vector product

      nnz_vector = 1 ; INDEX_nz_vector( nnz_vector ) = 1
      goth = .FALSE.
      WRITE( out, "( ' Call CUTEST_ushprod with goth = .FALSE.' )" )
      CALL CUTEST_ushprod_r( status, n, goth, X,                               &
                           nnz_vector, INDEX_nz_vector, VECTOR,                &
                           nnz_result, INDEX_nz_result, RESULT )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_SRESULT( out, n, nnz_vector, INDEX_nz_vector, VECTOR,         &
                          nnz_result, INDEX_nz_result, RESULT )

      goth = .TRUE.
      WRITE( out, "( ' Call CUTEST_ushprod with goth = .TRUE.' )" )
      CALL CUTEST_ushprod_r( status, n, goth, X,                               &
                           nnz_vector, INDEX_nz_vector, VECTOR,                &
                           nnz_result, INDEX_nz_result, RESULT )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_SRESULT( out, n, nnz_vector, INDEX_nz_vector, VECTOR,         &
                          nnz_result, INDEX_nz_result, RESULT )

!  compute a band of the Hessian

      nsemib = n / 2
      lbandh = nsemib
      ALLOCATE( H_band( 0 : lbandh, n ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

      WRITE( out, "( ' Call CUTEST_ubandh' )" )
      CALL CUTEST_ubandh_r( status, n, X, nsemib, H_band, lbandh, maxsbw )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_H_BAND( out, n, lbandh, H_band, nsemib )

!  calls and time report

      WRITE( out, "( ' CALL CUTEST_ureport' )" )
      CALL CUTEST_ureport_r( status, CALLS, CPU )
      WRITE( out, "( ' CALLS(1-4) =', 4( 1X, I0 ) )" ) INT( CALLS( 1 : 4 ) )
      WRITE( out, "( ' CPU(1-4) =', 4F7.2 )" ) CPU( 1 : 4 )

!  terminal exit

      WRITE( out, "( ' Call CUTEST_uterminate' )" )
      CALL CUTEST_uterminate_r( status )
      IF ( status /= 0 ) GO to 900

!  one more setup ...

      WRITE( out, "( ' Call CUTEST_usetup ' )" )
      CALL CUTEST_usetup_r( status, input, out, buffer, n, X, X_l, X_u )
      IF ( status /= 0 ) GO to 900

!  ... and terminal exit

      WRITE( out, "( ' Call CUTEST_uterminate' )" )
      CALL CUTEST_uterminate_r( status )
      IF ( status /= 0 ) GO to 900

      DEALLOCATE( X_type, H_row, H_col, HE_row, HE_row_ptr, HE_val_ptr, X,     &
                  X_l, X_u, G, H_val, HE_val, VECTOR, RESULT, H2_val, H_band,  &
                  X_names, INDEX_nz_vector, INDEX_nz_result, stat = alloc_stat )
      CLOSE( input )
      STOP

!  error exits

 900  CONTINUE
      WRITE( out, "( ' error status = ', I0 )" ) status
      CLOSE( INPUT  )
      STOP

 990  CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) alloc_stat
      CLOSE( INPUT  )
      STOP

    CONTAINS

!  data printing subroutines

      SUBROUTINE WRITE_X( out, n, X, X_l, X_u )
      INTEGER ( KIND = ip_ ) :: n, out
      REAL ( KIND = rp_ ), DIMENSION( n ) :: X, X_l, X_u
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' *       i      X_l          X          X_u' )" )
      DO i = 1, n
        WRITE( out, "( ' * ', I7, 3ES12.4 )" ) i, X_l( i ), X( i ), X_u( i )
      END DO
      END SUBROUTINE WRITE_X

      SUBROUTINE WRITE_X_type( out, n, X_type )
      INTEGER ( KIND = ip_ ) :: n, out
      INTEGER ( KIND = ip_ ), DIMENSION( n ) :: X_type
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' *       i  X_type' )" )
      DO i = 1, n
        WRITE( out, "( ' * ', I7, 2X, I0 )" ) i, X_type( i )
      END DO
      END SUBROUTINE WRITE_X_type

      SUBROUTINE WRITE_p_name( out, p_name )
      INTEGER ( KIND = ip_ ) :: out
      CHARACTER ( len = 10 ) ::  p_name
      WRITE( out, "( ' * p_name = ', A )" ) p_name
      END SUBROUTINE WRITE_p_name

      SUBROUTINE WRITE_X_names( out, n, X_names )
      INTEGER ( KIND = ip_ ) :: n, out
      CHARACTER ( len = 10 ), DIMENSION( n ) :: X_names
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' *       i  X_name' )" )
      DO i = 1, n
        WRITE( out, "( ' * ', I7, 2X, A10 )" ) i, X_names( i )
      END DO
      END SUBROUTINE WRITE_X_names

      SUBROUTINE WRITE_f( out, f )
      INTEGER ( KIND = ip_ ) :: out
      REAL ( KIND = rp_ ) :: f
      WRITE( out, "( ' * f = ', ES12.4 )" ) f
      END SUBROUTINE WRITE_f

      SUBROUTINE WRITE_G( out, n, G )
      INTEGER ( KIND = ip_ ) :: n, out
      REAL ( KIND = rp_ ), DIMENSION( n ) :: G
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' *       i       G' )" )
      DO i = 1, n
        WRITE( out, "( ' * ', I7, ES12.4 )" ) i, G( i )
      END DO
      END SUBROUTINE WRITE_G

      SUBROUTINE WRITE_H_dense( out, n, l_h2_1, H2_val )
      INTEGER ( KIND = ip_ ) :: n, l_h2_1, out
      REAL ( KIND = rp_ ), DIMENSION( l_h2_1, n ) :: H2_val
      INTEGER ( KIND = ip_ ) :: i, j
      WRITE( out, "( ' * H(dense)' )" )
      DO j = 1, n, 4
        IF ( j + 3 <= n ) THEN
          WRITE( out, "( ' *       i   j', I8, 3I12 )" ) j, j + 1, j + 2, j + 3
        ELSE IF ( j + 2 <= n ) THEN
          WRITE( out, "( ' *       i   j', I8, 2I12 )" ) j, j + 1, j + 2
        ELSE IF ( j + 1 <= n ) THEN
          WRITE( out, "( ' *       i   j', I8, I12 )" ) j, j + 1
        ELSE
          WRITE( out, "( ' *       i   j', I8 )" ) j
        END IF
        DO i = 1, n
          IF ( j + 3 <= n ) THEN
            WRITE( out, "( ' * ', I7,  4X, 4ES12.4 )" )                        &
              i, H2_val( i, j ), H2_val( i, j + 1 ),                           &
              H2_val( i, j + 2 ), H2_val( i, j + 3 )
          ELSE IF ( j + 2 <= n ) THEN
            WRITE( out, "( ' * ',  I7, 4X, 3ES12.4 )" )                        &
              i, H2_val( i, j ), H2_val( i, j + 1 ), H2_val( i, j + 2 )
          ELSE IF ( j + 1 <= n ) THEN
            WRITE( out, "( ' * ',  I7, 4X, 2ES12.4 )" )                        &
              i, H2_val( i, j ), H2_val( i, j + 1 )
          ELSE
            WRITE( out, "( ' * ',  I7, 4X, ES12.4 )" ) i, H2_val( i, j )
          END IF
        END DO
      END DO
      END SUBROUTINE WRITE_H_dense

      SUBROUTINE WRITE_H_sparsity_pattern( out, H_ne, l_h, H_row, H_col )
      INTEGER ( KIND = ip_ ) :: l_h, H_ne, out
      INTEGER ( KIND = ip_ ), DIMENSION( l_h ) :: H_row, H_col
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' * H(sparse)' )" )
      WRITE( out, "( ' * ', 4( '    row    col' ) )" )
      DO i = 1, H_ne, 4
        IF ( i + 3 <= H_ne ) THEN
          WRITE( out, "( ' * ',  4( 2I7 ) )" )                                 &
            H_row( i ), H_col( i ), H_row( i + 1 ), H_col( i + 1 ),            &
            H_row( i + 2 ), H_col( i + 2 ), H_row( i + 3 ), H_col( i + 3 )
        ELSE IF ( i + 2 <= H_ne ) THEN
          WRITE( out, "( ' * ',  3( 2I7 ) )" )                                 &
            H_row( i ), H_col( i ), H_row( i + 1 ), H_col( i + 1 ),            &
            H_row( i + 2 ), H_col( i + 2 )
        ELSE IF ( i + 1 <= H_ne ) THEN
          WRITE( out, "( ' * ',  2( 2I7 ) )" )                                 &
            H_row( i ), H_col( i ), H_row( i + 1 ), H_col( i + 1 )
        ELSE
          WRITE( out, "( ' * ',  2I7 )" ) H_row( i ), H_col( i )
        END IF
      END DO
      END SUBROUTINE WRITE_H_sparsity_pattern

      SUBROUTINE WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )
      INTEGER ( KIND = ip_ ) :: l_h, H_ne, out
      INTEGER ( KIND = ip_ ), DIMENSION( l_h ) :: H_row, H_col
      REAL ( KIND = rp_ ), DIMENSION( l_h ) :: H_val
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' * H(sparse)' )" )
      WRITE( out, "( ' * ', 2( '    row    col     val    ' ) )" )
      DO i = 1, H_ne, 2
        IF ( i + 1 <= H_ne ) THEN
          WRITE( out, "( ' * ',  2( 2I7, ES12.4 ) )" )                         &
            H_row( i ), H_col( i ), H_val( i ),                                &
            H_row( i + 1 ), H_col( i + 1 ), H_val( i + 1 )
        ELSE
          WRITE( out, "( ' * ',  2( 2I7, ES12.4 ) )" )                         &
            H_row( i ), H_col( i ), H_val( i )
        END IF
      END DO
      END SUBROUTINE WRITE_H_sparse

      SUBROUTINE WRITE_H_element( out, ne, lhe_ptr, HE_row_ptr,                &
                      HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )
      INTEGER ( KIND = ip_ ) :: ne, lhe_ptr, lhe_row, lhe_val, out
      INTEGER ( KIND = ip_ ), DIMENSION( lhe_ptr ) :: HE_row_ptr, HE_val_ptr
      INTEGER ( KIND = ip_ ), DIMENSION( lhe_row ) :: HE_row
      REAL ( KIND = rp_ ), DIMENSION( lhe_val ) :: HE_val
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' * H(element)' )" )
      DO i = 1, ne
        IF (  HE_row_ptr( i + 1 ) > HE_row_ptr( i ) ) THEN
          WRITE( out, "( ' * element ', I0 )" ) i
          WRITE( out, "( ' * indices ', 5I12, /, ( ' *', 9X, 5I12 ) )" )       &
           HE_row( HE_row_ptr( i ) : HE_row_ptr( i + 1 ) - 1 )
          WRITE( out, "( ' * values  ', 5ES12.4, /, ( ' *', 9X, 5ES12.4 ) )" ) &
           HE_val( HE_val_ptr( i ) : HE_val_ptr( i + 1 ) - 1 )
        ELSE
          WRITE( out, "( ' * element ', I0, ' has no indices' )" ) i
        END IF
      END DO
      END SUBROUTINE WRITE_H_element

      SUBROUTINE WRITE_RESULT( out, n, VECTOR, RESULT )
      INTEGER ( KIND = ip_ ) :: n, out
      REAL ( KIND = rp_ ), DIMENSION( n ) :: VECTOR, RESULT
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' *       i    VECTOR     RESULT' )" )
      DO i = 1, n
        WRITE( out, "( ' * ', I7, 2ES12.4 )" ) i, VECTOR( i ), RESULT( i )
      END DO
      END SUBROUTINE WRITE_RESULT

      SUBROUTINE WRITE_SRESULT( out, n, nnz_vector, INDEX_nz_vector, VECTOR,   &
                                nnz_result, INDEX_nz_result, RESULT )
      INTEGER ( KIND = ip_ ) :: n, out,  nnz_vector,  nnz_result
      INTEGER ( KIND = ip_ ), DIMENSION( nnz_vector ) :: INDEX_nz_vector
      INTEGER ( KIND = ip_ ), DIMENSION( n ) :: INDEX_nz_result
      REAL ( KIND = rp_ ), DIMENSION( n ) :: VECTOR, RESULT
      INTEGER ( KIND = ip_ ) :: i, j
      WRITE( out, "( ' *       i    VECTOR' )" )
      DO j = 1, nnz_vector
        i = INDEX_nz_vector( j )
        WRITE( out, "( ' * ', I7, 2ES12.4 )" ) i, VECTOR( i )
      END DO
      WRITE( out, "( ' *       i    RESULT' )" )
      DO j = 1, nnz_result
        i = INDEX_nz_result( j )
        WRITE( out, "( ' * ', I7, 2ES12.4 )" ) i, RESULT( i )
      END DO
      END SUBROUTINE WRITE_SRESULT

!     SUBROUTINE WRITE_H_BAND( out, n, lbandh, H_band, nsemib, maxsbw )
      SUBROUTINE WRITE_H_BAND( out, n, lbandh, H_band, nsemib )
      INTEGER ( KIND = ip_ ) :: n, lbandh, nsemib, out
!     INTEGER ( KIND = ip_ ) :: maxsbw
      REAL ( KIND = rp_ ), DIMENSION( 0 : lbandh, n ):: H_band
      INTEGER ( KIND = ip_ ) :: i, j
      WRITE( out, "( ' * H(band)' )" )
!     WRITE( out, "( ' * H(band) has max seemibandwidth ', I0 )" ) maxsbw
      WRITE( out, "( ' *       i   band', I7, 4I12 )" ) ( j, j = 0, nsemib )
      DO j = 1, n
        WRITE( out, "( ' * ', I7, 6X, 5ES12.4, :, /,                           &
       &               ( ' * ', 7X, 6X, 5ES12.4, : ) )" )                      &
          j, ( H_band( i, j ), i = 0, nsemib )
      END DO
      END SUBROUTINE WRITE_H_BAND

    END PROGRAM CUTEST_test_unconstrained_tools

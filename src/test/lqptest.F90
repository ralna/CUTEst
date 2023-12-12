! THIS VERSION: CUTEST 2.2 - 2023-11-12 AT 15:50 GMT.

#include "cutest_modules.h"

!-*-*-*-*-*-*-*-*- C U T E S T   l q p _ t e s t   P R O G R A M -*-*-*-*-*-*-*-

    PROGRAM CUTEST_lqp_test

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released March 2013

      USE CUTEST_KINDS_precision
      USE CUTEST_LQP_precision

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

      INTEGER ( KIND = ip_ ) :: n, m, H_ne, A_ne, status
      REAL ( KIND = rp_ ) :: f, h_pert
      CHARACTER ( len = 10 ) ::  p_name
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: A_row, A_col, A_ptr
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: H_row, H_col, H_ptr
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, X_l, X_u, Z, G
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: Y, C_l, C_u
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: A_val, H_val
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : , : ) :: A_dense, H_dense
      CHARACTER ( len = 10 ), ALLOCATABLE, DIMENSION( : ) :: X_names, C_names

!  open the problem data file

      OPEN ( input, FILE = 'q_OUTSDIF.d', FORM = 'FORMATTED', STATUS = 'OLD' )

!  dense version

      WRITE( out, "( ' CALL CUTEST_lqp_create (dense) ' )" )
      CALL CUTEST_lqp_create( status, input, buffer, out, n, m, f, G, X, X_l,  &
                              X_u, Z, Y, C_l, C_u, p_name, X_names, C_names,   &
                              A_dense = A_dense, H_dense = H_dense )
      IF ( status /= 0 ) GO TO 900
      WRITE( out, "( ' * n = ', I0, ', m = ', I0,                              &
     &  ', A_ne = ', I0, ', H_ne = ', I0 )" ) n, m, A_ne, H_ne
      CALL WRITE_X( out, n, X, X_l, X_u, Z )
      CALL WRITE_Y( out, m, Y, C_l, C_u )
      CALL WRITE_p_name( out, p_name )
      CALL WRITE_X_names( out, n, X_names )
      CALL WRITE_C_names( out, m, C_names )
      CALL WRITE_f( out, f )
      CALL WRITE_G( out, n, G )
      CALL WRITE_A_dense( out, n, m, m, n, A_dense )
      CALL WRITE_H_dense( out, n, n, H_dense )

      DEALLOCATE( X, X_l, X_u, Z, G, Y, C_l, &
                  A_dense, H_dense, X_names, C_names, stat = status )

!  dense version with perturbation

      WRITE( out, "( ' CALL CUTEST_lqp_create (dense, perturbed) ' )" )
      h_pert = one
      CALL CUTEST_lqp_create( status, input, buffer, out, n, m, f, G, X, X_l,  &
                              X_u, Z, Y, C_l, C_u, p_name, X_names, C_names,   &
                              A_dense = A_dense, H_dense = H_dense,            &
                              H_pert = h_pert )
      IF ( status /= 0 ) GO TO 900
      WRITE( out, "( ' * n = ', I0, ', m = ', I0,                              &
     &  ', A_ne = ', I0, ', H_ne = ', I0 )" ) n, m, A_ne, H_ne
      CALL WRITE_H_dense( out, n, n, H_dense )

      DEALLOCATE( X, X_l, X_u, Z, G, Y, C_l, C_u,                              &
                  A_dense, H_dense, X_names, C_names, stat = status )

!  sparse co-ordinate version

      WRITE( out, "( ' CALL CUTEST_lqp_create (co-ordinate) ' )" )
      h_pert = zero
      CALL CUTEST_lqp_create( status, input, buffer, out, n, m, f, G, X, X_l,  &
                              X_u, Z, Y, C_l, C_u, p_name, X_names, C_names,   &
                              A_ne = A_ne, A_row = A_row,                      &
                              A_col = A_col, A_val = A_val,                    &
                              H_ne = H_ne, H_row = H_row,                      &
                              H_col = H_col, H_val = H_val,                    &
                              H_pert = h_pert )
      IF ( status /= 0 ) GO TO 900
      WRITE( out, "( ' * n = ', I0, ', m = ', I0,                              &
     &  ', A_ne = ', I0, ', H_ne = ', I0 )" ) n, m, A_ne, H_ne
      CALL WRITE_A_sparse( out, A_ne, A_ne, A_val, A_row, A_col )
      CALL WRITE_H_sparse( out, H_ne, H_ne, H_val, H_row, H_col )

      DEALLOCATE( A_row, A_col, H_row, H_col, X, X_l, X_u, Z, G, Y, C_l, C_u,  &
                  A_val, H_val, X_names, C_names, stat = status )

!  sparse co-ordinate version with perturbation

      WRITE( out, "( ' CALL CUTEST_lqp_create (co-ordinate, perturbed) ' )" )
      h_pert = one
      CALL CUTEST_lqp_create( status, input, buffer, out, n, m, f, G, X, X_l,  &
                              X_u, Z, Y, C_l, C_u, p_name, X_names, C_names,   &
                              A_ne = A_ne, A_row = A_row,                      &
                              A_col = A_col, A_val = A_val,                    &
                              H_ne = H_ne, H_row = H_row,                      &
                              H_col = H_col, H_val = H_val,                    &
                              H_pert = h_pert )
      IF ( status /= 0 ) GO TO 900
      WRITE( out, "( ' * n = ', I0, ', m = ', I0,                              &
     &  ', A_ne = ', I0, ', H_ne = ', I0 )" ) n, m, A_ne, H_ne
      CALL WRITE_H_sparse( out, H_ne, H_ne, H_val, H_row, H_col )

      DEALLOCATE( A_row, A_col, H_row, H_col, X, X_l, X_u, Z, G, Y, C_l, C_u,  &
                  A_val, H_val, X_names, C_names, stat = status )

!  sparse by-rows version

      WRITE( out, "( ' CALL CUTEST_lqp_create (by-rows) ' )" )
      CALL CUTEST_lqp_create( status, input, buffer, out, n, m, f, G, X, X_l,  &
                              X_u, Z, Y, C_l, C_u, p_name, X_names, C_names,   &
                              A_ptr = A_ptr, A_col = A_col, A_val = A_val,     &
                              H_ptr = H_ptr, H_col = H_col, H_val = H_val )
      IF ( status /= 0 ) GO TO 900
      WRITE( out, "( ' * n = ', I0, ', m = ', I0, ', A_ne = ', I0,             &
     & ', H_ne = ', I0 )" ) n, m, A_ptr( m + 1 ) - 1, H_ptr( m + 1 ) - 1
      CALL WRITE_A_byrows( out, m, A_val, A_col, A_ptr )
      CALL WRITE_H_byrows( out, n, H_val, H_col, H_ptr )

      DEALLOCATE( A_ptr, A_col, H_ptr, H_col, X, X_l, X_u, Z, G, Y, C_l, C_u,  &
                  A_val, H_val, X_names, C_names, stat = status )

!  sparse by-rows version with perturbation

      WRITE( out, "( ' CALL CUTEST_lqp_create (by-rows, perturbed) ' )" )
      h_pert = one
      CALL CUTEST_lqp_create( status, input, buffer, out, n, m, f, G, X, X_l,  &
                              X_u, Z, Y, C_l, C_u, p_name, X_names, C_names,   &
                              A_ptr = A_ptr, A_col = A_col, A_val = A_val,     &
                              H_ptr = H_ptr, H_col = H_col, H_val = H_val,     &
                              H_pert = h_pert )
      IF ( status /= 0 ) GO TO 900
      WRITE( out, "( ' * n = ', I0, ', m = ', I0, ', A_ne = ', I0,             &
     & ', H_ne = ', I0 )" ) n, m, A_ptr( m + 1 ) - 1, H_ptr( m + 1 ) - 1
      CALL WRITE_H_byrows( out, n, H_val, H_col, H_ptr )

      DEALLOCATE( A_ptr, A_col, H_ptr, H_col, X, X_l, X_u, Z, G, Y, C_l, C_u,  &
                  A_val, H_val, X_names, C_names, stat = status )


!  sparse by-cols version

      WRITE( out, "( ' CALL CUTEST_lqp_create (by-cols) ' )" )
      CALL CUTEST_lqp_create( status, input, buffer, out, n, m, f, G, X, X_l,  &
                              X_u, Z, Y, C_l, C_u, p_name, X_names, C_names,   &
                              A_ptr = A_ptr, A_row = A_row, A_val = A_val,     &
                              H_ptr = H_ptr, H_row = H_row, H_val = H_val )
      IF ( status /= 0 ) GO TO 900
      WRITE( out, "( ' * n = ', I0, ', m = ', I0, ', A_ne = ', I0,             &
     & ', H_ne = ', I0 )" ) n, m, A_ptr( n + 1 ) - 1, H_ptr( m + 1 ) - 1
      CALL WRITE_A_bycols( out, n, A_val, A_row, A_ptr )
      CALL WRITE_H_bycols( out, n, H_val, H_row, H_ptr )

      DEALLOCATE( A_ptr, A_row, H_ptr, H_row, X, X_l, X_u, Z, G, Y, C_l, C_u,  &
                  A_val, H_val, X_names, C_names, stat = status )

!  sparse by-cols version with perturbation

      WRITE( out, "( ' CALL CUTEST_lqp_create (by-cols, perturbed) ' )" )
      h_pert = one
      CALL CUTEST_lqp_create( status, input, buffer, out, n, m, f, G, X, X_l,  &
                              X_u, Z, Y, C_l, C_u, p_name, X_names, C_names,   &
                              A_ptr = A_ptr, A_row = A_row, A_val = A_val,     &
                              H_ptr = H_ptr, H_row = H_row, H_val = H_val,     &
                              H_pert = h_pert )
      IF ( status /= 0 ) GO TO 900
      WRITE( out, "( ' * n = ', I0, ', m = ', I0, ', A_ne = ', I0,             &
     & ', H_ne = ', I0 )" ) n, m, A_ptr( n + 1 ) - 1, H_ptr( m + 1 ) - 1
      CALL WRITE_H_bycols( out, n, H_val, H_row, H_ptr )

      DEALLOCATE( A_ptr, A_row, H_ptr, H_row, X, X_l, X_u, Z, G, Y, C_l, C_u,  &
                  A_val, H_val, X_names, C_names, stat = status )

!  sparse co-ordinate version

      WRITE( out, "( ' CALL CUTEST_lqp_create (LP co-ordinate) ' )" )
      CALL CUTEST_lqp_create( status, input, buffer, out, n, m, f, G, X, X_l,  &
                              X_u, Z, Y, C_l, C_u, p_name, X_names, C_names,   &
                              A_ne = A_ne, A_row = A_row,                      &
                              A_col = A_col, A_val = A_val )
      IF ( status /= 0 ) GO TO 900
      WRITE( out, "( ' * n = ', I0, ', m = ', I0,                              &
     &  ', A_ne = ', I0 )" ) n, m, A_ne
      CALL WRITE_A_sparse( out, A_ne, A_ne, A_val, A_row, A_col )

      DEALLOCATE( A_row, A_col, X, X_l, X_u, Z, G, Y, C_l, C_u,                &
                  A_val, X_names, C_names, stat = status )

!  error test

      WRITE( out, "( ' CALL CUTEST_lqp_create (error test) ' )" )
      CALL CUTEST_lqp_create( status, input, buffer, out, n, m, f, G, X, X_l,  &
                              X_u, Z, Y, C_l, C_u, p_name, X_names, C_names,   &
                              A_ne = A_ne )
      IF ( status /= 0 ) GO TO 900

      CLOSE( input )
      STOP

!  error exits

 900  CONTINUE
      WRITE( out, "( ' error status = ', I0 )" ) status
      CLOSE( INPUT  )
      STOP

    CONTAINS

!  data printing subroutines

      SUBROUTINE WRITE_X( out, n, X, X_l, X_u, Z )
      INTEGER ( KIND = ip_ ) :: n, out
      REAL ( KIND = rp_ ), DIMENSION( n ) :: X, X_l, X_u, Z
      INTEGER :: i
      WRITE( out, "( ' *       i      X_l          X          X_u          Z')")
      DO i = 1, n
        WRITE( out, "( ' * ', I7, 4ES12.4 )" )                                 &
          i, X_l( i ), X( i ), X_u( i ), Z( i )
      END DO
      END SUBROUTINE WRITE_X

      SUBROUTINE WRITE_Y( out, m, Y, C_l, C_u )
      INTEGER ( KIND = ip_ ) :: m, out
      REAL ( KIND = rp_ ), DIMENSION( m ) :: Y, C_l, C_u
      INTEGER :: i
      WRITE( out, "( ' *       i      C_l         C_u          Y   ' )" )
      DO i = 1, m
        WRITE( out, "( ' * ', I7, 3ES12.4 )" ) i, C_l( i ), C_u( i ), Y( i )
      END DO
      END SUBROUTINE WRITE_Y

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

      SUBROUTINE WRITE_C_names( out, m, C_names )
      INTEGER ( KIND = ip_ ) :: m, out
      CHARACTER ( len = 10 ), DIMENSION( m ) :: C_names
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' *       i  C_name' )" )
      DO i = 1, m
        WRITE( out, "( ' * ', I7, 2X, A10 )" ) i, C_names( i )
      END DO
      END SUBROUTINE WRITE_C_names

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

      SUBROUTINE WRITE_A_dense( out, n, m, l_j2_1, l_j2_2, J2_val )
      INTEGER ( KIND = ip_ ) :: n, m, l_J2_1, l_j2_2, out
      REAL ( KIND = rp_ ), DIMENSION( l_j2_1, l_j2_2 ) :: J2_val
      INTEGER ( KIND = ip_ ) :: i, j
      WRITE( out, "( ' * A(dense)' )" )
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
        DO i = 1, m
          IF ( j + 3 <= n ) THEN
            WRITE( out, "( ' * ', I7,  4X, 4ES12.4 )" )                        &
              i, J2_val( i, j ), J2_val( i, j + 1 ),                           &
              J2_val( i, j + 2 ), J2_val( i, j + 3 )
          ELSE IF ( j + 2 <= n ) THEN
            WRITE( out, "( ' * ',  I7, 4X, 3ES12.4 )" )                        &
              i, J2_val( i, j ), J2_val( i, j + 1 ), J2_val( i, j + 2 )
          ELSE IF ( j + 1 <= n ) THEN
            WRITE( out, "( ' * ',  I7, 4X, 2ES12.4 )" )                        &
              i, J2_val( i, j ), J2_val( i, j + 1 )
          ELSE
            WRITE( out, "( ' * ',  I7, 4X, ES12.4 )" ) i, J2_val( i, j )
          END IF
        END DO
      END DO
      END SUBROUTINE WRITE_A_dense

      SUBROUTINE WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )
      INTEGER ( KIND = ip_ ) :: l_h, H_ne, out
      INTEGER ( KIND = ip_ ), DIMENSION( l_h ) :: H_row, H_col
      REAL ( KIND = rp_ ), DIMENSION( l_h ) :: H_val
      INTEGER ( KIND = ip_ ) :: i
      IF ( H_ne == 0 ) RETURN
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

      SUBROUTINE WRITE_A_sparse( out, A_ne, l_a, A_val, A_row, A_col )
      INTEGER ( KIND = ip_ ) :: l_a, A_ne, out
      INTEGER ( KIND = ip_ ), DIMENSION( l_a ) :: A_row, A_col
      REAL ( KIND = rp_ ), DIMENSION( l_a ) :: A_val
      INTEGER ( KIND = ip_ ) :: i
      IF ( A_ne == 0 ) RETURN
      WRITE( out, "( ' * A(sparse)' )" )
      WRITE( out, "( ' * ', 2( '    row    col     val    ' ) )" )
      DO i = 1, A_ne, 2
        IF ( i + 1 <= A_ne ) THEN
          WRITE( out, "( ' * ',  2( 2I7, ES12.4 ) )" )                         &
            A_row( i ), A_col( i ), A_val( i ),                                &
            A_row( i + 1 ), A_col( i + 1 ), A_val( i + 1 )
        ELSE
          WRITE( out, "( ' * ',  2( 2I7, ES12.4 ) )" )                         &
            A_row( i ), A_col( i ), A_val( i )
        END IF
      END DO
      END SUBROUTINE WRITE_A_sparse

      SUBROUTINE WRITE_H_byrows( out, n, H_val, H_col, H_ptr )
      INTEGER ( KIND = ip_ ) :: n, out
      INTEGER ( KIND = ip_ ), DIMENSION( n + 1 ) :: H_ptr
      INTEGER ( KIND = ip_ ), DIMENSION( H_ptr( n + 1 ) - 1 ) :: H_col
      REAL ( KIND = rp_ ), DIMENSION( H_ptr( n + 1 ) - 1 ) :: H_val
      INTEGER ( KIND = ip_ ) :: i, l, l_up, maxc
      WRITE( out, "( ' * H(by rows)' )" )
      maxc = MAXVAL( H_ptr( 2 : n + 1 ) - H_ptr( 1 : n ) )
      IF ( maxc >= 3 ) THEN
        WRITE( out, "( ' *     row ', 3( '   col     val     ' ) )" )
      ELSE IF ( maxc >= 2 ) THEN
        WRITE( out, "( ' *     row ', 2( '   col     val     ' ) )" )
      ELSE IF ( maxc >= 1 ) THEN
        WRITE( out, "( ' *     row ',  ( '   col     val     ' ) )" )
      ELSE
        WRITE( out, "( ' *     row ' )" )
      END IF
      DO i = 1, n
        l_up =  H_ptr( i + 1 ) - 1
        IF ( H_ptr( i ) > l_up ) THEN
          WRITE( out, "( ' * ',  I7, ' no entries ' )" ) i
        ELSE
          DO l = H_ptr( i ), l_up, 3
            IF ( l + 2 <= l_up ) THEN
              WRITE( out, "( ' * ',  I7, 3( I7, ES12.4 ) )" ) i,               &
                H_col( l ), H_val( l ), H_col( l + 1 ), H_val( l + 1 ),        &
                H_col( l + 2 ), H_val( l + 2 )
            ELSE IF ( l + 1 <= l_up ) THEN
              WRITE( out, "( ' * ',  I7, 2( I7, ES12.4 ) )" ) i,               &
                H_col( l ), H_val( l ), H_col( l + 1 ), H_val( l + 1 )
            ELSE
              WRITE( out, "( ' * ',  I7, ( I7, ES12.4 ) )" ) i,                &
                H_col( l ), H_val( l )
            END IF
          END DO
        END IF
      END DO
      END SUBROUTINE WRITE_H_byrows

      SUBROUTINE WRITE_A_byrows( out, m, A_val, A_col, A_ptr )
      INTEGER ( KIND = ip_ ) :: m, out
      INTEGER ( KIND = ip_ ), DIMENSION( m + 1 ) :: A_ptr
      INTEGER ( KIND = ip_ ), DIMENSION( A_ptr( m + 1 ) - 1 ) :: A_col
      REAL ( KIND = rp_ ), DIMENSION( A_ptr( m + 1 ) - 1 ) :: A_val
      INTEGER ( KIND = ip_ ) :: i, l, l_up, maxc
      WRITE( out, "( ' * A(by rows)' )" )
      maxc = MAXVAL( A_ptr( 2 : m + 1 ) - A_ptr( 1 : m ) )
      IF ( maxc >= 3 ) THEN
        WRITE( out, "( ' *     row ', 3( '   col     val     ' ) )" )
      ELSE IF ( maxc >= 2 ) THEN
        WRITE( out, "( ' *     row ', 2( '   col     val     ' ) )" )
      ELSE IF ( maxc >= 1 ) THEN
        WRITE( out, "( ' *     row ',  ( '   col     val     ' ) )" )
      ELSE
        WRITE( out, "( ' *     row ' )" )
      END IF
      DO i = 1, m
        l_up =  A_ptr( i + 1 ) - 1
        IF ( A_ptr( i ) > l_up ) THEN
          WRITE( out, "( ' * ',  I7, ' no entries ' )" ) i
        ELSE
          DO l = A_ptr( i ), l_up, 3
            IF ( l + 2 <= l_up ) THEN
              WRITE( out, "( ' * ',  I7, 3( I7, ES12.4 ) )" ) i,               &
                A_col( l ), A_val( l ), A_col( l + 1 ), A_val( l + 1 ),        &
                A_col( l + 2 ), A_val( l + 2 )
            ELSE IF ( l + 1 <= l_up ) THEN
              WRITE( out, "( ' * ',  I7, 2( I7, ES12.4 ) )" ) i,               &
                A_col( l ), A_val( l ), A_col( l + 1 ), A_val( l + 1 )
            ELSE
              WRITE( out, "( ' * ',  I7, ( I7, ES12.4 ) )" ) i,                &
                A_col( l ), A_val( l )
            END IF
          END DO
        END IF
      END DO
      END SUBROUTINE WRITE_A_byrows

      SUBROUTINE WRITE_H_bycols( out, n, H_val, H_row, H_ptr )
      INTEGER ( KIND = ip_ ) :: n, out
      INTEGER ( KIND = ip_ ), DIMENSION( n + 1 ) :: H_ptr
      INTEGER ( KIND = ip_ ), DIMENSION( H_ptr( n + 1 ) - 1 ) :: H_row
      REAL ( KIND = rp_ ), DIMENSION( H_ptr( n + 1 ) - 1 ) :: H_val
      INTEGER ( KIND = ip_ ) :: i, l, l_up, maxr
      WRITE( out, "( ' * H(by cols)' )" )
      maxr = MAXVAL( H_ptr( 2 : n + 1 ) - H_ptr( 1 : n ) )
      IF ( maxr >= 3 ) THEN
        WRITE( out, "( ' *     col ', 3( '   row     val     ' ) )" )
      ELSE IF ( maxr >= 2 ) THEN
        WRITE( out, "( ' *     col ', 2( '   row     val     ' ) )" )
      ELSE IF ( maxr >= 1 ) THEN
        WRITE( out, "( ' *     col ',  ( '   row     val     ' ) )" )
      ELSE
        WRITE( out, "( ' *     col ' )" )
      END IF
      DO i = 1, n
        l_up =  H_ptr( i + 1 ) - 1
        IF ( H_ptr( i ) > l_up ) THEN
          WRITE( out, "( ' * ',  I7, ' no entries ' )" ) i
        ELSE
          DO l = H_ptr( i ), l_up, 3
            IF ( l + 2 <= l_up ) THEN
              WRITE( out, "( ' * ',  I7, 3( I7, ES12.4 ) )" ) i,               &
                H_row( l ), H_val( l ), H_row( l + 1 ), H_val( l + 1 ),        &
                H_row( l + 2 ), H_val( l + 2 )
            ELSE IF ( l + 1 <= l_up ) THEN
              WRITE( out, "( ' * ',  I7, 2( I7, ES12.4 ) )" ) i,               &
                H_row( l ), H_val( l ), H_row( l + 1 ), H_val( l + 1 )
            ELSE
              WRITE( out, "( ' * ',  I7, ( I7, ES12.4 ) )" ) i,                &
                H_row( l ), H_val( l )
            END IF
          END DO
        END IF
      END DO
      END SUBROUTINE WRITE_H_bycols

      SUBROUTINE WRITE_A_bycols( out, n, A_val, A_row, A_ptr )
      INTEGER ( KIND = ip_ ) :: n, out
      INTEGER ( KIND = ip_ ), DIMENSION( n + 1 ) :: A_ptr
      INTEGER ( KIND = ip_ ), DIMENSION( A_ptr( n + 1 ) - 1 ) :: A_row
      REAL ( KIND = rp_ ), DIMENSION( A_ptr( n + 1 ) - 1 ) :: A_val
      INTEGER ( KIND = ip_ ) :: i, l, l_up, maxr
      WRITE( out, "( ' * A(by cols)' )" )
      maxr = MAXVAL( A_ptr( 2 : n + 1 ) - A_ptr( 1 : n ) )
      IF ( maxr >= 3 ) THEN
        WRITE( out, "( ' *     col ', 3( '   row     val     ' ) )" )
      ELSE IF ( maxr >= 2 ) THEN
        WRITE( out, "( ' *     col ', 2( '   row     val     ' ) )" )
      ELSE IF ( maxr >= 1 ) THEN
        WRITE( out, "( ' *     col ',  ( '   row     val     ' ) )" )
      ELSE
        WRITE( out, "( ' *     col ' )" )
      END IF
      DO i = 1, n
        l_up =  A_ptr( i + 1 ) - 1
        IF ( A_ptr( i ) > l_up ) THEN
          WRITE( out, "( ' * ',  I7, ' no entries ' )" ) i
        ELSE
          DO l = A_ptr( i ), l_up, 3
            IF ( l + 2 <= l_up ) THEN
              WRITE( out, "( ' * ',  I7, 3( I7, ES12.4 ) )" ) i,               &
                A_row( l ), A_val( l ), A_row( l + 1 ), A_val( l + 1 ),        &
                A_row( l + 2 ), A_val( l + 2 )
            ELSE IF ( l + 1 <= l_up ) THEN
              WRITE( out, "( ' * ',  I7, 2( I7, ES12.4 ) )" ) i,               &
                A_row( l ), A_val( l ), A_row( l + 1 ), A_val( l + 1 )
            ELSE
              WRITE( out, "( ' * ',  I7, ( I7, ES12.4 ) )" ) i,                &
                A_row( l ), A_val( l )
            END IF
          END DO
        END IF
      END DO
      END SUBROUTINE WRITE_A_bycols

    END PROGRAM CUTEST_lqp_test

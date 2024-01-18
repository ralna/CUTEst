! THIS VERSION: CUTEST 2.2 - 2023-11-12 AT 14:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-*-*-*-*-*- C U T E S T  L Q P  M O D U l E -*-*-*-*-*-*-*-*-*-*-

!  Copyright reserved, Bongartz/Conn/Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   Essence contained in GALAHAD-CUTEr QP interfaces, April 2004
!   Stand-alone CUTEst module, March 2013

!  For full documentation, see
!   http://galahad.rl.ac.uk/galahad-www/specs.html

    MODULE CUTEST_LQP_precision

      USE CUTEST_KINDS_precision
      USE CUTEST_INTERFACE_precision

      IMPLICIT NONE

      PRIVATE
      PUBLIC :: CUTEST_lqp_create

!----------------------
!   P a r a m e t e r s
!----------------------

      REAL ( KIND = rp_ ), PARAMETER :: zero = 0.0_rp_

!  module procedures

    CONTAINS

!-*-*-*-*-*-  C U T E S T _ l q p _ c r e a t e  S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE CUTEST_LQP_create( status, input, io_buffer, out, n, m, f,    &
                                    G, X, X_l, X_u, Z, Y, C_l, C_u,            &
                                    p_name, X_names, C_names, X_type,          &
                                    A_ne, A_row, A_col, A_ptr, A_val, A_dense, &
                                    H_ne, H_row, H_col, H_ptr, H_val, H_dense, &
                                    H_pert, dont_terminate_cutest )

!  ------------------------------------------------
!  build the data for the linear program

!    minimize    f + g^T x
!    subject to  c_l <= A x <= c_l
!    and         x_l <=  x  <= x_l

!  or the quadratic program

!    minimize    f + g^T x + 1/2 x^T H x
!    subject to  c_l <= A x <= c_l
!    and         x_l <=  x  <= x_l

!  from a problem written in MPS, QPS or SIF format
!  ------------------------------------------------

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: input, io_buffer, out
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, n, m
      REAL ( KIND = rp_ ), INTENT( OUT ) :: f
      CHARACTER ( LEN = 10 ), INTENT( OUT ) :: p_name
      REAL ( KIND = rp_ ), ALLOCATABLE, INTENT( OUT ),                         &
        DIMENSION( : ) :: G, X, X_l, X_u, Z, Y, C_l, C_u
      CHARACTER ( LEN = 10 ), ALLOCATABLE, INTENT( OUT ),                      &
        DIMENSION( : )  :: X_names, C_names

!  optional dummy arguments: for a_ and h_, in order

!  if dense is present, dense storage is required
!  if ne, row, col and val are present, sparse co-ordinate storage is required
!  if row, ptr and val are present, column-wise storage is required
!  if col, ptr and val are present, row-wise storage is required
!  otherwise an error will be flagged with status = -1 (A_) or
!  H will be ignored, i.e., and LP will be formed (H_)

!  if h_pert is present, this value will be added to the diagonal of the Hessian
!  if x_type is present determine the type of x (0=continuous,>0=integer)

      INTEGER ( KIND = ip_ ), OPTIONAL :: A_ne, H_ne
      LOGICAL, OPTIONAL :: dont_terminate_cutest
      REAL ( KIND = rp_ ), OPTIONAL :: H_pert
      INTEGER ( KIND = ip_ ), ALLOCATABLE, OPTIONAL,                           &
        DIMENSION( : ) :: X_type, A_row, A_col, A_ptr, H_row, H_col, H_ptr
      REAL ( KIND = rp_ ), ALLOCATABLE, OPTIONAL,                              &
        DIMENSION( : ) :: A_val, H_val
      REAL ( KIND = rp_ ), ALLOCATABLE, OPTIONAL,                              &
        DIMENSION( : , : ) :: A_dense, H_dense

!  local variables

      INTEGER ( KIND = ip_ ) :: alloc_status, i, j, l, liw, la, lh, nea, neh
      CHARACTER( LEN = 1 ) :: a_type, h_type
      LOGICAL :: pert_hess
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: A_tmp, H_tmp, IW
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X0, C
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATN, LINEAR

!  check that suitable arguments are present for A

      IF ( PRESENT( A_dense ) ) THEN
        a_type = 'D'
      ELSE IF ( PRESENT( A_ne ) .AND. PRESENT( A_row ) .AND.                   &
                PRESENT( A_col ) .AND. PRESENT( A_val ) ) THEN
        a_type = 'S'
      ELSE IF ( PRESENT( A_row ) .AND. PRESENT( A_ptr ) .AND.                  &
                PRESENT( A_val ) ) THEN
        a_type = 'C'
      ELSE IF ( PRESENT( A_col ) .AND. PRESENT( A_ptr ) .AND.                  &
                PRESENT( A_val ) ) THEN
        a_type = 'R'
      ELSE
        status = - 1
        RETURN
      END IF

!  check that suitable arguments are present for H

      IF ( PRESENT( H_dense ) ) THEN
        h_type = 'D'
      ELSE IF ( PRESENT( H_ne ) .AND. PRESENT( H_row ) .AND.                   &
                PRESENT( H_col ) .AND. PRESENT( H_val ) ) THEN
        h_type = 'S'
      ELSE IF ( PRESENT( H_row ) .AND. PRESENT( H_ptr ) .AND.                  &
                PRESENT( H_val ) ) THEN
        h_type = 'C'
      ELSE IF ( PRESENT( H_col ) .AND. PRESENT( H_ptr ) .AND.                  &
                PRESENT( H_val ) ) THEN
        h_type = 'R'
      ELSE
        h_type = 'N'
      END IF

!  check to see if there is any diagonal Hessian perturbation

      IF ( PRESENT( H_pert ) ) THEN
        pert_hess = H_pert /= zero
      ELSE
        pert_hess = .FALSE.
      END IF

!  determine the number of variables and constraints

      CALL CUTEST_cdimen_r( status, input, n, m )
      IF ( status /= 0 ) RETURN

!  allocate suitable arrays

      ALLOCATE( X( n ), X_l( n ), X_u( n ), G( n ), Z( n ),                    &
                C_l( m ), C_u( m ), Y( m ), X_names( n ), C_names( m ),        &
                EQUATN( m ), LINEAR( m ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) GO TO 990

!  set up the data structures necessary to hold the problem

      CALL CUTEST_csetup_r( status, input, out, io_buffer, n, m, X, X_l, X_u,  &
                            Y, C_l, C_u, EQUATN, LINEAR, 0_ip_, 0_ip_, 0_ip_ )
      IF ( status /= 0 ) RETURN
      DEALLOCATE( LINEAR, STAT = alloc_status )
      IF ( alloc_status /= 0 ) GO TO 990

!  determine the names of the problem, variables and constraints.

      CALL CUTEST_cnames_r( status, n, m, p_name, X_names, C_names )
      IF ( status /= 0 ) RETURN

!  if required, determine the variable types

      IF ( PRESENT( X_type ) ) THEN
        ALLOCATE( X_type( n ), STAT = alloc_status )
        IF ( alloc_status /= 0 ) GO TO 990
        CALL CUTEST_cvartype_r( status, n, X_type )
        IF ( status /= 0 ) RETURN
      END IF

!  ensure that the initial point satisfies its bounds

      X( : n ) = MIN( X_u( : n ), MAX( X_l( : n ), X( : n ) ) )
      Z( : n ) = zero

!  allocate temporary storage

      ALLOCATE( X0( n ), C( m ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) GO TO 990

!  set X0 to zero to determine the constant terms for the problem functions

      X0 = zero

!  evaluate the constant terms of the objective (f) and constraint functions (C)

      CALL CUTEST_cfn_r( status, n, m, X0, f, C( : m ) )
      IF ( status /= 0 ) RETURN
      DO i = 1, m
        IF ( EQUATN( i ) ) THEN
          C_l( i ) = C_l( i ) - C( i )
          C_u( i ) = C_l( i )
        ELSE
          C_l( i ) = C_l( i ) - C( i )
          C_u( i ) = C_u( i ) - C( i )
        END IF
      END DO

      DEALLOCATE( EQUATN, C, STAT = alloc_status )
      IF ( alloc_status /= 0 ) GO TO 990

!  form A. i. dense storage is required

      IF ( a_type == 'D' ) THEN
        ALLOCATE( A_dense( m, n ), STAT = alloc_status )
        IF ( alloc_status /= 0 ) GO TO 990
        CALL CUTEST_cgr_r( status, n, m, X0, Y, .FALSE., G, .FALSE.,           &
                           m, n, A_dense )
        IF ( status /= 0 ) RETURN

!  determine the number of nonzeros in the Jacobian

      ELSE
        CALL CUTEST_cdimsj_r( status, la )
        IF ( status /= 0 ) RETURN
        la = MAX( la, 1 )
        G( : n ) = zero

!  ii. sparse co-ordinate storage is required

        IF ( a_type == 'S' ) THEN

!  allocate arrays to hold the Jacobian

          ALLOCATE( A_row( la ), A_col( la ), A_val( la ), STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 990

!  evaluate the linear terms of the constraint functions

          CALL CUTEST_csgr_r( status, n, m, X0, Y, .FALSE., nea, la,           &
                              A_val, A_col, A_row )
          IF ( status /= 0 ) RETURN

!  exclude zeros; set the linear term for the objective function

          A_ne = 0
          DO i = 1, nea
            IF ( A_val( i ) /= zero ) THEN
              IF ( A_row( i ) > 0 ) THEN
                A_ne = A_ne + 1
                A_row( A_ne ) = A_row( i )
                A_col( A_ne ) = A_col( i )
                A_val( A_ne ) = A_val( i )
              ELSE
                G( A_col( i ) ) = A_val( i )
              END IF
            END IF
          END DO

!  iii. sparse column-wise storage is required

        ELSE IF ( a_type == 'C' ) THEN

!  allocate arrays to hold the Jacobian

          ALLOCATE( A_row( la ), A_tmp( la ), A_val( la ), STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 990

!  evaluate the linear terms of the constraint functions

          CALL CUTEST_csgr_r( status, n, m, X0, Y, .FALSE., nea, la,           &
                              A_val, A_tmp, A_row )
          IF ( status /= 0 ) RETURN

!  exclude zeros; set the linear term for the objective function

          la = 0
          DO i = 1, nea
            IF ( A_val( i ) /= zero ) THEN
              IF ( A_row( i ) > 0 ) THEN
                la = la + 1
                A_row( la ) = A_row( i )
                A_tmp( la ) = A_tmp( i )
                A_val( la ) = A_val( i )
              ELSE
                G( A_tmp( i ) ) = A_val( i )
              END IF
            END IF
          END DO

!  transform A to column storage format

          liw = MAX( m, n ) + 1
          ALLOCATE( A_ptr( n + 1 ), IW( liw ), STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 990
          IF ( la /= 0 ) THEN
            CALL CUTEST_reorder_by_rows( n, m, la, A_tmp, A_row, la,           &
                                         A_val, A_ptr, n + 1, IW, liw,         &
                                         out, out, i )
          ELSE
            A_ptr = 0
          END IF
          DEALLOCATE( A_tmp, IW, STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 990

!  iv. sparse row-wise storage is required

        ELSE IF ( a_type == 'R' ) THEN

!  allocate arrays to hold the Jacobian

          ALLOCATE( A_tmp( la ), A_col( la ), A_val( la ), STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 990

!  evaluate the linear terms of the constraint functions

          CALL CUTEST_csgr_r( status, n, m, X0, Y, .FALSE., nea, la,           &
                              A_val, A_col, A_tmp )
          IF ( status /= 0 ) RETURN

!  exclude zeros; set the linear term for the objective function

          la = 0
          DO i = 1, nea
            IF ( A_val( i ) /= zero ) THEN
              IF ( A_tmp( i ) > 0 ) THEN
                la = la + 1
                A_tmp( la ) = A_tmp( i )
                A_col( la ) = A_col( i )
                A_val( la ) = A_val( i )
              ELSE
                G( A_col( i ) ) = A_val( i )
              END IF
            END IF
          END DO

!  transform A to row storage format

          liw = MAX( m, n ) + 1
          ALLOCATE( A_ptr( m + 1 ), IW( liw ), STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 990
          IF ( la /= 0 ) THEN
            CALL CUTEST_reorder_by_rows( m, n, la, A_tmp, A_col, la,           &
                                         A_val, A_ptr, m + 1, IW, liw,         &
                                         out, out, i )
          ELSE
            A_ptr = 0
          END IF
          DEALLOCATE( A_tmp, IW, STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 990
        END IF
      END IF

!  now form H. i. dense storage is required

      IF ( h_type == 'D' ) THEN
        ALLOCATE( H_dense( n, n ), STAT = alloc_status )
        IF ( alloc_status /= 0 ) GO TO 990
        CALL CUTEST_cdh_r( status, n, m, X0, Y, n, H_dense )
        IF ( status /= 0 ) RETURN
        IF ( pert_hess ) THEN
          DO i = 1, n
            H_dense( i, i ) = H_dense( i, i ) + H_pert
          END DO
        END IF

!  determine the number of nonzeros in the Hessian

      ELSE IF ( h_type /= 'N' ) THEN
        CALL CUTEST_cdimsh_r( status, lh )
        IF ( status /= 0 ) RETURN
        IF ( pert_hess ) lh = lh + n
        lh = MAX( lh, 1 )
        liw = n + 1
        ALLOCATE( IW( liw ), STAT = alloc_status )
        IW( : n ) = 0

!  ii. sparse co-ordinate storage is required

        IF ( h_type == 'S' ) THEN

!  allocate arrays to hold the Hessian

          ALLOCATE( H_row( lh ), H_col( lh ), H_val( lh ), STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 990

!  evaluate the Hessian of the Lagrangian function at the origin

          CALL CUTEST_csh_r( status, n, m, X0, Y, neh, lh, H_val, H_row, H_col )
          IF ( status /= 0 ) RETURN

!  remove out of range entries and only store the upper triangle

          H_ne = 0
          DO l = 1, neh
            i = H_row( l ) ; j = H_col( l )
            IF ( i < 1 .OR. i > n .OR. j < 1 .OR. j > n ) CYCLE
            IF ( H_val( l ) /= zero ) THEN
              H_ne = H_ne + 1 ; H_val( H_ne ) = H_val( l )
              IF ( i >= j ) THEN
                H_row( H_ne ) = j ; H_col( H_ne ) = i
              ELSE
                H_row( H_ne ) = i ; H_col( H_ne ) = j
              END IF
            END IF

!  add digonal perturbations if any

            IF ( i == j ) THEN
              IF ( pert_hess .AND. IW( i ) == 0 ) THEN
                IW( i ) = 1
                IF ( H_val( l ) /= zero ) THEN
                  H_val( H_ne ) = H_val( H_ne ) + H_pert
                ELSE
                  H_ne = H_ne + 1 ; H_val( H_ne ) = H_pert
                END IF
              END IF
            END IF
          END DO

          IF ( pert_hess ) THEN
            DO i = 1, n
              IF ( IW( i ) == 0 ) THEN
                H_ne = H_ne + 1 ; H_val( H_ne ) = H_pert
                H_row( H_ne ) = i ; H_col( H_ne ) = i
              END IF
            END DO
          END IF
          DEALLOCATE( IW, STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 990

!  iii. sparse column-wise storage is required

        ELSE IF ( h_type == 'C' ) THEN

!  allocate arrays to hold the Hessian

          ALLOCATE( H_row( lh ), H_tmp( lh ), H_val( lh ), STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 990

!  evaluate the Hessian of the Lagrangian function at the origin

          CALL CUTEST_csh_r( status, n, m, X0, Y, neh, lh, H_val, H_row, H_tmp )
          IF ( status /= 0 ) RETURN

!  remove out of range entries and only store the upper triangle

          lh = 0
          DO l = 1, neh
            i = H_row( l ) ; j = H_tmp( l )
            IF ( i < 1 .OR. i > n .OR. j < 1 .OR. j > n ) CYCLE
            IF ( H_val( l ) /= zero ) THEN
              lh = lh + 1 ; H_val( lh ) = H_val( l )
              IF ( i >= j ) THEN
                H_row( lh ) = j ; H_tmp( lh ) = i
              ELSE
                H_row( lh ) = i ; H_tmp( lh ) = j
              END IF
            END IF

!  add digonal perturbations if any

            IF ( i == j ) THEN
              IF ( pert_hess .AND. IW( i ) == 0 ) THEN
                IW( i ) = 1
                IF ( H_val( l ) /= zero ) THEN
                  H_val( lh ) = H_val( lh ) + H_pert
                ELSE
                  lh = lh + 1 ; H_val( lh ) = H_pert
                END IF
              END IF
            END IF
          END DO

          IF ( pert_hess ) THEN
            DO i = 1, n
              IF ( IW( i ) == 0 ) THEN
                lh = lh + 1 ; H_val( lh ) = H_pert
                H_row( lh ) = i ; H_tmp( lh ) = i
              END IF
            END DO
          END IF

!  transform H to column storage format

          ALLOCATE( H_ptr( n + 1 ), STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 990
          IF ( lh /= 0 ) THEN
            CALL CUTEST_reorder_by_rows( n, n, lh, H_tmp, H_row, lh,           &
                                         H_val, H_ptr, n + 1, IW, liw,         &
                                         out, out, i )
          ELSE
            H_ptr = 0
          END IF
          DEALLOCATE( H_tmp, IW, STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 990

!  iv. sparse row-wise storage is required

        ELSE IF ( h_type == 'R' ) THEN

!  allocate arrays to hold the Hessian

          ALLOCATE( H_tmp( lh ), H_col( lh ), H_val( lh ), STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 990

!  evaluate the Hessian of the Lagrangian function at the origin

          CALL CUTEST_csh_r( status, n, m, X0, Y, neh, lh, H_val, H_tmp, H_col )
          IF ( status /= 0 ) RETURN

!  remove out of range entries and only store the upper triangle

          lh = 0
          DO l = 1, neh
            i = H_tmp( l ) ; j = H_col( l )
            IF ( i < 1 .OR. i > n .OR. j < 1 .OR. j > n ) CYCLE
            IF ( H_val( l ) /= zero ) THEN
              lh = lh + 1 ; H_val( lh ) = H_val( l )
              IF ( i >= j ) THEN
                H_tmp( lh ) = j ; H_col( lh ) = i
              ELSE
                H_tmp( lh ) = i ; H_col( lh ) = j
              END IF
            END IF

!  add digonal perturbations if any

            IF ( i == j ) THEN
              IF ( pert_hess .AND. IW( i ) == 0 ) THEN
                IW( i ) = 1
                IF ( H_val( l ) /= zero ) THEN
                  H_val( lh ) = H_val( lh ) + H_pert
                ELSE
                  lh = lh + 1 ; H_val( lh ) = H_pert
                END IF
              END IF
            END IF
          END DO

          IF ( pert_hess ) THEN
            DO i = 1, n
              IF ( IW( i ) == 0 ) THEN
                lh = lh + 1 ; H_val( lh ) = H_pert
                H_tmp( lh ) = i ; H_col( lh ) = i
              END IF
            END DO
          END IF

!  Transform H to row storage format

          ALLOCATE( H_ptr( n + 1 ), STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 990
          IF ( lh /= 0 ) THEN
            CALL CUTEST_reorder_by_rows( n, n, lh, H_tmp, H_col, lh,           &
                                         H_val, H_ptr, n + 1, IW, liw,         &
                                         out, out, i )
          ELSE
            H_ptr = 0
          END IF
          DEALLOCATE( H_tmp, IW, STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 990
        END IF
      END IF

      DEALLOCATE( X0, STAT = alloc_status )
      IF ( alloc_status /= 0 ) GO TO 990
      IF ( PRESENT( dont_terminate_cutest ) ) THEN
        IF ( .NOT. dont_terminate_cutest ) CALL CUTEST_cterminate_r( status )
      ELSE
        CALL CUTEST_cterminate_r( status )
      END IF
      RETURN

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) alloc_status
      status = 1
      RETURN

!  end of subroutine CUTEST_lqp_create

     END SUBROUTINE CUTEST_lqp_create

!-*-*-  C U T E S T _ r e o r d e r _ b y _ r o w s   S U B R O U T I N E -*-*-

     SUBROUTINE CUTEST_reorder_by_rows( nr, nc, nnz, A_row, A_col, la, A_val,  &
                                        A_ptr, lptr, IW, liw, error, warning,  &
                                        inform )

!  Reorder a sparse matrix A from arbitary coordinate order to row order.
!  Extracted from GALAHAD SORT module
!  Nick Gould, 8th August 2002.

!  ------------------------- dummy arguments --------------------------
!
!  nr     integer, which gives the number of rows in A.
!         nr must be non-negative.
!
!  nc     integer, which gives the number of columns in A.
!         nc must be non-negative.
!
!  nnz    integer, which gives the number of nonzeros in A.
!         nnz must be non-negative.
!
!  A_row  integer array of length la. On entry, A_row(k), k = 1, ..., nnz give
!         the row indices of A. On exit, A_row will have been reordered, but
!         A_row(k) will still be the row index corresponding to the
!         entry with column index A_col(k).
!
!  A_col  integer array of length la. On entry, A_col(k), k = 1, ..., nnz give
!         the column indices of A. On exit, A_col will have been reordered so
!         that entries in row i appear directly before those in row i+1 for
!         i = 1, ..., nr-1.
!
!  la    integer, which gives the actual dimension of A_val.
!        la must be at least nnz.
!
!  A_val  real array of length la. On entry, A_val(k), k = 1, ..., nnz give
!         the values of A. On exit, A_val will have been reordered so that
!         entries in row i appear directly before those in row i+1 for
!         i = 1, ..., nr-1 and correspond to those in A_row and A_col.
!
!  A_ptr  integer array of length lptr. On exit, A_ptr(i), i = 1, ..., nr give
!         the starting addresses for the entries in A_row/A_col/A_val in
!         row i, while A_ptr(nr+1) gives the index of the first non-occupied
!         component of A.
!
!  lptr  integer, which gives the actual dimension of A_ptr.
!        lptr must be at least nr + 1.
!
!  IW     workspace integer array of length liw.
!
!  liw   integer, which gives the actual dimension of IW
!        liw  must be at least MAX( nr, nc ) + 1.
!
!  error integer, which gives the output unit number for error messages.
!        Error messages only occur if error > 0.
!
!  warning integer, which gives the output unit number for warning messages.
!        Warning messages only occur if warning > 0.
!
!  inform integer, which gives the exit status of SORT_reorder_by_rows.
!         Possible values are:
!
!          0   A has been successfully re-orderered.
!         -1   A has been successfully re-orderered, but there were duplicate
!              entries which have been summed.
!         -2   A has been successfully re-orderered, but there were row entries
!              out of range which were ignored.
!         -3   A has been successfully re-orderered, but there were column
!              entries out of range which were ignored.
!         -4   A has been successfully re-orderered, but there were no rows
!         -5   A has been successfully re-orderered, but there were no entries
!          1   nr, nc or nnz is too small. The reordering was unsuccessful.
!          2   la < nnz. The reordering was unsuccessful.
!          3   liw < MAX( nr, nc ) + 1. The reordering was unsuccessful.
!          4   lptr < nr + 1. The reordering was unsuccessful.
!          5   All entries were out of order. The reordering was unsuccessful.
!
!  ------------------ end of dummy arguments --------------------------
!
!  Dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: nr, nc, nnz, la, lptr, liw
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: error, warning
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: inform
      REAL ( KIND = rp_ ), INTENT( INOUT ), DIMENSION( la ) :: A_val
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( la ) :: A_row, A_col
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lptr ) :: A_ptr
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( liw ) :: IW

!  Local variables

      INTEGER ( KIND = ip_ ) :: i, j, k, k1, k2, l, nzi, ie, iep, je, jep
      INTEGER ( KIND = ip_ ) :: loc, idup, iout, jout, nzout
      REAL ( KIND = rp_ ) :: ae, aep

! Initialize data

      inform = 0
      nzout = 0 ; iout = 0 ; jout = 0 ; idup = 0

!  Check for faulty input data

      IF ( nr < 0 .OR. nc < 0 .OR. nnz < 0 ) THEN
        inform = 1
        IF ( error > 0 ) THEN
          WRITE( error, 2000 ) inform
          WRITE( error, "( 1X, ' nr, nc, or nnz is out of range', /,           &
         &           ' nr = ',I0,' nc = ',I0,' nnz = ', I0 )" ) nr, nc, nnz
        END IF
        RETURN
      END IF

      IF ( la < nnz ) THEN
        inform = 2
        IF ( error > 0 ) THEN
          WRITE( error, 2000 ) inform
          WRITE( error, "( 1X, ' increase la from', I0, ' to at least ', I0 )")&
            la, nnz
        END IF
        RETURN
      END IF

      IF ( liw < MAX( nr, nc ) + 1 ) THEN
        inform = 3
        IF ( error > 0 ) THEN
          WRITE( error, 2000 ) inform
          WRITE( error, 2020 ) 'liw ', liw, MAX( nr, nc ) + 1
        END IF
        RETURN
      END IF

      IF ( lptr < nr + 1 ) THEN
        inform = 4
        IF ( error > 0 ) THEN
          WRITE( error, 2000 ) inform
          WRITE( error, 2020 ) 'lptr', lptr, nr + 1
        END IF
        RETURN
      END IF

!  If the matrix has no rows or no entries, exit accordingly

      IF ( nr == 0 ) THEN
        inform = - 4
        A_ptr( 1 ) = 0
        IF ( warning > 0 ) THEN
          WRITE( warning, 2010 ) inform
          WRITE( warning, "( '   the matrix has no rows' )" )
        END IF
        RETURN
      END IF

      IF ( nnz == 0 ) THEN
        inform = - 5
        A_ptr( 1 : nr + 1 ) = 1
        IF ( warning > 0 ) THEN
          WRITE( warning, 2010 ) inform
          WRITE( warning, "( '   the matrix has no entries' )" )
        END IF
        RETURN
      END IF

!  Record the number of column and row indices out of order in iout and jout
!  and then remove them from consideration

      DO k = 1,nnz
        i = A_row( k ); j = A_col( k )
        IF ( i > nr .OR. i < 1 ) THEN
          iout = iout + 1
        ELSE IF ( j > nc .OR. j < 1 ) THEN
          jout = jout + 1
        ELSE
          nzout = nzout + 1
          A_row( nzout ) = i ; A_col( nzout ) = j
          A_val( nzout ) = A_val( k )
        END IF
      END DO

!  Inform the user if there has been faulty data

      IF ( iout > 0 ) THEN
        inform = - 2
        IF ( warning > 0 ) THEN
          WRITE( warning, 2010 ) inform
          WRITE( warning, "( 1X, I0,' entries input in A_row were out of',     &
         &              ' range and have been ignored by the routine')" ) iout
        END IF
      END IF

      IF ( jout > 0 ) THEN
        inform = - 3
        IF ( warning > 0 ) THEN
          WRITE( warning, 2010 ) inform
          WRITE( warning, "( 1X, I0,' entries input in A_col were out of',     &
         &              ' range and have been ignored by the routine')" ) jout
        END IF
      END IF

!  If all the data is faulty, exit

      IF ( iout + jout == nnz ) THEN
        inform = 5
        IF ( error > 0 ) THEN
          WRITE( error, 2000 ) inform
          WRITE( error, "( 4X, ' All entries input in A were out of range ' )" )
        END IF
        nzout = 0
        RETURN
      END IF

!  nzout gives the number of nonzero entries following removals. Now sort the
!  pattern of a sparse matrix from arbitary order to row order. The
!  order within each row is unimportant

!  Record the number of elements in each row in IW

      IW( : nr + 1 ) = 0
      DO k = 1, nzout
        i = A_row( k )
        IW( i ) = IW( i ) + 1
      END DO

!  Record the positions where each row would begin, in a compressed format
!  with the rows in natural order, in A_ptr and IW

      A_ptr( 1 ) = 1
      DO i = 2, nr + 1
        A_ptr( i ) = IW( i - 1 ) + A_ptr( i - 1 )
        IW( i - 1 ) = A_ptr( i - 1 )
      END DO

!  Reorder the elements into row order. Fill in each row from the front,
!  and increase the pointer IW( k ) by 1 as a new entry is placed in row k

      DO l = 1, nr
        DO k = IW( l ), A_ptr( l + 1 ) - 1
          ie = A_row( k ) ; je = A_col( k ) ; ae = A_val( k )
          DO j = 1, nzout
            IF ( ie == l ) EXIT
            loc = IW( ie )
            iep = A_row( loc ) ; jep = A_col( loc ) ; aep = A_val( loc )
            IW( ie ) = loc + 1
            A_row( loc ) = ie ; A_col( loc ) = je ; A_val( loc ) = ae
            ie = iep ; je = jep ; ae = aep
          END DO
          A_row( k ) = ie ; A_col( k ) = je ; A_val( k ) = ae
        END DO
      END DO

!  Check for duplicates

      nzout = 0
      k1 = 1
      nzi = 0
      IW( : nc ) = 0
      DO i = 1, nr
        k2 = A_ptr( i + 1 ) - 1
        A_ptr( i + 1 ) = A_ptr( i )
        DO k = k1, k2
          j = A_col( k )
          IF ( IW( j ) <= nzi ) THEN
            nzout = nzout + 1
            A_row( nzout ) = A_row( k )
            A_col( nzout ) = j ; A_val( nzout ) = A_val( k )
            A_ptr( i + 1 ) = A_ptr( i + 1 ) + 1
            IW( j ) = nzout

!  There is a duplicate in row i; sum the values

          ELSE
            idup = idup + 1
            A_val( IW( j ) ) = A_val( IW( j ) ) + A_val( k )
          END IF
        END DO
        k1 = k2 + 1
        nzi = nzout
      END DO
      IF ( idup > 0 ) THEN
        inform = - 1
        IF ( warning > 0 ) THEN
          WRITE( warning, 2010 ) inform
          WRITE( warning, "( 3X, I0,' duplicate input entries summed' )" ) idup
        END IF
      END IF

!  Non-executable statements

 2000 FORMAT( /,' - Error return from SORT_reorder_by_rows - inform = ', I0 )
 2010 FORMAT( /,' - Warning message from SORT_reorder_by_rows - inform = ', I0 )
 2020 FORMAT( 1X, ' increase ', A4, ' from ', I0,' to at least ', I0 )

!  End of subroutine CUTEST_reorder_by_rows

        END SUBROUTINE CUTEST_reorder_by_rows

!  end of module CUTEST_LQP_double

   END MODULE CUTEST_LQP_precision

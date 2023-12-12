! THIS VERSION: CUTEST 2.2 - 2023-11-16 AT 14:20 GMT.

#include "cutest_modules.h"

!-*-*-*-*-*-*- C U T E S T   q p l i b  _ m a i n   P R O G R A M -*-*-*-*-*-

    PROGRAM CUTEST_qplib_main

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released January 2014

     USE CUTEST_LQP_precision
     USE CUTEST_KINDS_precision

     IMPLICIT NONE

      REAL( KIND = sp_ ), PARAMETER :: teneps_s = 10.0_sp_ * EPSILON( 1.0_sp_ )
      REAL( KIND = dp_ ), PARAMETER :: teneps_d = 10.0_dp_ * EPSILON( 1.0_dp_ )

!----------------------
!   P a r a m e t e r s
!----------------------

      INTEGER ( KIND = ip_ ), PARAMETER :: input = 55
      INTEGER ( KIND = ip_ ), PARAMETER :: input_spec = 46
      INTEGER ( KIND = ip_ ), PARAMETER :: standard_out = 6
      INTEGER ( KIND = ip_ ), PARAMETER :: qplib_out = 61
      INTEGER ( KIND = ip_ ), PARAMETER :: qplib_out_dummy = 62
      INTEGER ( KIND = ip_ ), PARAMETER :: buffer = 77
      REAL ( KIND = rp_ ), PARAMETER :: zero = 0.0_rp_
      REAL ( KIND = rp_ ), PARAMETER :: one = 1.0_rp_
      REAL ( KIND = rp_ ), PARAMETER :: infinity = ( 10.0_rp_ ) ** 19
      REAL ( KIND = rp_ ), PARAMETER :: infinity_used = ( 10.0_rp_ ) ** 20

      INTEGER ( KIND = ip_ ), PARAMETER :: qp = 1
      INTEGER ( KIND = ip_ ), PARAMETER :: qcqp = 2
      INTEGER ( KIND = ip_ ), PARAMETER :: bqp = 3
      INTEGER ( KIND = ip_ ), PARAMETER :: lp = 4
      INTEGER ( KIND = ip_ ), PARAMETER :: qcp = 5

!     CHARACTER ( len = 16 ) :: char_int_default = REPEAT( ' ', 16 )
!     CHARACTER ( len = 24 ) :: char_val_default = REPEAT( ' ', 24 )

!--------------------------------
!   L o c a l   V a r i a b l e s
!--------------------------------

      INTEGER :: n, m, H_ne, A_ne, status, out
      REAL ( KIND = rp_ ) :: f, h_pert
      CHARACTER ( len = 10 ) ::  p_name
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: X_type
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: A_row, A_col
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: H_row, H_col
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, X_l, X_u, Z, G, X0
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: Y, C_l, C_u
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: A_val, H_val
      CHARACTER ( len = 10 ), ALLOCATABLE, DIMENSION( : ) :: X_names, C_names

      INTEGER ( KIND = ip_ ) :: i, int_var, bin_var, l, lh, nehi, nnzh_i
      INTEGER ( KIND = ip_ ) :: problem_type = 1
      LOGICAL :: filexx
      LOGICAL :: append_dim = .FALSE.
      LOGICAL :: qplib_wrfile = .FALSE.
      CHARACTER ( len = 16 ) :: char_i, char_j, char_l
      CHARACTER ( len = 24 ) :: char_val
      CHARACTER ( len = 28 ) :: out_p_name
      CHARACTER ( len = 34 ) :: out_p_name_qplib
      CHARACTER ( len = 75 ) :: qplib_hi
      REAL ( KIND = rp_ ) :: mode_v
!     REAL ( KIND = rp_ ), DIMENSION( 100 ) :: V

      out = standard_out

      OPEN( input_spec, FILE = 'QPLIB.SPC', FORM = 'FORMATTED', STATUS = 'OLD')
      REWIND( input_spec )

!  read input Spec data

!  problem_type = 1(QP,default),2,(QCQP),3(BQP),4(LP),5(QCP)
!  append_dim   = F(don't append _n_m to name where n.m = dims,default), T(do)
!  qplib_wrfile = T(write to a file "probname".qplib), F(write to standard out)

!  set up algorithmic input data

      READ ( input_spec, * ) problem_type
      READ ( input_spec, * ) append_dim
      READ ( input_spec, * ) qplib_wrfile
      CLOSE ( input_spec )

!  open the problem data file

      OPEN ( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED', STATUS = 'OLD' )

!  sparse co-ordinate version

      h_pert = zero
      IF ( problem_type == qcqp .OR. problem_type == qcp ) THEN
        CALL CUTEST_lqp_create( status, input, buffer, out, n, m, f, G,        &
                                X, X_l, X_u, Z, Y, C_l, C_u, p_name,           &
                                X_names, C_names,                              &
                                X_type = X_type,                               &
                                A_ne = A_ne, A_row = A_row,                    &
                                A_col = A_col, A_val = A_val,                  &
                                H_ne = H_ne, H_row = H_row,                    &
                                H_col = H_col, H_val = H_val,                  &
                                H_pert = h_pert,                               &
                                dont_terminate_cutest = .TRUE. )
      ELSE
        CALL CUTEST_lqp_create( status, input, buffer, out, n, m, f, G,        &
                                X, X_l, X_u, Z, Y, C_l, C_u, p_name,           &
                                X_names, C_names,                              &
                                X_type = X_type,                               &
                                A_ne = A_ne, A_row = A_row,                    &
                                A_col = A_col, A_val = A_val,                  &
                                H_ne = H_ne, H_row = H_row,                    &
                                H_col = H_col, H_val = H_val,                  &
                                H_pert = h_pert )
      END IF
      CLOSE( input )
      IF ( status /= 0 ) GO TO 900

!  obtain output problem name

      out_p_name = REPEAT( ' ', 28 )
      IF ( append_dim ) THEN
        char_l = TRIM_INT( n )
        IF ( m > 0 ) THEN
          char_i = TRIM_INT( m )
          out_p_name = TRIM( p_name ) // '_' // TRIM( char_l )                 &
                                      // '_' // TRIM( char_i )
        ELSE
          out_p_name = TRIM( p_name ) // '_' // TRIM( char_l )
        END IF
      ELSE
        out_p_name = TRIM( p_name )
      END IF

!  open output file if required

      IF ( qplib_wrfile ) THEN
        out_p_name_qplib = REPEAT( ' ', 34 )
        out_p_name_qplib = TRIM( out_p_name ) // ".qplib"
        INQUIRE( FILE = out_p_name_qplib, EXIST = filexx )
        IF ( filexx ) THEN
           OPEN( qplib_out, FILE = out_p_name_qplib, FORM = 'FORMATTED',       &
                 STATUS = 'OLD', IOSTAT = status )
        ELSE
           OPEN( qplib_out, FILE = out_p_name_qplib, FORM = 'FORMATTED',       &
                 STATUS = 'NEW', IOSTAT = status )
        END IF
        IF ( status /= 0 ) GO TO 900
        out = qplib_out
        REWIND ( out )
      END IF

!  see if the problem has integer or binary variables

      int_var = COUNT( X_type( : n ) > 0 )
      bin_var = COUNT( X_type( : n ) == 2 )

      IF ( problem_type /= qcqp .AND. problem_type /= qcp ) THEN
        IF ( A_ne == 0 ) THEN
          IF ( H_ne > 0 ) THEN
            problem_type = bqp
          ELSE
            problem_type = lp
          END IF
        ELSE
          IF ( H_ne > 0 ) THEN
            problem_type = qp
          ELSE
            problem_type = lp
          END IF
        END IF
      END IF

!  set header

      IF ( m > 0 ) THEN
        IF ( LEN( TRIM( out_p_name ) ) <= 24 ) THEN
          WRITE( out, "( A24, ' CUTEst ', A, '.SIF with n = ', I0, ', m = ',   &
         &   I0 )" ) out_p_name( 1 : 24 ), TRIM( p_name ), n, m
        ELSE
          WRITE( out, "( A28, /, 24X, ' CUTEst ', A, '.SIF with n = ', I0,     &
         & ', m = ', I0 )" ) out_p_name, TRIM( p_name ), n, m
        END IF
      ELSE
        WRITE( out, "( A24, ' CUTEst ', A,                                     &
       &       '.SIF with n = ', I0 )" ) out_p_name, TRIM( p_name ), n
      END IF
      IF ( int_var == 0 ) THEN
        SELECT CASE ( problem_type )
        CASE ( qcqp )
          WRITE( out, "( 'QCQ                      a quadratic program',       &
         &               ' with quadratic constraints' )" )
        CASE ( bqp )
          WRITE( out, "( 'QCB                      a bound-constrained',       &
         &               ' quadratic program' )" )
        CASE ( lp )
          WRITE( out, "( 'LCL                      a linear program' )" )
        CASE ( qcp )
          WRITE( out, "( 'LCQ                      a linear program',          &
         &                ' with quadratic constraints' )" )
        CASE DEFAULT
          WRITE( out, "( 'QCL                      a quadratic program' )")
        END SELECT
      ELSE IF ( bin_var == n ) THEN
        SELECT CASE ( problem_type )
        CASE ( qcqp )
          WRITE( out, "( 'QBQ                      a binary',                  &
         &    ' QP with quadratic constraints' )" )
        CASE ( bqp )
          WRITE( out, "( 'QBB                      a binary',                  &
         &    ' bound-constrained quadratic program' )" )
        CASE ( lp )
          WRITE( out, "( 'LBL                      a binary',                  &
       &     ' linear program' )" )
        CASE ( qcp )
          WRITE( out, "( 'LBQ                      a binary',                  &
         &    ' LP with quadratic constraints' )" )
        CASE DEFAULT
          WRITE( out, "( 'QBL                      a binary',                  &
         &   ' quadratic program' )")
        END SELECT
      ELSE IF ( int_var == n ) THEN
        SELECT CASE ( problem_type )
        CASE ( qcqp )
          WRITE( out, "( 'QIQ                      an integer',                &
         &    ' QP with quadratic constraints' )" )
        CASE ( bqp )
          WRITE( out, "( 'QIB                      an integer',                &
         &    ' bound-constrained quadratic program' )" )
        CASE ( lp )
          WRITE( out, "( 'LIL                      an integer',                &
       &     ' linear program' )" )
        CASE ( qcp )
          WRITE( out, "( 'LIQ                      an integer',                &
         &    ' LP with quadratic constraints' )" )
        CASE DEFAULT
          WRITE( out, "( 'QIL                      an integer',                &
         &   ' quadratic program' )")
        END SELECT
      ELSE
        SELECT CASE ( problem_type )
        CASE ( qcqp )
          WRITE( out, "( 'QGQ                      a mixed-integer',           &
         &    ' QP with quadratic constraints' )" )
        CASE ( bqp )
          WRITE( out, "( 'QGB                      a mixed-integer',           &
         &    ' bound-constrained quadratic program' )" )
        CASE ( lp )
          WRITE( out, "( 'QGL                      a mixed-integer',           &
       &     ' linear program' )" )
        CASE ( qcp )
          WRITE( out, "( 'LGQ                      a mixed-integer',           &
         &    ' LP with quadratic constraints' )" )
        CASE DEFAULT
          WRITE( out, "( 'QGL                      a mixed-integer',           &
         &   ' quadratic program' )")
        END SELECT
      END IF
      WRITE( out, "( 'Minimize' )" )
      char_l = TRIM_INT( n )
      WRITE( out, "( A16, 8X, ' # variables ' )" ) char_l
      IF ( problem_type /= bqp ) THEN
        char_l = TRIM_INT( m )
        WRITE( out, "( A16, 8X, ' # general linear constraints ' )" ) char_l
      END IF

!  Hessian values

      IF ( problem_type == qp .OR. problem_type == bqp .OR.                    &
           problem_type == qcqp ) THEN
        char_l = TRIM_INT( H_ne )
        IF ( H_ne == 0 ) THEN
          WRITE( out, "( /, A16, 8X, ' # nonzeros in upper triangle of H' )" ) &
            char_l
        ELSE
          WRITE( out, "( /, A16, 8X, ' # nonzeros in upper triangle of H:',    &
         &   ' row,column,value' )" ) char_l
          DO l = 1, H_ne
            char_i = TRIM_INT( H_row( l ) ) ; char_j = TRIM_INT( H_col( l ) )
            char_val = TRIM_VALUE( H_val( l ) )
            WRITE( out, "( A16, 1X, A16, 1X, A24 )" ) char_i, char_j, char_val
          END DO
        END IF
      END IF

!  gradient values

      mode_v = MODE( n, G )
      l = COUNT( G( : n ) /= mode_v )
      char_l = TRIM_INT( l ) ; char_val = TRIM_VALUE( mode_v )
      WRITE( out, "( /, A24, ' default value for entries in g' )" ) char_val
      IF ( l == 0 ) THEN
        WRITE( out, "( A16, 8X, ' # non default entries in g' )" ) char_l
      ELSE
        WRITE( out, "( A16, 8X, ' # non default entries in g: index,value' )") &
          char_l
        DO i = 1, n
          IF ( G( i ) /= mode_v ) THEN
            char_i = TRIM_INT( i ) ; char_val = TRIM_VALUE( G( i ) )
            WRITE( out, "( A16, 1X, A24 )" ) char_i, char_val
          END IF
        END DO
      END IF

!  function value

      char_val = TRIM_VALUE( f )
      WRITE( out, "( /, A24, ' value of f' )" ) char_val

!  Hessian values for constraints

      IF ( problem_type == qcqp .OR. problem_type == qcp ) THEN
        ALLOCATE( X0( n ), STAT = status )
        IF ( status /= 0 ) GO TO 900
        X0 = zero
        nnzh_i = 0 ; lh = SIZE( H_val )

!  open a dummy file to store the Hessian values temporarily

        OPEN( qplib_out_dummy,  IOSTAT = status )
        IF ( status /= 0 ) GO TO 900

!  write the Hessian values for the ith constraint to the dummy file

        DO i = 1, m
          CALL CUTEST_cish( status, n, X0, i, nehi, lh, H_val, H_row, H_col )
          IF ( status /= 0 ) GO TO 900
          char_l = TRIM_INT( i )
          DO l = 1, nehi
            IF ( H_val( l ) /= zero ) THEN
             nnzh_i = nnzh_i + 1
             char_i = TRIM_INT( H_row( l ) ) ; char_j = TRIM_INT( H_col( l ) )
             char_val = TRIM_VALUE( H_val( l ) )
             WRITE( qplib_out_dummy, "( A16, 1X, A16, 1X, A16, 1X, A24 )" )  &
               char_l, char_i, char_j, char_val
            END IF
          END DO
        END DO
        DEALLOCATE( X0, stat = status )
        CALL CUTEST_cterminate( status )

!  record the total number of constraint Hessian values

        char_l = TRIM_INT( nnzh_i )
        WRITE( out, "( /, A16, 8X, ' # nonzeros in upper triangle',            &
       &  ' of the H_i')") char_l

!  append the constraint Hessian values to the qplib file

        IF ( nnzh_i > 0 ) THEN
          REWIND( qplib_out_dummy )
          DO l = 1, nnzh_i
            READ( qplib_out_dummy, "( A75 )" ) qplib_hi
            WRITE( qplib_out, "( A75 )" ) qplib_hi
          END DO
        END IF
        CLOSE( qplib_out_dummy )
      END IF

!  constraint Jacobian values

      IF ( problem_type /= bqp ) THEN
        char_l = TRIM_INT( A_ne )
        IF ( A_ne == 0 ) THEN
          WRITE( out, "( /, A16, 8X, ' # nonzeros in A' )" ) char_l
        ELSE
          WRITE( out, "( /, A16, 8X, ' # nonzeros in A:',                      &
         &   ' row,column,value' )" ) char_l
          DO l = 1, A_ne
            char_i = TRIM_INT( A_row( l ) ) ; char_j = TRIM_INT( A_col( l ) )
            char_val = TRIM_VALUE( A_val( l ) )
            WRITE( out, "( A16, 1X, A16, 1X, A24 )" ) char_i, char_j, char_val
          END DO
        END IF
      END IF

!  infinity

      char_val = TRIM_VALUE( infinity )
      WRITE( out, "( /, A24, ' value of infinite bounds' )" ) char_val

!  constraint lower bounds

      IF ( problem_type /= bqp ) THEN
        IF ( m > 0 ) THEN
          mode_v = MODE( m, C_l )
          l = COUNT( C_l( : m ) /= mode_v )
          char_l = TRIM_INT( l ) ; char_val = TRIM_VALUE( mode_v )
          WRITE( out, "( /, A24, ' default value for entries in c_l' )" )      &
            char_val
          IF ( l == 0 ) THEN
            WRITE( out, "( A16, 8X, ' # non default entries in c_l' )" ) char_l
          ELSE
            WRITE( out, "( A16, 8X, ' # non default entries in c_l:',          &
           &  ' index,value' )" ) char_l
            DO i = 1, m
              IF ( C_l( i ) /= mode_v ) THEN
                char_i = TRIM_INT( i ) ; char_val = TRIM_VALUE( C_l( i ) )
                WRITE( out, "( A16, 1X, A24 )" ) char_i, char_val
              END IF
            END DO
          END IF
        ELSE
          mode_v = - infinity_used
          l = 0
          char_l = TRIM_INT( l ) ; char_val = TRIM_VALUE( mode_v )
          WRITE( out, "( /, A24, ' default value for entries in c_l' )" )      &
            char_val
          WRITE( out, "( A16, 8X, ' # non default entries in c_l' )" ) char_l
        END IF

!  constraint upper bounds

        IF ( m > 0 ) THEN
          mode_v = MODE( m, C_u )
          l = COUNT( C_u( : m ) /= mode_v )
          char_l = TRIM_INT( l ) ; char_val = TRIM_VALUE( mode_v )
          WRITE( out, "( /, A24, ' default value for entries in c_u' )" )      &
            char_val
          IF ( l == 0 ) THEN
            WRITE( out, "( A16, 8X, ' # non default entries in c_u' )" ) char_l
          ELSE
            WRITE( out, "( A16, 8X, ' # non default entries in c_u:',          &
          &   ' index,value' )" ) char_l
            DO i = 1, m
              IF ( C_u( i ) /= mode_v ) THEN
                char_i = TRIM_INT( i ) ; char_val = TRIM_VALUE( C_u( i ) )
                WRITE( out, "( A16, 1X, A24 )" ) char_i, char_val
              END IF
            END DO
          END IF
        ELSE
          mode_v = infinity_used
          l = 0
          char_l = TRIM_INT( l ) ; char_val = TRIM_VALUE( mode_v )
          WRITE( out, "( /, A24, ' default value for entries in c_u' )" )      &
            char_val
          WRITE( out, "( A16, 8X, ' # non default entries in c_u' )" ) char_l
        END IF
      END IF

!  variable lower bounds

      IF ( bin_var < n ) THEN
        mode_v = MODE( n, X_l )
        l = 0
        DO i = 1, n
          IF ( X_l( i ) /= mode_v .AND. X_type( i ) /= 2 ) l = l + 1
        END DO
        char_l = TRIM_INT( l ) ; char_val = TRIM_VALUE( mode_v )
        WRITE( out, "( /, A24, ' default value for entries in x_l' )" ) char_val
        IF ( l == 0 ) THEN
          WRITE( out, "( A16, 8X, ' # non default entries in x_l' )" ) char_l
        ELSE
          WRITE( out, "( A16, 8X, ' # non default entries in x_l:',            &
         &  ' index,value' )" ) char_l
          DO i = 1, n
            IF ( X_l( i ) /= mode_v .AND. X_type( i ) /= 2 ) THEN
              char_i = TRIM_INT( i ) ; char_val = TRIM_VALUE( X_l( i ) )
              WRITE( out, "( A16, 1X, A24 )" ) char_i, char_val
            END IF
          END DO
        END IF

!  variable upper bounds

        mode_v = MODE( n, X_u )
        l = 0
        DO i = 1, n
          IF ( X_u( i ) /= mode_v .AND. X_type( i ) /= 2 ) l = l + 1
        END DO
        char_l = TRIM_INT( l ) ; char_val = TRIM_VALUE( mode_v )
        WRITE( out, "( /, A24, ' default value for entries in x_u' )" ) char_val
        IF ( l == 0 ) THEN
          WRITE( out, "( A16, 8X, ' # non default entries in x_u' )" ) char_l
        ELSE
          WRITE( out, "( A16, 8X, ' # non default entries in x_u:',            &
         &  ' index,value' )") char_l
          DO i = 1, n
            IF ( X_u( i ) /= mode_v .AND. X_type( i ) /= 2 ) THEN
              char_i = TRIM_INT( i ) ; char_val = TRIM_VALUE( X_u( i ) )
              WRITE( out, "( A16, 1X, A24 )" ) char_i, char_val
            END IF
          END DO
        END IF
      END IF

!  variable types

      IF ( int_var > 0 .AND. int_var < n ) THEN
        IF ( n >= 2 * int_var ) THEN
          char_l = TRIM_INT( 0 )
          WRITE( out, "( /, A16, 8X, ' default variable type',                 &
         &  ' (0 for continuous, 1 for integer)' )" ) char_l
          char_j = TRIM_INT( int_var )
          IF ( int_var == 0 ) THEN
            WRITE( out, "( A16, 8X, ' # non default variables' )" ) char_j
          ELSE
            WRITE( out, "( A16, 8X, ' # non default variables: index,type' )") &
           &  char_j
            DO i = 1, n
              IF (  X_type( i ) /= 0 ) THEN
                char_i = TRIM_INT( i ) ; char_j = TRIM_INT( X_type( i ) )
                WRITE( out, "( A16, 1X, A16 )" ) char_i, char_j
              END IF
            END DO
          END IF
        ELSE
          char_l = TRIM_INT( 1 )
          WRITE( out, "( /, A16, 8X, ' default variable type',                 &
         & ' (0 for continuous, 1 for integer)' )" ) char_l
          char_j = TRIM_INT( n - int_var )
          IF ( int_var == n ) THEN
            WRITE( out, "( A16, 8X, ' # non default variables' )" ) char_j
          ELSE
            WRITE( out, "( A16, 8X, ' # non default variables: index,type' )") &
              char_j
            DO i = 1, n
              IF (  X_type( i ) == 0 ) THEN
                char_i = TRIM_INT( i ) ; char_j = TRIM_INT( X_type( i ) )
                WRITE( out, "( A16, 1X, A16 )" ) char_i, char_j
              END IF
            END DO
          END IF
        END IF
      END IF

!  initial primal variables

      mode_v = MODE( n, X )
      l = COUNT( X( : n ) /= mode_v )
      char_l = TRIM_INT( l ) ; char_val = TRIM_VALUE( mode_v )
      WRITE( out, "( /, A24, ' default value for entries in initial x' )" )    &
        char_val
      IF ( l == 0 ) THEN
        WRITE( out, "( A16, 8X, ' # non default entries in x' )" ) char_l
      ELSE
        WRITE( out, "( A16, 8X, ' # non default entries in x: index,value' )") &
          char_l
        DO i = 1, n
          IF ( X( i ) /= mode_v ) THEN
            char_i = TRIM_INT( i ) ; char_val = TRIM_VALUE( X( i ) )
            WRITE( out, "( A16, 1X, A24 )" ) char_i, char_val
          END IF
        END DO
      END IF

!  initial Lagrange multipliers

      IF ( problem_type /= bqp ) THEN
        IF ( m > 0 ) THEN
          mode_v = MODE( m, Y )
          l = COUNT( Y( : m ) /= mode_v )
          char_l = TRIM_INT( l ) ; char_val = TRIM_VALUE( mode_v )
          WRITE( out, "( /, A24, ' default value for entries in initial y' )" )&
            char_val
          IF ( l == 0 ) THEN
            WRITE( out, "( A16, 8X, ' # non default entries in y' )" ) char_l
          ELSE
            WRITE( out, "( A16, 8X, ' # non default entries in y:',            &
           &  ' index,value' )" ) char_l
            DO i = 1, m
              IF ( Y( i ) /= mode_v ) THEN
                char_i = TRIM_INT( i ) ; char_val = TRIM_VALUE( Y( i ) )
                WRITE( out, "( A16, 1X, A24 )" ) char_i, char_val
              END IF
            END DO
          END IF
        ELSE
          mode_v = zero
          l = 0
          char_l = TRIM_INT( l ) ; char_val = TRIM_VALUE( mode_v )
          WRITE( out, "( /, A24, ' default value for entries in initial y' )" )&
            char_val
          WRITE( out, "( A16, 8X, ' # non default entries in y' )" ) char_l
        END IF
      END IF

!  initial dual variables

      mode_v = MODE( n, Z )
      l = COUNT( Z( : n ) /= mode_v )
      char_l = TRIM_INT( l ) ; char_val = TRIM_VALUE( mode_v )
      WRITE( out, "( /, A24, ' default value for entries in initial z' )" )    &
        char_val
      IF ( l == 0 ) THEN
        WRITE( out, "( A16, 8X, ' # non default entries in z' )" ) char_l
      ELSE
        WRITE( out, "( A16, 8X, ' # non default entries in z: index,value' )") &
          char_l
        DO i = 1, n
          IF ( Z( i ) /= mode_v ) THEN
            char_i = TRIM_INT( i ) ; char_val = TRIM_VALUE( Z( i ) )
            WRITE( out, "( A16, 1X, A24 )" ) char_i, char_val
          END IF
        END DO
      END IF

!  variable names

      char_l = TRIM_INT( n )
      WRITE( out, "( /, A16, 8X, ' # non default names for variables:',        &
     &   ' index,name' )" ) char_l
      DO i = 1, n
        char_i = TRIM_INT( i )
        WRITE( out, "( A16, 1X, A10 )" ) char_i, X_names( i )
      END DO

!  constraint names

      IF ( problem_type /= bqp ) THEN
        IF ( m > 0 ) THEN
          char_l = TRIM_INT( m )
          WRITE( out, "( /, A16, 8X, ' # non default names for constraints:',  &
         &   ' index,name' )" ) char_l
          DO i = 1, m
            char_i = TRIM_INT( i )
            WRITE( out, "( A16, 1X, A10 )" ) char_i, C_names( i )
          END DO
        ELSE
          char_l = TRIM_INT( 0 )
          WRITE( out, "( /, A16, 8X, ' # non default names for constraints:',  &
         &   ' index,name' )" ) char_l
        END IF
      END IF

      DEALLOCATE( A_row, A_col, H_row, H_col, X, X_l, X_u, Z, G, Y, C_l, C_u,  &
                  A_val, H_val, X_names, C_names, X_type, stat = status )

      IF ( qplib_wrfile ) CLOSE( qplib_out )

      STOP

!  error exits

 900  CONTINUE
      WRITE( out, "( ' error status = ', I0 )" ) status
      CLOSE( INPUT  )
      STOP

    CONTAINS

!  ------------------------ M O D E  F U N C T I O N --------------------------

      FUNCTION MODE( n, V )
      IMPLICIT NONE
      REAL ( KIND = rp_ ) :: MODE
      INTEGER, INTENT( IN ) :: n
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: V

!  find the "mode", i.e., the most commonly-occuring value, of a vector v

      INTEGER :: i, mode_start, max_len, same, len, m, inform

      REAL ( KIND = rp_ ), DIMENSION( n ) :: V_sorted

      IF ( n > 0 ) THEN

!  sort a copy of v into increasing order

        V_sorted = V
        CALL SORT_heapsort_build( n, V_sorted, inform ) !  build the heap
        DO i = 1, n
          m = n - i + 1
          CALL SORT_heapsort_smallest( m, V_sorted, inform ) !  reorder v
        END DO

!  run through the sorted values, finding adjacent entries that are identical

        mode_start = 1 ; max_len = 1
        same = 1 ; len = 1
        DO i = 2, n
          IF ( V_sorted( i ) /= V_sorted( same ) ) THEN
            IF ( len > max_len ) THEN
              mode_start = same
              max_len = len
            END IF
            same = i ; len = 1
          ELSE
            len = len + 1
          END IF
        END DO
        IF ( len > max_len ) THEN
          mode_start = same
          max_len = len
        END IF
!       write(6,*) max_len
!       write(6,*) V_sorted( : n )
        MODE = V_sorted( mode_start )
      ELSE
        MODE = zero
      END IF
      RETURN

      END FUNCTION MODE

!  --------------- H E A P S O R T   S U B R O U T I N E S -------------------

!  heapsort routines extracted from GALAHAD

      SUBROUTINE SORT_heapsort_build( n, A, inform )

!  Given an array A, elements A(1), ...., A(N), subroutine SORT_heapsort_build
!  rearranges the elements to form a heap in which each parent has a smaller
!  value than either of its children.

!  Algorithm 232 of CACM (J. W. J. Williams): a combination of
!  ALGOL procedures SETHEAP and INHEAP

!  Programming: Nick Gould, January 26th 1995.

!  ------------------------- dummy arguments --------------------------
!
!  n      integer, which gives the number of values to be sorted.
!         n must be positive
!
!  A      real array of length n. On input, A must contain the
!         values which are to be sorted. On output, these values
!         will have been permuted so as to form a heap
!
!  inform integer, which informs the user of the success of SORT_heapsort_build.
!         If inform = 0 on exit, the heap has been formed.
!         If inform = 1 on exit, n was input with a value less than
!                       or equal to 0 and the heap has not been formed.
!
!  ------------------ end of dummy arguments --------------------------

      IMPLICIT NONE
      INTEGER, INTENT( IN ) :: n
      INTEGER, INTENT( OUT ) :: inform
      REAL ( KIND = rp_ ), INTENT( INOUT ), DIMENSION( n ) :: A

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------

      INTEGER :: i, j, k
      REAL ( KIND = rp_ )  :: rin

!  Add the elements to the heap one at a time

      IF ( n <= 0 ) THEN
         inform = 1
         RETURN
      ENDIF

      DO k = 2, n
        rin = A( k )

!  The cycle may be repeated log2(k) times, but on average is repeated
!  merely twice

        i = k
        DO
          IF ( i <= 1 ) EXIT
          j = i / 2
          IF ( A( j ) <= rin ) EXIT
          A( i ) = A( j )
          i = j
        END DO
        A( i ) = rin
      END DO
      inform = 0

      RETURN

!  End of subroutine SORT_heapsort_build

     END SUBROUTINE SORT_heapsort_build

     SUBROUTINE SORT_heapsort_smallest( m, A, inform )

!  Given an array A, elements A(1), ...., A(m) forming a heap,
!  SORT_heapsort_smallest assigns to rout the value of A(1), the smallest
!  member of the heap, and arranges the remaining members as elements
!  1 to m - 1 of A. rout is then placed in A(m)

!  Algorithm 232 of CACM (J. W. J. Williams): a combination of
!  ALGOL procedures OUTHEAP and SWOPHEAP

!  Programming: Nick Gould, January 26th 1995.

!  ------------------------- dummy arguments --------------------------
!
!  m      integer, which gives the number of values to be sorted.
!         m must be positive
!
!  A      real array of length m. On input, A must contain the values which
!         are to be sorted stored in a heap. On output, the smallest value
!         will have been moved into A(m) and the remaining values A(k),
!         k = 1,..., m-1 will have been restored to a heap
!
!  inform integer, which informs the user of the success of
!         SORT_heapsort_smallest.
!         If inform = 0 on exit, the smallest value has been found.
!         If inform = 1 on exit, m was input with a value less than
!                       or equal to 0 and the heap has not been formed
!
!  ------------------ end of dummy arguments --------------------------

      IMPLICIT NONE
      INTEGER, INTENT( IN ) :: m
      INTEGER, INTENT( OUT ) :: inform
      REAL ( KIND = rp_ ), INTENT( INOUT ), DIMENSION( m ) :: A

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------

      INTEGER :: i, j
      REAL ( KIND = rp_ ) :: rin, rout

!  Add the element rin to the heap, extract and assign to rout
!  the value of the smallest member of the resulting set, and
!  leave the remaining elements in a heap of the original size.
!  In this process, elements 1 to n+1 of the array A may be disturbed

      IF ( m <= 0 ) THEN
         inform = 1
         RETURN
      ENDIF

      IF ( m > 1 ) THEN
        i = 1
        rout = A( 1 )
        rin = A( m )

!  Move from the top of the heap comparing the value of node i
!  with its two daughters. If node i is smallest, the heap has been
!  restored. If one of the children is smallest, promote this child
!  in the heap and move to the now vacated node.
!  This cycle may be repeated log2(m) times

        DO
          j = i + i
          IF ( j > m - 1 ) EXIT

!  Determine which of the two daughters is smallest

          IF ( A( j + 1 ) < A( j ) ) j = j + 1

!  Determine if the smaller daughter is less than the value from node i

          IF ( A( j ) >= rin ) EXIT
          A( i ) = A( j )
          i = j
        END DO

!  The heap has been restored

        A( i ) = rin

!  Store the smallest value in the now vacated m-th position of the list

        A( m ) = rout

      END IF
      inform = 0

      RETURN

!  End of subroutine SORT_heapsort_smallest

      END SUBROUTINE SORT_heapsort_smallest

!  ---------------------- T R I M _ I N T    F U N C T I O N ------------------

      FUNCTION TRIM_INT( i )
      CHARACTER ( LEN = 16 ) :: TRIM_INT
      INTEGER :: i

!  write integer as a left shifted length 16 character

      TRIM_INT = REPEAT( ' ', 16 )
      WRITE( TRIM_INT, "( I0 )" ) i
      RETURN

!  end of function TRIM_INT

      END FUNCTION TRIM_INT

!  --------------------- T R I M _ V A L U E   F U N C T I O N ----------------

      FUNCTION TRIM_VALUE( value )
      CHARACTER ( LEN = 24 ) :: TRIM_VALUE
      REAL ( KIND = rp_ ) :: value

!  write a real value into 24 characters trimming as much as possible
!  without losing precision

      INTEGER :: i, i_start, i_point, i_end, j, k, l, zs
      REAL ( KIND = rp_ ) :: minus_value
      LOGICAL :: zeros
      CHARACTER ( LEN = 22 ) :: field22
      CHARACTER ( LEN = 23 ) :: field
      CHARACTER ( LEN = 24 ) :: field24

!  cram value into 23 characters

!write(6,*) value
      IF ( value == 0.0_dp_ ) THEN
        field = "0.0         "
      ELSE IF ( SIGN( 1.0_dp_, value ) > 0.0_dp_ ) THEN
        IF ( value >= ( 10.0_dp_ ) ** 100 ) THEN
          WRITE( field24, "( ES24.15E3 )" ) value
          field = field24( 1 : 20 ) // field24( 22 : 24 )
        ELSE IF ( value >= ( 10.0_dp_ ) ** 16 ) THEN
          WRITE( field24, "( ES24.15 )" ) value
          field = field24( 1 : 21 ) // field24( 23 : 24 )
        ELSE IF ( value >= ( 10.0_dp_ ) ** 15 ) THEN
          WRITE( field, "( F23.0 )" ) value
        ELSE IF ( value >= ( 10.0_dp_ ) ** 14 ) THEN
          WRITE( field, "( F23.1 )" ) value
        ELSE IF ( value >= ( 10.0_dp_ ) ** 13 ) THEN
          WRITE( field, "( F23.2 )" ) value
        ELSE IF ( value >= ( 10.0_dp_ ) ** 12 ) THEN
          WRITE( field, "( F23.3 )" ) value
        ELSE IF ( value >= ( 10.0_dp_ ) ** 11 ) THEN
          WRITE( field, "( F23.4 )" ) value
        ELSE IF ( value >= ( 10.0_dp_ ) ** 10 ) THEN
          WRITE( field, "( F23.5 )" ) value
        ELSE IF ( value >= ( 10.0_dp_ ) ** 9 ) THEN
          WRITE( field, "( F23.6 )" ) value
        ELSE IF ( value >= ( 10.0_dp_ ) ** 8 ) THEN
          WRITE( field, "( F23.7 )" ) value
        ELSE IF ( value >= ( 10.0_dp_ ) ** 7 ) THEN
          WRITE( field, "( F23.8 )" ) value
        ELSE IF ( value >= ( 10.0_dp_ ) ** 6 ) THEN
          WRITE( field, "( F23.9 )" ) value
        ELSE IF ( value >= ( 10.0_dp_ ) ** 5 ) THEN
          WRITE( field, "( F23.10 )" ) value
        ELSE IF ( value >= ( 10.0_dp_ ) ** 4 ) THEN
          WRITE( field, "( F23.11 )" ) value
        ELSE IF ( value >= ( 10.0_dp_ ) ** 3 ) THEN
          WRITE( field, "( F23.12 )" ) value
        ELSE IF ( value >= ( 10.0_dp_ ) ** 2 ) THEN
          WRITE( field, "( F23.13 )" ) value
        ELSE IF ( value >= ( 10.0_dp_ ) ** 1 ) THEN
          WRITE( field, "( F23.14 )" ) value
        ELSE IF ( value >= ( 10.0_dp_ ) ** 0 ) THEN
          WRITE( field, "( F23.15 )" ) value
        ELSE IF ( value >= ( 10.0_dp_ ) ** ( - 1 ) ) THEN
          WRITE( field24, "( F24.16 )" ) value
          field = field24( 2 : 24 )
        ELSE IF ( value >= ( 10.0_dp_ ) ** ( - 2 ) ) THEN
          WRITE( field24, "( F24.17 )" ) value
          field = field24( 2 : 24 )
        ELSE IF ( value >= ( 10.0_dp_ ) ** ( - 3 ) ) THEN
          WRITE( field24, "( F24.18 )" ) value
          field = field24( 2 : 24 )
        ELSE IF ( value >= ( 10.0_dp_ ) ** ( - 4 ) ) THEN
          WRITE( field24, "( F24.16 )" ) value
          field = field24( 2 : 24 )
        ELSE IF ( value >= ( 10.0_dp_ ) ** ( - 9 ) ) THEN
          WRITE( field24, "( ES24.15 )" ) value
          field = field24( 1 : 22 ) // field24( 24 : 24 )
        ELSE IF ( value >= ( 10.0_dp_ ) ** ( - 99 ) ) THEN
          WRITE( field, "( ES23.15 )" ) value
!       ELSE IF ( value >= ( 10.0_dp_ ) ** ( - 999 ) ) THEN
!         WRITE( field, "( ES23.15E3 )" ) value
        ELSE
          WRITE( field, "( ES23.15E4 )" ) value
        END IF
      ELSE
        minus_value = - value
        IF ( ABS( minus_value - 1.0_dp_ ) <= teneps_d ) minus_value = 1.0_dp_
        IF ( minus_value >= ( 10.0_dp_ ) ** 100 ) THEN
          WRITE( field, "( ES23.15E3 )" ) minus_value
          field22 = field( 1 : 19 ) // field( 21 : 23 )
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** 16 ) THEN
          WRITE( field, "( ES23.15 )" ) minus_value
          field22 = field( 1 : 20 ) // field( 22 : 23 )
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** 15 ) THEN
          WRITE( field22, "( F22.0 )" ) minus_value
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** 14 ) THEN
          WRITE( field22, "( F22.1 )" ) minus_value
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** 13 ) THEN
          WRITE( field22, "( F22.2 )" ) minus_value
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** 12 ) THEN
          WRITE( field22, "( F22.3 )" ) minus_value
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** 11 ) THEN
          WRITE( field22, "( F22.4 )" ) minus_value
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** 10 ) THEN
          WRITE( field22, "( F22.5 )" ) minus_value
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** 9 ) THEN
          WRITE( field22, "( F22.6 )" ) minus_value
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** 8 ) THEN
          WRITE( field22, "( F22.7 )" ) minus_value
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** 7 ) THEN
          WRITE( field22, "( F22.8 )" ) minus_value
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** 6 ) THEN
          WRITE( field22, "( F22.9 )" ) minus_value
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** 5 ) THEN
          WRITE( field22, "( F22.10 )" ) minus_value
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** 4 ) THEN
          WRITE( field22, "( F22.11 )" ) minus_value
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** 3 ) THEN
          WRITE( field22, "( F22.12 )" ) minus_value
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** 2 ) THEN
          WRITE( field22, "( F22.13 )" ) minus_value
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** 1 ) THEN
          WRITE( field22, "( F22.14 )" ) minus_value
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** 0 ) THEN
          WRITE( field22, "( F22.15 )" ) minus_value
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** ( - 1 ) ) THEN
          WRITE( field, "( F23.16 )" ) minus_value
          field22 = field( 2 : 23 )
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** ( - 2 ) ) THEN
          WRITE( field, "( F23.17 )" ) minus_value
          field22 = field( 2 : 23 )
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** ( - 3 ) ) THEN
          WRITE( field, "( F23.18 )" ) minus_value
          field22 = field( 2 : 23 )
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** ( - 4 ) ) THEN
          WRITE( field, "( F23.15 )" ) minus_value
          field22 = field( 2 : 23 )
        ELSE IF ( minus_value >= ( 10.0_dp_ ) ** ( - 9 ) ) THEN
          WRITE( field, "( ES23.15 )" ) minus_value
          field22 = field( 1 : 21 ) // field( 23 : 23 )
        ELSE IF ( minus_value > ( 10.0_dp_ ) ** ( - 99 ) ) THEN
          WRITE( field22, "( ES22.15 )" ) minus_value
!       ELSE IF ( minus_value > ( 10.0_dp_ ) ** ( - 999 ) ) THEN
!         WRITE( field22, "( ES22.15E3 )" ) minus_value
        ELSE
          WRITE( field22, "( ES22.15E4 )" ) minus_value
        END IF
        field = "-" //  ADJUSTL( field22 )
      END IF

!  shift the value left

      field24 = ADJUSTL( field ) // ' '

!  find the positions of the first digit in the mantissa

      IF ( field24( 1 : 1 ) == '-' ) THEN
        i_start = 2
      ELSE
        i_start = 1
      END IF

!  find the positions of the decimal point and last digit in the mantissa

      i_point = 24 ; i_end = 1
      DO i = 1, 23
        IF ( field24( i : i ) == '.' ) i_point = i
        IF ( field24( i + 1 : i + 1 ) == ' ' .OR.                             &
             field24( i + 1 : i + 1 ) == 'e' .OR.                             &
             field24( i + 1 : i + 1 ) == 'E' .OR.                             &
             field24( i + 1 : i + 1 ) == 'd' .OR.                             &
             field24( i + 1 : i + 1 ) == 'D' ) THEN
          i_end = i
          EXIT
        END IF
      END DO

!     IF ( i_end - i_point >= 15 ) THEN
      IF ( i_end - i_start >= 15 ) THEN

!  round down any *01 to *00

        IF ( field24( i_end - 1 : i_end ) == '01' )                       &
          field24( i_end - 1 : i_end ) = '00'

!  round any *9r to **0r where ** = *+1

        IF ( field24( i_end - 1 : i_end ) == '99' ) THEN
          DO i = i_end, i_point + 1, - 1
            IF ( field24( i : i ) == '9' ) THEN
              field24( i : i ) = '0'
            ELSE
              READ( field24( i : i ), "( I1 )" ) l
              WRITE( field24( i : i ), "( I1 )" ) l + 1
              EXIT
            END IF
            IF ( i == i_point + 1 ) THEN
              DO j = i_point - 1, i_start, - 1
                IF ( field24( j : j ) == '9' ) THEN
                  field24( j : j ) = '0'
                ELSE
                  READ( field24( j : j ), "( I1 )" ) l
                  WRITE( field24( j : j ), "( I1 )" ) l + 1
                  EXIT
                END IF
                IF ( j == i_start ) THEN
                  DO l = i_end - 1, i_start, - 1
                    field24( l + 1 : l + 1 ) = field24( l : l )
                  END DO
                  field24( i_start : i_start ) = '1'
                END IF
              END DO
            END IF
          END DO
        END IF
      END IF

!     field24 = REPEAT( ' ', 24 )
!     IF ( value > - 10.0_rp_ .AND. value < 10.0_rp_ ) THEN
!       WRITE( field24, "( F19.16 )" ) value
!     ELSE
!       WRITE( field24, "( ES23.16 )" ) value
!     END IF

      TRIM_VALUE = field24

!  remove any leading space

!     IF ( TRIM_VALUE( 1 : 1 ) == ' ' ) THEN
!       DO i = 2, 24
!         TRIM_VALUE( i - 1 : i - 1 ) = TRIM_VALUE( i : i )
!       END DO
!     END IF

      zeros = .FALSE.
      DO i = 1, 24
        IF ( TRIM_VALUE( i : i ) == '0' ) THEN
          IF ( .NOT. zeros ) THEN
            zs = i
            zeros = .TRUE.
          END IF
        ELSE IF ( TRIM_VALUE( i : i ) == 'E' .OR.                              &
                  TRIM_VALUE( i : i ) == 'e' .OR.                              &
                  TRIM_VALUE( i : i ) == 'D' .OR.                              &
                  TRIM_VALUE( i : i ) == 'd' ) THEN
          IF ( zeros ) THEN
            DO j = zs + 1, zs + 25 - i
              k = i + ( j - zs - 1 )
              TRIM_VALUE( j : j ) = TRIM_VALUE( k : k  )
            END DO
            DO j = zs + 26 - i, 24
              TRIM_VALUE( j : j ) = ' '
            END DO
          END IF
          zeros = .FALSE.
          EXIT
        ELSE IF ( TRIM_VALUE( i : i ) == ' ' ) THEN
          IF ( zeros ) THEN
            DO j = zs + 1, i
              TRIM_VALUE( j : j ) = ' '
            END DO
          END IF
          zeros = .FALSE.
          EXIT
        ELSE
          zeros = .FALSE.
        END IF
      END DO
      IF ( zeros ) THEN
        DO j = zs + 1, i
          TRIM_VALUE( j : j ) = ' '
        END DO
      END IF

!  remove superflous 0 from the exponent

      DO i = 1, 24
        IF ( TRIM_VALUE( i : i ) == 'E' .OR.                                   &
             TRIM_VALUE( i : i ) == 'e' .OR.                                   &
             TRIM_VALUE( i : i ) == 'D' .OR.                                   &
             TRIM_VALUE( i : i ) == 'd' ) THEN
          IF ( TRIM_VALUE( i + 1 : i + 1 ) == '+' .OR.                         &
               TRIM_VALUE( i + 1 : i + 1 ) == '-' ) THEN
            IF ( TRIM_VALUE( i + 2 : i + 2 ) == '0' ) THEN
              IF ( TRIM_VALUE( i + 3 : i + 3 ) == '0' ) THEN
                IF ( TRIM_VALUE( i + 4 : i + 4 ) == ' ' ) THEN
                  TRIM_VALUE( i + 3 : i + 3 ) = '0'
                ELSE
                  TRIM_VALUE( i + 2 : i + 2 ) = TRIM_VALUE( i + 4 : i + 4 )
                  TRIM_VALUE( i + 3 : i + 4 ) = '  '
                END IF
              ELSE
                IF ( TRIM_VALUE( i + 4 : i + 4 ) == ' ' ) THEN
                ELSE
                  TRIM_VALUE( i + 2 : i + 2 ) = TRIM_VALUE( i + 3 : i + 3 )
                  TRIM_VALUE( i + 3 : i + 3 ) = TRIM_VALUE( i + 4 : i + 4 )
                  TRIM_VALUE( i + 4 : i + 4 ) = ' '
                END IF
              END IF
            END IF
          ELSE
            IF ( TRIM_VALUE( i + 1 : i + 1 ) == '0' ) THEN
              IF ( TRIM_VALUE( i + 2 : i + 2 ) == '0' ) THEN
                IF ( TRIM_VALUE( i + 3 : i + 3 ) == ' ' ) THEN
                  TRIM_VALUE( i + 2 : i + 2 ) = '0'
                ELSE
                  TRIM_VALUE( i + 1 : i + 1 ) = TRIM_VALUE( i + 3 : i + 3 )
                  TRIM_VALUE( i + 2 : i + 3 ) = '  '
                END IF
              ELSE
                IF ( TRIM_VALUE( i + 3 : i + 3 ) == ' ' ) THEN
                ELSE
                  TRIM_VALUE( i + 1 : i + 1 ) = TRIM_VALUE( i + 2 : i + 2 )
                  TRIM_VALUE( i + 2 : i + 2 ) = TRIM_VALUE( i + 3 : i + 3 )
                  TRIM_VALUE( i + 3 : i + 3 ) = ' '
                END IF
              END IF
            END IF
          END IF
          EXIT
        END IF

!  remove trailing 0 unless it is preceeded by a .

        IF ( TRIM_VALUE( i : i ) == ' ' ) THEN
          IF ( i < 3 ) EXIT
          IF ( TRIM_VALUE( i - 1 : i - 1 ) == '0' .AND.                        &
               TRIM_VALUE( i - 2 : i - 2 ) /= '.' ) THEN
               TRIM_VALUE( i - 1 : i - 1 ) = ' '
          END IF
          EXIT
        END IF

      END DO

!  if the string starts with a ., add a 0 at the front

      IF ( TRIM_VALUE( 1 : 1 ) == '.' ) THEN
        DO i = 24, 2, -1
          TRIM_VALUE( i : i ) = TRIM_VALUE( i - 1 : i - 1 )
        END DO
        TRIM_VALUE( 1 : 1 ) = '0'
      END IF

!  if the string starts with a ., add a 0 at the front

      IF ( TRIM_VALUE( 1 : 1 ) == '.' ) THEN
        DO i = 24, 2, -1
          TRIM_VALUE( i : i ) = TRIM_VALUE( i - 1 : i - 1 )
        END DO
        TRIM_VALUE( 1 : 1 ) = '0'
      END IF

!  if the string starts with a -., replace by -0. at the front

      IF ( TRIM_VALUE( 1 : 2 ) == '-.' ) THEN
        DO i = 24, 3, -1
          TRIM_VALUE( i : i ) = TRIM_VALUE( i - 1 : i - 1 )
        END DO
        TRIM_VALUE( 2 : 2 ) = '0'
      END IF
      RETURN

!  end of function TRIM_VALUE

      END FUNCTION TRIM_VALUE

    END PROGRAM CUTEST_qplib_main


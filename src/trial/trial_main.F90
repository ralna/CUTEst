! THIS VERSION: CUTEST 2.6 - 2026-01-16 AT 15:40 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-*- C U T E S T  t r i a l _ m a i n  P R O G R A M -*-*-*-*-*-*-*-

    PROGRAM CUTEST_trial_main

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
      REAL ( KIND = rp_ ), PARAMETER :: one = 1.0_rp_
      REAL ( KIND = rp_ ), PARAMETER :: zero = 0.0_rp_

!--------------------------------
!   L o c a l   V a r i a b l e s
!--------------------------------

      INTEGER ( KIND = ip_ ) :: n, m, H_ne
      INTEGER ( KIND = ip_ ) :: status, alloc_stat
      INTEGER ( KIND = ip_ ) :: l_h
      INTEGER ( KIND = ip_ ) :: iprob
      INTEGER ( KIND = ip_ ) :: nonlinear_variables_objective
      INTEGER ( KIND = ip_ ) :: nonlinear_variables_constraints
      INTEGER ( KIND = ip_ ) :: equality_constraints, linear_constraints
      LOGICAL :: only_print_small
      LOGICAL :: noobj
      CHARACTER ( len = 10 ) ::  p_name
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: X_type
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: H_row, H_col
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: HE_row, HE_row_ptr
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, X_l, X_u, G, Ji
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: Y, C_l, C_u, C
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: G_val, H_val
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: VECTOR, RESULT
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : , : ) :: H_band
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATION, LINEAR
      CHARACTER ( len = 10 ), ALLOCATABLE, DIMENSION( : ) :: X_names, C_names

!  open the problem data file

      OPEN ( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED', STATUS = 'OLD' )

!  decide whether this problem has general constraints

      CALL CUTEST_cdimen_r( status, input, n, m )
      only_print_small = .TRUE.

      IF ( m == 0 ) THEN

!  ======================== Test unconstrained tools ==========================

        WRITE( out, "( /, ' This problem is unconstrained', / )" )

!  allocate basic arrays

        CALL CUTEST_udimen_r( status, input, n )
        WRITE( out, "( ' * n = ', I0 )" ) n
        ALLOCATE( X( n ), X_l( n ), X_u( n ), G( n ), VECTOR( n ), RESULT( n ),&
                  X_names( n ), X_type( n ), stat = alloc_stat )
        IF ( alloc_stat /= 0 ) GO TO 990

!  set up SIF data

        CALL CUTEST_usetup_r( status, input, out, buffer, n, X, X_l, X_u )
        IF ( status /= 0 ) GO to 900
!       IF ( only_print_small )                                                &
!         CALL WRITE_X( out, n, X, X_l, X_u )

!X = (/ 1.1_wp, 2.2_wp, 3.3_wp, 4.4_rp_ /)

!  obtain variable and problem names

        CALL CUTEST_unames_r( status, n, p_name, X_names )
        IF ( status /= 0 ) GO to 900
        CALL WRITE_p_name( out, p_name )
!       IF ( only_print_small )                                                &
!         CALL WRITE_X_names( out, n, X_names )

!  obtain problem name

        CALL CUTEST_probname_r( status, p_name )
        IF ( status /= 0 ) GO to 900
        CALL WRITE_p_name( out, p_name )

!  obtain variable names

        CALL CUTEST_varnames_r( status, n, X_names )
        IF ( status /= 0 ) GO to 900
!       IF ( only_print_small )                                                &
!         CALL WRITE_X_names( out, n, X_names )

!  obtain variable types

        CALL CUTEST_uvartype_r( status, n, X_type )
        IF ( status /= 0 ) GO to 900
!       IF ( only_print_small )                                                &
!         CALL WRITE_X_type( out, n, X_type )

!  compute the number of nonzeros in the sparse Hessian

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
        IF ( only_print_small )                                                &
          CALL WRITE_H_sparsity_pattern( out, H_ne, l_h, H_row, H_col )

!  compute the sparse Hessian value

        WRITE( out, "( ' Call CUTEST_ush' )" )
        CALL CUTEST_ush_r( status, n, X, H_ne, l_h, H_val, H_row, H_col )
        IF ( status /= 0 ) GO to 900
        IF ( only_print_small )                                                &
          CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )

        DEALLOCATE( X_type, H_row, H_col, HE_row, HE_row_ptr, X,   &
                    X_l, X_u, G, H_val, VECTOR, RESULT, H_band,&
                    X_names, stat = alloc_stat )

!  ========================= Test constrained tools ===========================

      ELSE
        WRITE( out, "( /, ' This problem is constrained', / )" )

!  allocate basic arrays

        CALL CUTEST_cnoobj_r( status, input, noobj )
        IF ( noobj ) THEN
          WRITE( out, "( ' there is no objective function' )" )
        ELSE
          WRITE( out, "( ' there is an objective function' )" )
        END IF

        ALLOCATE( X( n ), X_l( n ), X_u( n ), G( n ), Ji( n ),                 &
                  X_names( n ), X_type( n ), stat = alloc_stat )
        IF ( alloc_stat /= 0 ) GO TO 990
        ALLOCATE( C( m ), Y( m ), C_l( m ), C_u( m ), C_names( m ),            &
                  EQUATION( m ), LINEAR( m ), stat = alloc_stat )
        IF ( alloc_stat /= 0 ) GO TO 990
        ALLOCATE( VECTOR( MAX( n, m ) ), RESULT( MAX( n, m ) ),                &
                  stat = alloc_stat )
        IF ( alloc_stat /= 0 ) GO TO 990

!  set up SIF data

        CALL CUTEST_csetup_r( status, input, out, buffer, n, m, X, X_l, X_u,   &
                        Y, C_l, C_u, EQUATION, LINEAR, 1_ip_, 1_ip_, 1_ip_ )
        IF ( status /= 0 ) GO to 900

!  obtain numbers of nonlinear variables, and equality and linear constraints

        CALL CUTEST_cstats_r( status, nonlinear_variables_objective,           &
                              nonlinear_variables_constraints,                 &
                              equality_constraints, linear_constraints )
        IF ( status /= 0 ) GO to 900
        WRITE( out, "( ' * nonlinear_variables_objective = ', I0, /,           &
   &                 ' * nonlinear_variables_constraints = ', I0, /,           &
   &                 ' * equality_constraints = ', I0, /,                      &
   &                 ' * linear_constraints = ', I0 )" )                       &
         nonlinear_variables_objective, nonlinear_variables_constraints,       &
         equality_constraints, linear_constraints

!  obtain variable and problem names

        CALL CUTEST_cnames_r( status, n, m, p_name, X_names, C_names )
        IF ( status /= 0 ) GO to 900
        CALL WRITE_p_name( out, p_name )

!  obtain constraint names

        CALL CUTEST_connames_r( status, m, C_names )
        IF ( status /= 0 ) GO to 900

!  obtain variable types

        CALL CUTEST_cvartype_r( status, n, X_type )
        IF ( status /= 0 ) GO to 900

!  compute the number of nonzeros in the sparse Hessian

        CALL CUTEST_cdimsh_r( status, H_ne )
        IF ( status /= 0 ) GO to 900
        WRITE( out, "( ' * H_ne = ', I0 )" ) H_ne

        l_h = H_ne
        ALLOCATE( H_val( l_h ), H_row( l_h ), H_col( l_h ), stat = alloc_stat )
        IF ( alloc_stat /= 0 ) GO TO 990

!  compute the sparsity pattern of the Hessian

        WRITE( out, "( ' Call CUTEST_cshp' )" )
        CALL CUTEST_cshp_r( status, n, H_ne, l_h, H_row, H_col )
        IF ( status /= 0 ) GO to 900
        IF ( only_print_small )                                                &
          CALL WRITE_H_sparsity_pattern( out, H_ne, l_h, H_row, H_col )

!  compute the sparse Hessian value

!Y = 1.0_rp_

        WRITE( out, "( ' CALL CUTEST_csh' )" )
        CALL CUTEST_csh_r( status, n, m, X, Y,                                 &
                           H_ne, l_h, H_val, H_row, H_col )
        IF ( status /= 0 ) GO to 900
        IF ( only_print_small )                                                &
          CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )

!  compute the sparsity pattern of the Hessian of the objective or a constraint

        iprob = 0
        WRITE( out, "( ' Call CUTEST_cishp for the objective' )" )
        CALL CUTEST_cishp_r( status, n, iprob, H_ne, l_h, H_row, H_col )
        IF ( status /= 0 ) GO to 900
        IF ( only_print_small )                                                &
          CALL WRITE_H_sparsity_pattern( out, H_ne, l_h, H_row, H_col )
        iprob = 1
        WRITE( out, "( ' Call CUTEST_cishp for a constraint' )" )
        CALL CUTEST_cishp_r( status, n, iprob, H_ne, l_h, H_row, H_col )
        IF ( status /= 0 ) GO to 900
        IF ( only_print_small )                                                &
          CALL WRITE_H_sparsity_pattern( out, H_ne, l_h, H_row, H_col )

!  compute the sparse Hessian value of the objective or a constraint

        iprob = 0
        WRITE( out, "( ' CALL CUTEST_cish for objective' )" )
        CALL CUTEST_cish_r( status, n, X, iprob,                               &
                            H_ne, l_h, H_val, H_row, H_col )
        IF ( status /= 0 ) GO to 900
        IF ( only_print_small )                                                &
          CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )
        iprob = 1
        WRITE( out, "( ' CALL CUTEST_cish for a constraint' )" )
        CALL CUTEST_cish_r( status, n, X, iprob,                               &
                            H_ne, l_h, H_val, H_row, H_col )
        IF ( status /= 0 ) GO to 900
        IF ( only_print_small )                                                &
          CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )

        DEALLOCATE( X_type, H_row, H_col, HE_row, HE_row_ptr, X,   &
                    X_l, X_u, G, Ji, Y, C_l, C_u, C, H_val,  &
                    VECTOR, RESULT, g_val, &
                    X_names, C_names, EQUATION, LINEAR, stat = alloc_stat )
      END IF
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

!     SUBROUTINE WRITE_X( out, n, X, X_l, X_u )
!     INTEGER ( KIND = ip_ ) :: n, out
!     REAL ( KIND = rp_ ), DIMENSION( n ) :: X, X_l, X_u
!     WRITE( out, "( ' *       i      X_l          X          X_u' )" )
!     DO i = 1, n
!       WRITE( out, "( ' * ', I7, 3ES12.4 )" ) i, X_l( i ), X( i ), X_u( i )
!     END DO
!     END SUBROUTINE WRITE_X

!     SUBROUTINE WRITE_Y( out, m, Y, C_l, C_u, EQUATION, LINEAR )
!     INTEGER ( KIND = ip_ ) :: m, out
!     REAL ( KIND = rp_ ), DIMENSION( m ) :: Y, C_l, C_u
!     LOGICAL, DIMENSION( m ) :: EQUATION, LINEAR
!     WRITE( out, "( ' *       i      C_l         C_u          Y   ',          &
!   &   '      EQUATION   LINEAR' )" )
!     DO i = 1, m
!       WRITE( out, "( ' * ', I7, 3ES12.4, 2L10 )" ) i, C_l( i ), C_u( i ),    &
!         Y( i ), EQUATION( i ), LINEAR( i )
!     END DO
!     END SUBROUTINE WRITE_Y

!     SUBROUTINE WRITE_X_type( out, n, X_type )
!     INTEGER ( KIND = ip_ ) :: n, out
!     INTEGER ( KIND = ip_ ), DIMENSION( n ) :: X_type
!     INTEGER ( KIND = ip_ ) :: i
!     WRITE( out, "( ' *       i  X_type' )" )
!     DO i = 1, n
!       WRITE( out, "( ' * ', I7, 2X, I0 )" ) i, X_type( i )
!     END DO
!     END SUBROUTINE WRITE_X_type

      SUBROUTINE WRITE_p_name( out, p_name )
      INTEGER ( KIND = ip_ ) :: out
      CHARACTER ( len = 10 ) ::  p_name
      WRITE( out, "( ' * p_name = ', A )" ) p_name
      END SUBROUTINE WRITE_p_name

 !    SUBROUTINE WRITE_X_names( out, n, X_names )
 !    INTEGER ( KIND = ip_ ) :: n, out
 !    CHARACTER ( len = 10 ), DIMENSION( n ) :: X_names
 !    INTEGER ( KIND = ip_ ) :: i
 !    WRITE( out, "( ' *       i  X_name' )" )
 !    DO i = 1, n
 !      WRITE( out, "( ' * ', I7, 2X, A10 )" ) i, X_names( i )
 !    END DO
 !    END SUBROUTINE WRITE_X_names

 !    SUBROUTINE WRITE_C_names( out, m, C_names )
 !    INTEGER ( KIND = ip_ ) :: m, out
!     CHARACTER ( len = 10 ), DIMENSION( m ) :: C_names
!     INTEGER ( KIND = ip_ ) :: i
!     WRITE( out, "( ' *       i  C_name' )" )
!     DO i = 1, m
!       WRITE( out, "( ' * ', I7, 2X, A10 )" ) i, C_names( i )
!     END DO
!     END SUBROUTINE WRITE_C_names

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

    END PROGRAM CUTEST_trial_main

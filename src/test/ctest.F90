! THIS VERSION: CUTEST 2.2 - 2024-08-27 AT 09:25 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*- C U T E S T  t e s t _ c o n s t r a i n e d _ t o o l s  P R O G R A M -*-

    PROGRAM CUTEST_test_constrained_tools

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

      INTEGER ( KIND = ip_ ) :: n, m, H_ne, HE_nel, HE_val_ne, HE_row_ne
      INTEGER ( KIND = ip_ ) :: J_ne, Ji_ne, status, l_g, G_ne, alloc_stat
      INTEGER ( KIND = ip_ ) :: l_h2_1, l_h, lhe_ptr, lhe_val, lhe_row, l_ohp
      INTEGER ( KIND = ip_ ) :: nonlinear_variables_objective
      INTEGER ( KIND = ip_ ) :: nonlinear_variables_constraints
      INTEGER ( KIND = ip_ ) :: equality_constraints, linear_constraints
      INTEGER ( KIND = ip_ ) :: l_j2_1, l_j2_2, l_j, icon, iprob
      INTEGER ( KIND = ip_ ) :: CHP_ne, l_chp, OHP_ne, nnz_vector, nnz_result
      REAL ( KIND = rp_ ) :: f, ci, y0
      LOGICAL :: grad, byrows, goth, gotj, grlagf, jtrans, noobj
      CHARACTER ( len = 10 ) ::  p_name
      CHARACTER ( len = 30 ) :: classification
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: X_type
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: H_row, H_col
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: HE_row, HE_row_ptr
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: HE_val_ptr
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: G_var, J_var, J_fun
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: CHP_ind, CHP_ptr
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: OHP_ind
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: INDEX_nz_vector
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: INDEX_nz_result
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, X_l, X_u, G, Ji
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: Y, C_l, C_u, C, J_val
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: G_val, H_val, HE_val
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: VECTOR, RESULT
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: CHP_val, OHP_val
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : , : ) :: H2_val
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : , : ) :: J2_val
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATION, LINEAR
      CHARACTER ( len = 10 ), ALLOCATABLE, DIMENSION( : ) :: X_names, C_names
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 7 )

!  open the problem data file

      OPEN ( input, FILE = 'c_OUTSDIF.d', FORM = 'FORMATTED', STATUS = 'OLD' )

!  allocate basic arrays

      WRITE( out, "( ' CALL CUTEST_cdimen ' )" )
      CALL CUTEST_cdimen_r( status, input, n, m )
      WRITE( out, "( ' * n = ', I0, ', m = ', I0 )" ) n, m
      WRITE( out, "( ' CALL CUTEST_cnoobj ' )" )
      CALL CUTEST_cnoobj_r( status, input, noobj )
      IF ( noobj ) THEN
        WRITE( out, "( ' there is no objective function' )" )
      ELSE
        WRITE( out, "( ' there is an objective function' )" )
      END IF
      l_h2_1 = n
      ALLOCATE( X( n ), X_l( n ), X_u( n ), G( n ), Ji( n ),                   &
                X_names( n ), X_type( n ), INDEX_nz_vector( n ),               &
                INDEX_nz_result( n ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990
      ALLOCATE( C( m ), Y( m ), C_l( m ), C_u( m ), C_names( m ),              &
                EQUATION( m ), LINEAR( m ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990
      ALLOCATE( VECTOR( MAX( n, m ) ), RESULT( MAX( n, m ) ),                  &
                stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990
      ALLOCATE( H2_val( l_h2_1, n ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990
      l_j2_1 = MAX( m, n ) ; l_j2_2 = l_j2_1
      ALLOCATE( J2_val( l_j2_1, l_j2_2 ), stat = alloc_stat )
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

      WRITE( out, "( ' CALL CUTEST_csetup ' )" )
      CALL CUTEST_csetup_r( status, input, out, buffer, n, m, X, X_l, X_u,     &
                      Y, C_l, C_u, EQUATION, LINEAR, 1_ip_, 1_ip_, 1_ip_ )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_X( out, n, X, X_l, X_u )
      CALL WRITE_Y( out, m, Y, C_l, C_u, EQUATION, LINEAR )

      X( 1 : 2 )  = (/ 1.1_rp_, 2.2_rp_ /)

!  obtain numbers of nonlinear variables, and equality and linear constraints

      WRITE( out, "( ' CALL CUTEST_cstats' )" )
      CALL CUTEST_cstats_r( status, nonlinear_variables_objective,             &
                          nonlinear_variables_constraints,                     &
                          equality_constraints, linear_constraints )
      IF ( status /= 0 ) GO TO 900
      WRITE( out, "( ' * nonlinear_variables_objective = ', I0, /,             &
     &               ' * nonlinear_variables_constraints = ', I0, /,           &
     &               ' * equality_constraints = ', I0, /,                      &
     &               ' * linear_constraints = ', I0 )" )                       &
       nonlinear_variables_objective, nonlinear_variables_constraints,         &
       equality_constraints, linear_constraints

!  obtain variable and problem names

      WRITE( out, "( ' CALL CUTEST_cnames' )" )
      CALL CUTEST_cnames_r( status, n, m, p_name, X_names, C_names )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_p_name( out, p_name )
      CALL WRITE_X_names( out, n, X_names )
      CALL WRITE_C_names( out, m, C_names )

!  obtain constraint names

      WRITE( out, "( ' Call CUTEST_connames' )" )
      CALL CUTEST_connames_r( status, m, C_names )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_C_names( out, m, C_names )

!  obtain variable types

      WRITE( out, "( ' CALL CUTEST_cvartype' )" )
      CALL CUTEST_cvartype_r( status, n, X_type )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_X_type( out, n, X_type )

!  compute the objective and constraint function values

      WRITE( out, "( ' CALL CUTEST_cfn' )" )

      CALL CUTEST_cfn_r( status, n, m, X, f, C )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_f( out, f )
      CALL WRITE_C( out, m, C )

!  compute the objective function value

      WRITE( out, "( ' CALL CUTEST_cifn for the objective function' )" )
      icon = 0
      CALL CUTEST_cifn_r( status, n, icon, X, f )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_f( out, f )

!  compute a constraint value

      WRITE( out, "( ' CALL CUTEST_cifn for a constraint' )" )
      icon = 1
      CALL CUTEST_cifn_r( status, n, icon, X, ci )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_CI( out, icon, ci )

!  compute the constraint function values alone

      WRITE( out, "( ' CALL CUTEST_ccf' )" )

      CALL CUTEST_ccf_r( status, n, m, X, C )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_C( out, m, C )

!  compute the gradient and dense Jacobian values

      grlagf = .TRUE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CUTEST_cgr with grlagf = .TRUE. and ',             &
     &               'jtrans = .TRUE.' )" )
      CALL CUTEST_cgr_r( status, n, m, X, Y, grlagf, G, jtrans,                &
                         l_j2_1, l_j2_2, J2_val )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_G( out, n, G )
      CALL WRITE_JT_dense( out, n, m, l_j2_1, l_j2_2, J2_val )
      grlagf = .TRUE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CUTEST_cgr with grlagf = .TRUE. and ',             &
     &               'jtrans = .FALSE.' )" )
      CALL CUTEST_cgr_r( status, n, m, X, Y, grlagf, G, jtrans,                &
                         l_j2_1, l_j2_2, J2_val )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_G( out, n, G )
      CALL WRITE_J_dense( out, n, m, l_j2_1, l_j2_2, J2_val )
      grlagf = .FALSE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CUTEST_cgr with grlagf = .FALSE. and ',            &
     &               'jtrans = .TRUE.' )" )
      CALL CUTEST_cgr_r( status, n, m, X, Y, grlagf, G, jtrans,                &
                         l_j2_1, l_j2_2, J2_val )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_G( out, n, G )
      CALL WRITE_JT_dense( out, n, m, l_j2_1, l_j2_2, J2_val )
      grlagf = .FALSE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CUTEST_cgr with grlagf = .FALSE. and ',            &
     &               'jtrans = .FALSE.' )" )
      CALL CUTEST_cgr_r( status, n, m, X, Y, grlagf, G, jtrans,                &
                         l_j2_1, l_j2_2, J2_val )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_G( out, n, G )
      CALL WRITE_J_dense( out, n, m, l_j2_1, l_j2_2, J2_val )

!  compute the objective function and gradient values

      grad = .FALSE.
      WRITE( out, "( ' CALL CUTEST_cofg with grad = .FALSE.' )" )
      CALL CUTEST_cofg_r( status, n, X, f, G, grad )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_f( out, f )
      grad = .TRUE.
      WRITE( out, "( ' CALL CUTEST_cofg with grad = .TRUE.' )" )
      CALL CUTEST_cofg_r( status, n, X, f, G, grad )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_f( out, f )
      CALL WRITE_G( out, n, G )

!  compute just its gradient

      icon = 0
      WRITE( out, "( ' CALL CUTEST_cigr for the objective function' )" )
      CALL CUTEST_cigr_r( status, n, icon, X, G )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_G( out, n, G )

!  compute the objective function and sparse gradient values

      l_g = n
      ALLOCATE( G_val( l_g ), G_var( l_g ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990
      grad = .FALSE.
      WRITE( out, "( ' CALL CUTEST_cofsg with grad = .FALSE.' )" )
      CALL CUTEST_cofsg_r( status, n, X, f, G_ne, l_g, G_val, G_var, grad )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_f( out, f )
      grad = .TRUE.
      WRITE( out, "( ' CALL CUTEST_cofsg with grad = .TRUE.' )" )
      CALL CUTEST_cofsg_r( status, n, X, f, G_ne, l_g, G_val, G_var, grad )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_f( out, f )
      CALL WRITE_SG( out, G_ne, l_g, G_val, G_var )

!  compute the number of nonzeros in the sparse gradient of the objective

      WRITE( out, "( ' CALL CUTEST_cdimsg' )" )
      CALL CUTEST_cdimsg_r( status, G_ne )
      IF ( status /= 0 ) GO TO 900
      WRITE( out, "( ' * G_ne = ', I0 )" ) G_ne

!  compute its sparsity pattern

      icon = 0
      WRITE( out, "( ' CALL CUTEST_cisgrp for the objective function' )" )
      CALL CUTEST_cisgrp_r( status, n, icon, G_ne, l_g, G_var )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_G_sparsity_pattern( out, G_ne, l_g, G_var )

!  and its values

      WRITE( out, "( ' CALL CUTEST_cisgr for the objective function' )" )
      CALL CUTEST_cisgr_r( status, n, icon, X, G_ne, l_g, G_val, G_var )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_SG( out, G_ne, l_g, G_val, G_var )

!  compute the number of nonzeros in the sparse Jacobian

      WRITE( out, "( ' CALL CUTEST_cdimsj' )" )
      CALL CUTEST_cdimsj_r( status, J_ne )
      IF ( status /= 0 ) GO TO 900
      WRITE( out, "( ' * J_ne = ', I0 )" ) J_ne

      l_j = J_ne
      ALLOCATE( J_val( l_j ), J_fun( l_j ), J_var( l_j ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

!  compute the sparsity pattern of the Jacobian

      WRITE( out, "( ' Call CUTEST_csjp' )" )
      CALL CUTEST_csjp_r( status, J_ne, l_j, J_var, J_fun )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_J_sparsity_pattern( out, J_ne, l_j, J_fun, J_var )

!  compute the sparsity pattern of the Jacobian and objective gradient

      WRITE( out, "( ' Call CUTEST_csgrp' )" )
      CALL CUTEST_csgrp_r( status, n, J_ne, l_j, J_var, J_fun )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_J_sparsity_pattern( out, J_ne, l_j, J_fun, J_var )

!  compute the gradient and sparse Jacobian values

      grlagf = .TRUE.
      WRITE( out, "( ' CALL CUTEST_csgr with grlagf = .TRUE.' )" )
      CALL CUTEST_csgr_r( status, n, m, X, Y, grlagf,                          &
                          J_ne, l_j, J_val, J_var, J_fun )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      grlagf = .FALSE.
      WRITE( out, "( ' CALL CUTEST_csgr with grlagf = .FALSE.' )" )
      CALL CUTEST_csgr_r( status, n, m, X, Y, grlagf,                          &
                          J_ne, l_j, J_val, J_var, J_fun )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )

!  compute the constraint and dense Jacobian values

      grad = .TRUE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CUTEST_ccfg with grad = .TRUE. and ',              &
     &               'jtrans = .TRUE.' )" )
      CALL CUTEST_ccfg_r( status, n, m, X, C, jtrans,                          &
                          l_j2_1, l_j2_2, J2_val, grad )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_C( out, m, C )
      CALL WRITE_JT_dense( out, n, m, l_j2_1, l_j2_2, J2_val )
      grad = .TRUE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CUTEST_ccfg with grad = .TRUE. and ',              &
     &               'jtrans = .FALSE.' )" )
      CALL CUTEST_ccfg_r( status, n, m, X, C, jtrans,                          &
                          l_j2_1, l_j2_2, J2_val, grad )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_C( out, m, C )
      CALL WRITE_J_dense( out, n, m, l_j2_1, l_j2_2, J2_val )
      grad = .FALSE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CUTEST_ccfg with grad = .FALSE. and ',             &
     &               'jtrans = .TRUE.' )" )
      CALL CUTEST_ccfg_r( status, n, m, X, C, jtrans,                          &
                        l_j2_1, l_j2_2, J2_val, grad )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_C( out, m, C )
      grad = .FALSE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CUTEST_ccfg with grad = .FALSE. and ',             &
     &               'jtrans = .FALSE.' )" )
      CALL CUTEST_ccfg_r( status, n, m, X, C, jtrans,                          &
                          l_j2_1, l_j2_2, J2_val, grad )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_C( out, m, C )

!  compute the constraint and sparse Jacobian values

      grad = .TRUE.
      WRITE( out, "( ' CALL CUTEST_ccfsg with grad = .TRUE.' )" )
      CALL CUTEST_ccfsg_r( status, n, m, X, C,                                 &
                           J_ne, l_j, J_val, J_var, J_fun, grad )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_C( out, m, C )
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      grad = .FALSE.
      WRITE( out, "( ' CALL CUTEST_ccfsg with grad = .FALSE.' )" )
      CALL CUTEST_ccfsg_r( status, n, m, X, C,                                 &
                           J_ne, l_j, J_val, J_var, J_fun, grad )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_C( out, m, C )

!  compute the Lagrangian function and gradient values

      grad = .TRUE.
      WRITE( out, "( ' CALL CUTEST_clfg with grad = .TRUE.' )" )
      CALL CUTEST_clfg_r( status, n, m, X, Y, f, G, grad )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_f( out, f )
      CALL WRITE_G( out, n, G )
      grad = .FALSE.
      WRITE( out, "( ' CALL CUTEST_clfg with grad = .FALSE.' )" )
      CALL CUTEST_clfg_r( status, n, m, X, Y, f, G, grad )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_f( out, f )

!  compute an individual constraint and its dense gradient

      icon = 1
      grad = .FALSE.
      WRITE( out, "( ' CALL CUTEST_ccifg with grad = .FALSE.' )" )
      CALL CUTEST_ccifg_r( status, n, icon, X, ci, Ji, grad )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_CI( out, icon, ci )
      grad = .TRUE.
      WRITE( out, "( ' CALL CUTEST_ccifg with grad = .TRUE.' )" )
      CALL CUTEST_ccifg_r( status, n, icon, X, ci, Ji, grad )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_CI( out, icon, ci )
      CALL WRITE_JI( out, n, icon, Ji )

!  compute just its dense gradient

      WRITE( out, "( ' CALL CUTEST_cigr for a constraint' )" )
      CALL CUTEST_cigr_r( status, n, icon, X, Ji )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_JI( out, n, icon, Ji )

!  compute an individual constraint and its sparse gradient

      grad = .FALSE.
      WRITE( out, "( ' CALL CUTEST_ccifsg with grad = .FALSE.' )" )
      CALL CUTEST_ccifsg_r( status, n, icon, X, ci,                            &
                            Ji_ne, n, Ji, J_var, grad )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_CI( out, icon, ci )
      grad = .TRUE.
      WRITE( out, "( ' CALL CUTEST_ccifsg with grad = .TRUE.' )" )
      CALL CUTEST_ccifsg_r( status, n, icon, X, ci,                            &
                            Ji_ne, n, Ji, J_var, grad )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_CI( out, icon, ci )
      CALL WRITE_SJI( out, icon, Ji_ne, n, Ji, J_var )

!  compute the sparsity pattern of the gradient of an individual constraint

      WRITE( out, "( ' CALL CUTEST_cisgrp for a constraint' )" )
      CALL CUTEST_cisgrp_r( status, n, icon, G_ne, l_g, G_var )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_G_sparsity_pattern( out, G_ne, l_g, G_var )

!  and its values

      WRITE( out, "( ' CALL CUTEST_cisgr for a constraint' )" )
      CALL CUTEST_cisgr_r( status, n, icon, X, Ji_ne, n, Ji, J_var )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_SJI( out, icon, Ji_ne, n, Ji, J_var )

!  compute the dense Hessian value

      WRITE( out, "( ' CALL CUTEST_cdh' )" )
      CALL CUTEST_cdh_r( status, n, m, X, Y, l_h2_1, H2_val )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )

!  compute the dense Hessian value of the John function

      y0 = 2.0_rp_
      WRITE( out, "( ' CALL CUTEST_cdhj' )" )
      CALL CUTEST_cdhj_r( status, n, m, X, y0, Y, l_h2_1, H2_val )
      IF ( status /= 0 ) GO to 900
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )

!  compute the dense Hessian value without the objective function

      WRITE( out, "( ' CALL CUTEST_cdhc' )" )
      CALL CUTEST_cdhc_r( status, n, m, X, Y, l_h2_1, H2_val )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )

!  compute the dense Hessian value of the objective or a constraint

      iprob = 0
      WRITE( out, "( ' CALL CUTEST_cidh for objective' )" )
      CALL CUTEST_cidh_r( status, n, X, iprob, l_h2_1, H2_val )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )
      iprob = 1
      WRITE( out, "( ' CALL CUTEST_cidh for a constraint' )" )
      CALL CUTEST_cidh_r( status, n, X, iprob, l_h2_1, H2_val )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )

!  compute the gradient and dense Hessian values

      grlagf = .TRUE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CUTEST_cgrdh with grlagf = .TRUE. and ',           &
     &               'jtrans = .TRUE.' )" )
      CALL CUTEST_cgrdh_r( status, n, m, X, Y, grlagf, G, jtrans,              &
                           l_j2_1, l_j2_2, J2_val, l_h2_1, H2_val )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_G( out, n, G )
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )
      grlagf = .TRUE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CUTEST_cgrdh with grlagf = .TRUE. and ',           &
     &               'jtrans = .FALSE.' )")
      CALL CUTEST_cgrdh_r( status, n, m, X, Y, grlagf, G, jtrans,              &
                           l_j2_1, l_j2_2, J2_val, l_h2_1, H2_val )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_G( out, n, G )
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )
      grlagf = .FALSE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CUTEST_cgrdh with grlagf = .FALSE. and ',          &
     &               'jtrans = .TRUE.' )")
      CALL CUTEST_cgrdh_r( status, n, m, X, Y, grlagf, G, jtrans,              &
                           l_j2_1, l_j2_2, J2_val, l_h2_1, H2_val )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_G( out, n, G )
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )
      grlagf = .FALSE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CUTEST_cgrdh with grlagf = .FALSE. and ',          &
     &               'jtrans = .FALSE.')")
      CALL CUTEST_cgrdh_r( status, n, m, X, Y, grlagf, G, jtrans,              &
                           l_j2_1, l_j2_2, J2_val, l_h2_1, H2_val )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_G( out, n, G )
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )

!  compute the number of nonzeros in the sparse Hessian

      WRITE( out, "( ' CALL CUTEST_cdimsh' )" )
      CALL CUTEST_cdimsh_r( status, H_ne )
      IF ( status /= 0 ) GO TO 900
      WRITE( out, "( ' * H_ne = ', I0 )" ) H_ne

      l_h = H_ne
      ALLOCATE( H_val( l_h ), H_row( l_h ), H_col( l_h ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

!  compute the sparsity pattern of the Hessian

      WRITE( out, "( ' Call CUTEST_cshp' )" )
      CALL CUTEST_cshp_r( status, n, H_ne, l_h, H_row, H_col )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_H_sparsity_pattern( out, H_ne, l_h, H_row, H_col )

!  compute the sparse Hessian value

      WRITE( out, "( ' CALL CUTEST_csh' )" )
      CALL CUTEST_csh_r( status, n, m, X, Y,                                   &
                         H_ne, l_h, H_val, H_row, H_col )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )

!  compute the sparse Hessian value without the objective

      WRITE( out, "( ' CALL CUTEST_cshc' )" )
      CALL CUTEST_cshc_r( status, n, m, X, Y,                                  &
                          H_ne, l_h, H_val, H_row, H_col )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )

!  compute the sparse Hessian of the John function

      WRITE( out, "( ' CALL CUTEST_cshj' )" )
      CALL CUTEST_cshj_r( status, n, m, X, y0, Y,                              &
                          H_ne, l_h, H_val, H_row, H_col )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )

!  compute the sparse Hessian value of the objective or a constraint

      iprob = 0
      WRITE( out, "( ' CALL CUTEST_cish for objective' )" )
      CALL CUTEST_cish_r( status, n, X, iprob,                                 &
                          H_ne, l_h, H_val, H_row, H_col )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )
      iprob = 1
      WRITE( out, "( ' CALL CUTEST_cish for a constraint' )" )
      CALL CUTEST_cish_r( status, n, X, iprob,                                 &
                          H_ne, l_h, H_val, H_row, H_col )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )

!  compute the sparsity pattern of the gradients and Hessian

      WRITE( out, "( ' Call CUTEST_csgrshp' )" )
      CALL CUTEST_csgrshp_r( status, n, J_ne, l_j, J_var, J_fun,               &
                             H_ne, l_h, H_row, H_col )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_J_sparsity_pattern( out, J_ne, l_j, J_fun, J_var )
      CALL WRITE_H_sparsity_pattern( out, H_ne, l_h, H_row, H_col )

!  compute the gradient and sparse Hessian values

      grlagf = .TRUE.
      WRITE( out, "( ' CALL CUTEST_csgrsh with grlagf = .TRUE.' )" )
      CALL CUTEST_csgrsh_r( status, n, m, X, Y, grlagf, J_ne, l_j, J_val,      &
                            J_var, J_fun, H_ne, l_h, H_val, H_row, H_col )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )
      grlagf = .FALSE.
      WRITE( out, "( ' CALL CUTEST_csgrsh with grlagf = .FALSE.' )" )
      CALL CUTEST_csgrsh_r( status, n, m, X, Y, grlagf, J_ne, l_j, J_val,      &
                            J_var, J_fun, H_ne, l_h, H_val, H_row, H_col )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )

!  compute the number of nonzeros in the element Hessian

      WRITE( out, "( ' CALL CUTEST_cdimse' )" )
      CALL CUTEST_cdimse_r( status, HE_nel, HE_val_ne, HE_row_ne )
      IF ( status /= 0 ) GO TO 900
      WRITE( out, "( ' * H_nel = ', I0, ' HE_val_ne = ', I0,                   &
     &                 ' HE_row_ne = ', I0 )" ) HE_nel, HE_val_ne, HE_row_ne

      lhe_ptr = HE_nel + 1
      lhe_val = HE_val_ne
      lhe_row = HE_row_ne
      ALLOCATE( HE_row_ptr( lhe_ptr ), HE_val_ptr( lhe_ptr ),                  &
                HE_row( lhe_row ),  HE_val( lhe_val ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

!  compute the element Hessian value

      byrows = .FALSE.
      WRITE( out, "( ' CALL CUTEST_ceh with byrows = .FALSE.' )" )
      CALL CUTEST_ceh_r( status, n, m, X, Y, HE_nel, lhe_ptr, HE_row_ptr,      &
                         HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val, byrows )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )
      byrows = .TRUE.
      WRITE( out, "( ' CALL CUTEST_ceh with byrows = .TRUE.' )" )
      CALL CUTEST_ceh_r( status, n, m, X, Y, HE_nel, lhe_ptr, HE_row_ptr,      &
                         HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val, byrows )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )

!  compute the gradient and element Hessian values

      grlagf = .TRUE. ; byrows = .TRUE.
      WRITE( out, "( ' CALL CUTEST_csgreh with grlagf = .TRUE. and ',          &
     &               'byrows = .TRUE.')" )
      CALL CUTEST_csgreh_r( status, n, m, X, Y, grlagf, J_ne, l_j,             &
                          J_val, J_var, J_fun, HE_nel, lhe_ptr, HE_row_ptr,    &
                          HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val, byrows )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )
      grlagf = .TRUE. ; byrows = .FALSE.
      WRITE( out, "(' CALL CUTEST_csgreh with grlagf = .TRUE. and ',           &
     &               'byrows = .FALSE.')" )
      CALL CUTEST_csgreh_r( status, n, m, X, Y, grlagf, J_ne, l_j,             &
                          J_val, J_var, J_fun, HE_nel, lhe_ptr, HE_row_ptr,    &
                          HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val, byrows )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )
      grlagf = .FALSE. ; byrows = .TRUE.
      WRITE( out, "( ' CALL CUTEST_csgreh with grlagf = .FALSE. and ',         &
     &               'byrows = .TRUE.')")
      CALL CUTEST_csgreh_r( status, n, m, X, Y, grlagf, J_ne, l_j,             &
                          J_val, J_var, J_fun, HE_nel, lhe_ptr, HE_row_ptr,    &
                          HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val, byrows )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )
      grlagf = .FALSE. ; byrows = .FALSE.
      WRITE( out, "(' CALL CUTEST_csgreh with grlagf = .FALSE. and ',          &
     &               'byrows = .FALSE.')")
      CALL CUTEST_csgreh_r( status, n, m, X, Y, grlagf, J_ne, l_j,             &
                          J_val, J_var, J_fun, HE_nel, lhe_ptr, HE_row_ptr,    &
                          HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val, byrows )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )

!  compute a Hessian-vector product

      VECTOR( 1 ) = one ; VECTOR( 2 : n ) = zero
      goth = .FALSE.
      WRITE( out, "( ' Call CUTEST_chprod with goth = .FALSE.' )" )
      CALL CUTEST_chprod_r( status, n, m, goth, X, Y, VECTOR, RESULT )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_RESULT( out, n, VECTOR, RESULT )
      goth = .TRUE.
      WRITE( out, "( ' Call CUTEST_chprod with goth = .TRUE.' )" )
      CALL CUTEST_chprod_r( status, n, m, goth, X, Y, VECTOR, RESULT )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_RESULT( out, n, VECTOR, RESULT )

!  compute a sparse Hessian-vector product

      nnz_vector = 1 ; INDEX_nz_vector( nnz_vector ) = 1
      goth = .FALSE.
      WRITE( out, "( ' Call CUTEST_cshprod with goth = .FALSE.' )" )
      CALL CUTEST_cshprod_r( status, n, m, goth, X, Y,                         &
                             nnz_vector, INDEX_nz_vector, VECTOR,              &
                             nnz_result, INDEX_nz_result, RESULT )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_SRESULT( out, n, nnz_vector, INDEX_nz_vector, VECTOR,         &
                          nnz_result, INDEX_nz_result, RESULT )

      goth = .TRUE.
      WRITE( out, "( ' Call CUTEST_cshprod with goth = .TRUE.' )" )
      CALL CUTEST_cshprod_r( status, n, m, goth, X, Y,                         &
                             nnz_vector, INDEX_nz_vector, VECTOR,              &
                             nnz_result, INDEX_nz_result, RESULT )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_SRESULT( out, n, nnz_vector, INDEX_nz_vector, VECTOR,         &
                          nnz_result, INDEX_nz_result, RESULT )

!  compute a Hessian-of-the-John-function-vector product

      goth = .FALSE.
      WRITE( out, "( ' Call CUTEST_chjprod with goth = .FALSE.' )" )
      CALL CUTEST_chjprod_r( status, n, m, goth, X, y0, Y, VECTOR, RESULT )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_RESULT( out, n, VECTOR, RESULT )
      goth = .TRUE.
      WRITE( out, "( ' Call CUTEST_chpjrod with goth = .TRUE.' )" )
      CALL CUTEST_chjprod_r( status, n, m, goth, X, y0, Y, VECTOR, RESULT )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_RESULT( out, n, VECTOR, RESULT )

!  compute a Hessian-vector product ignoring the objective

      goth = .FALSE.
      WRITE( out, "( ' Call CUTEST_chcprod with goth = .FALSE.' )" )
      CALL CUTEST_chcprod_r( status, n, m, goth, X, Y, VECTOR, RESULT )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_RESULT( out, n, VECTOR, RESULT )
      goth = .TRUE.
      WRITE( out, "( ' Call CUTEST_chcprod with goth = .TRUE.' )" )
      CALL CUTEST_chcprod_r( status, n, m, goth, X, Y, VECTOR, RESULT )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_RESULT( out, n, VECTOR, RESULT )

!  compute a sparse Hessian-vector product ignoring the objective

      goth = .FALSE.
      WRITE( out, "( ' Call CUTEST_cshprod with goth = .FALSE.' )" )
      CALL CUTEST_cshcprod_r( status, n, m, goth, X, Y,                        &
                           nnz_vector, INDEX_nz_vector, VECTOR,                &
                           nnz_result, INDEX_nz_result, RESULT )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_SRESULT( out, n, nnz_vector, INDEX_nz_vector, VECTOR,         &
                          nnz_result, INDEX_nz_result, RESULT )

      goth = .TRUE.
      WRITE( out, "( ' Call CUTEST_cshprod with goth = .TRUE.' )" )
      CALL CUTEST_cshcprod_r( status, n, m, goth, X, Y,                        &
                              nnz_vector, INDEX_nz_vector, VECTOR,             &
                              nnz_result, INDEX_nz_result, RESULT )
      IF ( status /= 0 ) GO TO 900
      CALL WRITE_SRESULT( out, n, nnz_vector, INDEX_nz_vector, VECTOR,         &
                          nnz_result, INDEX_nz_result, RESULT )

!  compute a Jacobian-vector product

      VECTOR( 1 ) = one ; VECTOR( 2 : MAX( n, m ) ) = zero
      gotj = .FALSE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CJPROD with gotj = .FALSE. and jtrans = .FALSE.')" )
      CALL CUTEST_cjprod_r( status, n, m, gotj, jtrans, X, VECTOR, n,          &
                            RESULT, m )
      CALL WRITE_RESULT2( out, n, VECTOR, m, RESULT )
      gotj = .TRUE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CJPROD with gotj = .TRUE. and jtrans = .FALSE.' )" )
      CALL CUTEST_cjprod_r( status, n, m, gotj, jtrans, X, VECTOR, n,          &
                            RESULT, m )
      CALL WRITE_RESULT2( out, n, VECTOR, m, RESULT )
      gotj = .FALSE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CJPROD with gotj = .FALSE. and jtrans = .TRUE.')" )
      CALL CUTEST_cjprod_r( status, n, m, gotj, jtrans, X, VECTOR, m,          &
                            RESULT, n )
      CALL WRITE_RESULT2( out, m, VECTOR, n, RESULT )
      gotj = .TRUE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CJPROD with gotj = .TRUE. and jtrans = .TRUE.' )" )
      CALL CUTEST_cjprod_r( status, n, m, gotj, jtrans, X, VECTOR, m,          &
                            RESULT, n )
      CALL WRITE_RESULT2( out, m, VECTOR, n, RESULT )

!  compute a sparse Jacobian-vector product

      gotj = .FALSE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CSJPROD with gotj = .FALSE. and jtrans = .FALSE.')")
      CALL CUTEST_csjprod_r( status, n, m, gotj, jtrans, X,                    &
                             nnz_vector, INDEX_nz_vector, VECTOR, n,           &
                             nnz_result, INDEX_nz_result, RESULT, m )
      CALL WRITE_SRESULT2( out, nnz_vector, INDEX_nz_vector, VECTOR, n,        &
                           nnz_result, INDEX_nz_result, RESULT, m )
      gotj = .TRUE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CSJPROD with gotj = .TRUE. and jtrans = .FALSE.')" )
      CALL CUTEST_csjprod_r( status, n, m, gotj, jtrans, X,                    &
                             nnz_vector, INDEX_nz_vector, VECTOR, n,           &
                             nnz_result, INDEX_nz_result, RESULT, m )
      CALL WRITE_SRESULT2( out, nnz_vector, INDEX_nz_vector, VECTOR, n,        &
                           nnz_result, INDEX_nz_result, RESULT, m )
      gotj = .FALSE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CSJPROD with gotj = .FALSE. and jtrans = .TRUE.')" )
      CALL CUTEST_csjprod_r( status, n, m, gotj, jtrans, X,                    &
                             nnz_vector, INDEX_nz_vector, VECTOR, m,           &
                             nnz_result, INDEX_nz_result, RESULT, n )
      CALL WRITE_SRESULT2( out, nnz_vector, INDEX_nz_vector, VECTOR, m,        &
                           nnz_result, INDEX_nz_result, RESULT, n )
      gotj = .TRUE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CSJPROD with gotj = .TRUE. and jtrans = .TRUE.' )" )
      CALL CUTEST_csjprod_r( status, n, m, gotj, jtrans, X,                    &
                             nnz_vector, INDEX_nz_vector, VECTOR, m,           &
                             nnz_result, INDEX_nz_result, RESULT, n )
      CALL WRITE_SRESULT2( out, nnz_vector, INDEX_nz_vector, VECTOR, m,        &
                           nnz_result, INDEX_nz_result, RESULT, n )

!  compute the number of nonzeros when forming the products of the constraint
!  Hessians with a vector

      WRITE( out, "( ' CALL CUTEST_cdimchp' )" )
      CALL CUTEST_cdimchp_r( status, CHP_ne )
      IF ( status /= 0 ) GO TO 900
      WRITE( out, "( ' * CHP_ne = ', I0 )" ) CHP_ne

      l_chp = CHP_ne
      ALLOCATE( CHP_val( l_chp ), CHP_ind( l_chp ), CHP_ptr( m + 1 ),          &
                stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

!  compute the sparsity pattern needed for the matrix-vector products between
!  each constraint Hessian and a vector

      WRITE( out, "( ' Call CUTEST_cchprodsp' )" )
      CALL CUTEST_cchprodsp_r( status, m, l_chp, CHP_ind, CHP_ptr )
      CALL WRITE_CHP_sparsity( out, m, l_chp, CHP_ind, CHP_ptr )

!  compute the matrix-vector products between each constraint Hessian and a
!  vector

      goth = .FALSE.
      WRITE( out, "( ' Call CUTEST_cchprods with goth = .FALSE.' )" )
       CALL CUTEST_cchprods_r( status, n, m, goth, X, VECTOR, l_chp,           &
                               CHP_val, CHP_ind, CHP_ptr )
       CALL WRITE_CHP( out, m, l_chp, CHP_val, CHP_ind, CHP_ptr )

      goth = .TRUE.
      WRITE( out, "( ' Call CUTEST_cchprods with goth = .TRUE.' )" )
       CALL CUTEST_cchprods_r( status, n, m, goth, X, VECTOR, l_chp,           &
                               CHP_val, CHP_ind, CHP_ptr )
       CALL WRITE_CHP( out, m, l_chp, CHP_val, CHP_ind, CHP_ptr )

!  compute the number of nonzeros when forming the products of the objective
!  Hessians with a vector

      WRITE( out, "( ' CALL CUTEST_cdimohp' )" )
      CALL CUTEST_cdimohp_r( status, OHP_ne )
      IF ( status /= 0 ) GO TO 900
      WRITE( out, "( ' * OHP_ne = ', I0 )" ) OHP_ne

      l_ohp = OHP_ne
      ALLOCATE( OHP_val( l_ohp ), OHP_ind( l_ohp ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

!  compute the sparsity pattern needed for the matrix-vector product between
!  the objective Hessian and a vector

      WRITE( out, "( ' Call CUTEST_cohprodsp' )" )
      CALL CUTEST_cohprodsp_r( status, OHP_ne, l_ohp, OHP_ind )
      CALL WRITE_OHP_sparsity( out, OHP_ne, l_ohp, OHP_ind )

!  compute the matrix-vector product between the objective Hessian and a
!  vector

      goth = .FALSE.
      WRITE( out, "( ' Call CUTEST_cohprods with goth = .FALSE.' )" )
      CALL CUTEST_cohprods_r( status, n, goth, X, VECTOR,                      &
                              OHP_ne, l_ohp, OHP_val, OHP_ind )
      CALL WRITE_OHP( out, OHP_ne, l_ohp, OHP_val, OHP_ind )

      goth = .TRUE.
      WRITE( out, "( ' Call CUTEST_cohprods with goth = .TRUE.' )" )
      CALL CUTEST_cohprods_r( status, n, goth, X, VECTOR,                      &
                              OHP_ne, l_ohp, OHP_val, OHP_ind )
      CALL WRITE_OHP( out, OHP_ne, l_ohp, OHP_val, OHP_ind )

!  calls and time report

      WRITE( out, "( ' CALL CUTEST_creport' )" )
      CALL CUTEST_creport_r( status, CALLS, CPU )
      WRITE( out, "( ' CALLS(1-7) =', 7( 1X, I0 ) )" ) INT( CALLS( 1 : 7 ) )
      WRITE( out, "( ' CPU(1-4) =', 4F7.2 )" ) CPU( 1 : 4 )

!  terminal exit

      WRITE( out, "( ' Call CUTEST_cterminate' )" )
      CALL CUTEST_cterminate_r( status )
      IF ( status /= 0 ) GO TO 900

!  one more setup ...

      WRITE( out, "( ' CALL CUTEST_csetup ' )" )
      CALL CUTEST_csetup_r( status, input, out, buffer, n, m, X, X_l, X_u,     &
                             Y, C_l, C_u, EQUATION, LINEAR, 1_ip_, 1_ip_, 1_ip_ )
      IF ( status /= 0 ) GO TO 900

!  ... and terminal exit

      WRITE( out, "( ' Call CUTEST_cterminate' )" )
      CALL CUTEST_cterminate_r( status )
      IF ( status /= 0 ) GO TO 900

      DEALLOCATE( X_type, H_row, H_col, HE_row, HE_row_ptr, HE_val_ptr, X,     &
                  X_l, X_u, G, Ji, Y, C_l, C_u, C, H_val, HE_val, H2_val,      &
                  J_val, J_var, J_fun, J2_val, VECTOR, RESULT, g_val, g_var,   &
                  X_names, C_names, EQUATION, LINEAR, INDEX_nz_vector,         &
                  INDEX_nz_result, CHP_val, CHP_ind, CHP_ptr,                  &
                  OHP_val, OHP_ind, stat = alloc_stat )
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

      SUBROUTINE WRITE_Y( out, m, Y, C_l, C_u, EQUATION, LINEAR )
      INTEGER ( KIND = ip_ ) :: m, out
      REAL ( KIND = rp_ ), DIMENSION( m ) :: Y, C_l, C_u
      LOGICAL, DIMENSION( m ) :: EQUATION, LINEAR
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' *       i      C_l         C_u          Y   ',          &
    &   '      EQUATION   LINEAR' )" )
      DO i = 1, m
        WRITE( out, "( ' * ', I7, 3ES12.4, 2L10 )" ) i, C_l( i ), C_u( i ),    &
          Y( i ), EQUATION( i ), LINEAR( i )
      END DO
      END SUBROUTINE WRITE_Y

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

      SUBROUTINE WRITE_C( out, m, C )
      INTEGER ( KIND = ip_ ) :: m, out
      REAL ( KIND = rp_ ), DIMENSION( m ) :: C
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' *       i       C' )" )
      DO i = 1, m
        WRITE( out, "( ' * ', I7, ES12.4 )" ) i, C( i )
      END DO
      END SUBROUTINE WRITE_C

      SUBROUTINE WRITE_CI( out, icon, ci )
      INTEGER ( KIND = ip_ ) :: icon, out
      REAL ( KIND = rp_ ) :: ci
      WRITE( out, "( ' * c(', I0, ') = ', ES12.4 )" ) icon, ci
      END SUBROUTINE WRITE_CI

      SUBROUTINE WRITE_G( out, n, G )
      INTEGER ( KIND = ip_ ) :: n, out
      REAL ( KIND = rp_ ), DIMENSION( n ) :: G
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' *       i       G' )" )
      DO i = 1, n
        WRITE( out, "( ' * ', I7, ES12.4 )" ) i, G( i )
      END DO
      END SUBROUTINE WRITE_G

      SUBROUTINE WRITE_SG( out, G_ne, lG, G, J_var )
      INTEGER ( KIND = ip_ ) :: G_ne, lG, out
      INTEGER ( KIND = ip_ ), DIMENSION( lG ) :: J_var
      REAL ( KIND = rp_ ), DIMENSION( lG ) :: G
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' *       i      G' )" )
      DO i = 1, G_ne
        WRITE( out, "( ' * ', I7, ES12.4 )" ) J_var( i ), G( i )
      END DO
      END SUBROUTINE WRITE_SG

      SUBROUTINE WRITE_JI( out, n, icon, Ji )
      INTEGER ( KIND = ip_ ) :: n, icon, out
      REAL ( KIND = rp_ ), DIMENSION( n ) :: Ji
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' *       i   J(', I0, ')' )" ) icon
      DO i = 1, n
        WRITE( out, "( ' * ', I7, ES12.4 )" ) i, Ji( i )
      END DO
      END SUBROUTINE WRITE_JI

      SUBROUTINE WRITE_SJI( out, icon, Ji_ne, lji, Ji, J_var )
      INTEGER ( KIND = ip_ ) :: icon, Ji_ne, lji, out
      INTEGER ( KIND = ip_ ), DIMENSION( lji ) :: J_var
      REAL ( KIND = rp_ ), DIMENSION( lji ) :: Ji
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' *       i   J(', I0, ')' )" ) icon
      DO i = 1, Ji_ne
        WRITE( out, "( ' * ', I7, ES12.4 )" ) J_var( i ), Ji( i )
      END DO
      END SUBROUTINE WRITE_SJI

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

      SUBROUTINE WRITE_J_dense( out, n, m, l_j2_1, l_j2_2, J2_val )
      INTEGER ( KIND = ip_ ) :: n, m, l_J2_1, l_j2_2, out
      REAL ( KIND = rp_ ), DIMENSION( l_j2_1, l_j2_2 ) :: J2_val
      INTEGER ( KIND = ip_ ) :: i, j
      WRITE( out, "( ' * J(dense)' )" )
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
      END SUBROUTINE WRITE_J_dense

      SUBROUTINE WRITE_JT_dense( out, n, m, l_j2_1, l_j2_2, J2_val )
      INTEGER ( KIND = ip_ ) :: n, m, l_J2_1, l_j2_2, out
      REAL ( KIND = rp_ ), DIMENSION( l_j2_1, l_j2_2 ) :: J2_val
      INTEGER ( KIND = ip_ ) :: i, j
      WRITE( out, "( ' * J(transpose)(dense)' )" )
      DO j = 1, m, 4
        IF ( j + 3 <= m ) THEN
          WRITE( out, "( ' *       i   j', I8, 3I12 )" ) j, j + 1, j + 2, j + 3
        ELSE IF ( j + 2 <= m ) THEN
          WRITE( out, "( ' *       i   j', I8, 2I12 )" ) j, j + 1, j + 2
        ELSE IF ( j + 1 <= m ) THEN
          WRITE( out, "( ' *       i   j', I8, I12 )" ) j, j + 1
        ELSE
          WRITE( out, "( ' *       i   j', I8 )" ) j
        END IF
        DO i = 1, n
          IF ( j + 3 <= m ) THEN
            WRITE( out, "( ' * ', I7,  4X, 4ES12.4 )" )                        &
              i, J2_val( i, j ), J2_val( i, j + 1 ),                           &
              J2_val( i, j + 2 ), J2_val( i, j + 3 )
          ELSE IF ( j + 2 <= m ) THEN
            WRITE( out, "( ' * ',  I7, 4X, 3ES12.4 )" )                        &
              i, J2_val( i, j ), J2_val( i, j + 1 ), J2_val( i, j + 2 )
          ELSE IF ( j + 1 <= m ) THEN
            WRITE( out, "( ' * ',  I7, 4X, 2ES12.4 )" )                        &
              i, J2_val( i, j ), J2_val( i, j + 1 )
          ELSE
            WRITE( out, "( ' * ',  I7, 4X, ES12.4 )" ) i, J2_val( i, j )
          END IF
        END DO
      END DO
      END SUBROUTINE WRITE_JT_dense

      SUBROUTINE WRITE_G_sparsity_pattern( out, G_ne, l_g, G_ind )
      INTEGER ( KIND = ip_ ) :: l_g, G_ne, out
      INTEGER ( KIND = ip_ ), DIMENSION( l_g ) :: G_ind
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' * G(sparse)' )" )
      WRITE( out, "( ' * ', 8( '    ind' ) )" )
      DO i = 1, G_ne, 8
        IF ( i + 7 <= G_ne ) THEN
          WRITE( out, "( ' * ',  8I7 )" ) G_ind(  i: i + 7 )
        ELSE IF ( i + 6 <= G_ne ) THEN
          WRITE( out, "( ' * ',  7I7 )" ) G_ind(  i: i + 6 )
        ELSE IF ( i + 5 <= G_ne ) THEN
          WRITE( out, "( ' * ',  6I7 )" ) G_ind(  i: i + 5 )
        ELSE IF ( i + 4 <= G_ne ) THEN
          WRITE( out, "( ' * ',  5I7 )" ) G_ind(  i: i + 4 )
        ELSE IF ( i + 3 <= G_ne ) THEN
          WRITE( out, "( ' * ',  4I7 )" ) G_ind(  i: i + 3 )
        ELSE IF ( i + 2 <= G_ne ) THEN
          WRITE( out, "( ' * ',  3I7 )" ) G_ind( i : i + 2 )
        ELSE IF ( i + 1 <= G_ne ) THEN
          WRITE( out, "( ' * ',  2I7 )" ) G_ind( i : i + 1 )
        ELSE
          WRITE( out, "( ' * ',  I7 )" ) G_ind( i )
        END IF
      END DO
      END SUBROUTINE WRITE_G_sparsity_pattern

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

      SUBROUTINE WRITE_J_sparsity_pattern( out, J_ne, l_j, J_fun, J_var )
      INTEGER ( KIND = ip_ ) :: l_j, J_ne, out
      INTEGER ( KIND = ip_ ), DIMENSION( l_j ) :: J_fun, J_var
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' * J(sparse)' )" )
      WRITE( out, "( ' * ', 2( '    fun    var' ) )" )
      DO i = 1, J_ne, 2
        IF ( i + 1 <= J_ne ) THEN
          WRITE( out, "( ' * ',  2( 2I7 ) )" )                                 &
            J_fun( i ), J_var( i ), J_fun( i + 1 ), J_var( i + 1 )
        ELSE
          WRITE( out, "( ' * ',  2( 2I7 ) )" ) J_fun( i ), J_var( i )
        END IF
      END DO
      END SUBROUTINE WRITE_J_sparsity_pattern

      SUBROUTINE WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      INTEGER ( KIND = ip_ ) :: l_j, J_ne, out
      INTEGER ( KIND = ip_ ), DIMENSION( l_j ) :: J_fun, J_var
      REAL ( KIND = rp_ ), DIMENSION( l_j ) :: J_val
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' * J(sparse)' )" )
      WRITE( out, "( ' * ', 2( '    fun    var     val    ' ) )" )
      DO i = 1, J_ne, 2
        IF ( i + 1 <= J_ne ) THEN
          WRITE( out, "( ' * ',  2( 2I7, ES12.4 ) )" )                         &
            J_fun( i ), J_var( i ), J_val( i ),                                &
            J_fun( i + 1 ), J_var( i + 1 ), J_val( i + 1 )
        ELSE
          WRITE( out, "( ' * ',  2( 2I7, ES12.4 ) )" )                         &
            J_fun( i ), J_var( i ), J_val( i )
        END IF
      END DO
      END SUBROUTINE WRITE_J_sparse

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
          WRITE( out, "( ' * no indices in element ', I0 )" ) i
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

      SUBROUTINE WRITE_RESULT2( out, len_vector, VECTOR, len_result, RESULT )
      INTEGER ( KIND = ip_ ) :: len_vector, len_result, out
      REAL ( KIND = rp_ ), DIMENSION( len_vector ) :: VECTOR
      REAL ( KIND = rp_ ), DIMENSION( len_result ) :: RESULT
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' *       i    VECTOR     RESULT' )" )
      DO i = 1, MIN( len_vector, len_result )
        WRITE( out, "( ' * ', I7, 2ES12.4 )" ) i, VECTOR( i ), RESULT( i )
      END DO
      IF ( len_vector > len_result ) THEN
        DO i = len_result + 1, len_vector
          WRITE( out, "( ' * ', I7, ES12.4, '     -' )" ) i, VECTOR( i )
        END DO
      ELSE IF ( len_vector < len_result ) THEN
        DO i = len_vector + 1, len_result
          WRITE( out, "( ' * ', I7, '     -      ', ES12.4 )" ) i, RESULT( i )
        END DO
      END IF
      END SUBROUTINE WRITE_RESULT2

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

      SUBROUTINE WRITE_SRESULT2( out, nnz_vector, INDEX_nz_vector,             &
                                 VECTOR, len_vector,                           &
                                 nnz_result, INDEX_nz_result,                  &
                                 RESULT, len_result )
      INTEGER ( KIND = ip_ ) :: len_vector, len_result, out
      INTEGER ( KIND = ip_ ) :: nnz_vector,  nnz_result
      INTEGER ( KIND = ip_ ), DIMENSION( nnz_vector ) :: INDEX_nz_vector
      INTEGER ( KIND = ip_ ), DIMENSION( nnz_result ) :: INDEX_nz_result
      REAL ( KIND = rp_ ), DIMENSION( len_vector ) :: VECTOR
      REAL ( KIND = rp_ ), DIMENSION( len_result ) :: RESULT
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
      END SUBROUTINE WRITE_SRESULT2

      SUBROUTINE WRITE_CHP_sparsity( out, m, l_chp, CHP_ind, CHP_ptr )
      INTEGER ( KIND = ip_ ) :: m, l_chp, out
      INTEGER ( KIND = ip_ ), DIMENSION( m + 1 ) :: CHP_ptr
      INTEGER ( KIND = ip_ ), DIMENSION( l_chp ) :: CHP_ind
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' * CH(product_sparsity)' )" )
      DO i = 1, m
        IF (  CHP_ptr( i + 1 ) > CHP_ptr( i ) ) THEN
          WRITE( out, "( ' * constraint Hessian ', I0 )" ) i
          WRITE( out, "( ' * product indices ', 5I12, : , /,                   &
         &  ( ' *', 17X, 5I12, : ) )" )                                        &
            CHP_ind( CHP_ptr( i ) : CHP_ptr( i + 1 ) - 1 )
        ELSE
          WRITE( out, "( ' * no Hessian indices for constraint ', I0 )" ) i
        END IF
      END DO
      END SUBROUTINE WRITE_CHP_sparsity

      SUBROUTINE WRITE_CHP( out, m, l_chp, CHP_val, CHP_ind, CHP_ptr )
      INTEGER ( KIND = ip_ ) :: m, l_chp, out
      INTEGER ( KIND = ip_ ), DIMENSION( m + 1 ) :: CHP_ptr
      INTEGER ( KIND = ip_ ), DIMENSION( l_chp ) :: CHP_ind
      REAL ( KIND = rp_ ), DIMENSION( l_chp ) :: CHP_val
      INTEGER ( KIND = ip_ ) :: i
      WRITE( out, "( ' * CH(product)' )" )
      DO i = 1, m
        IF (  CHP_ptr( i + 1 ) > CHP_ptr( i ) ) THEN
          WRITE( out, "( ' * constraint Hessian ', I0 )" ) i
          WRITE( out, "( ' * product indices ', 5I12, : , /,                   &
         &  ( ' *', 17X, 5I12, : ) )" )                                        &
            CHP_ind( CHP_ptr( i ) : CHP_ptr( i + 1 ) - 1 )
          WRITE( out, "( ' * product values  ', 5ES12.4, : , /,                &
         &  ( ' *', 17X, 5ES12.4, : ) )" )                                     &
            CHP_val( CHP_ptr( i ) : CHP_ptr( i + 1 ) - 1 )
        ELSE
          WRITE( out, "( ' * no Hessian indices for constraint ', I0 )" ) i
        END IF
      END DO
      END SUBROUTINE WRITE_CHP

      SUBROUTINE WRITE_OHP_sparsity( out, nnzohp, l_ohp, OHP_ind )
      INTEGER ( KIND = ip_ ) :: nnzohp, l_ohp, out
      INTEGER ( KIND = ip_ ), DIMENSION( l_ohp ) :: OHP_ind
      WRITE( out, "( ' * OH(product sparsity)' )" )
      WRITE( out, "( ' * objective Hessian' )" )
      WRITE( out, "( ' * product indices ', 5I12, : , /,                       &
     &  ( ' *', 17X, 5I12, : ) )" ) OHP_ind( : nnzohp )
      END SUBROUTINE WRITE_OHP_sparsity

      SUBROUTINE WRITE_OHP( out, nnzohp, l_ohp, OHP_val, OHP_ind )
      INTEGER ( KIND = ip_ ) :: nnzohp, l_ohp, out
      INTEGER ( KIND = ip_ ), DIMENSION( l_ohp ) :: OHP_ind
      REAL ( KIND = rp_ ), DIMENSION( l_ohp ) :: OHP_val
      WRITE( out, "( ' * OH(product)' )" )
      WRITE( out, "( ' * objective Hessian' )" )
      WRITE( out, "( ' * product indices ', 5I12, : , /,                       &
     &  ( ' *', 17X, 5I12, : ) )" ) OHP_ind( : nnzohp )
      WRITE( out, "( ' * product values  ', 5ES12.4, : , /,                    &
     &  ( ' *', 17X, 5ES12.4, : ) )" ) OHP_val( : nnzohp )
      END SUBROUTINE WRITE_OHP

    END PROGRAM CUTEST_test_constrained_tools

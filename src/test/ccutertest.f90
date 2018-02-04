! THIS VERSION: CUTEST 1.0 - 17/11/2012 AT 17:00 GMT.

!-*- C U T E S T  t e s t _ c o n s t r a i n e d _ t o o l s  P R O G R A M -*-

    PROGRAM CUTEST_test_constrained_tools

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released November 2012

!     USE CUTEST

!--------------------
!   P r e c i s i o n
!--------------------

      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!----------------------
!   P a r a m e t e r s
!----------------------

      INTEGER, PARAMETER :: input = 55
      INTEGER, PARAMETER :: out = 6
      REAL ( KIND = wp ), PARAMETER :: one = 1.0_wp

!--------------------------------
!   L o c a l   V a r i a b l e s
!--------------------------------

      INTEGER :: n, m, H_ne, HE_nel, HE_val_ne, HE_row_ne, J_ne, Ji_ne
      INTEGER :: l_h2_1, l_h, lhe_ptr, lhe_val, lhe_row, alloc_stat
      REAL ( KIND = wp ) :: f, ci
      LOGICAL :: grad, byrows, goth, gotj, efirst, lfirst, nvfrst
      LOGICAL :: grlagf, jtrans
      CHARACTER ( len = 10 ) ::  p_name
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: H_row, H_col, X_type
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: HE_row, HE_row_ptr, HE_val_ptr
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: J_var, J_fun
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: X, X_l, X_u, G, Ji
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: Y, C_l, C_u, C, J_val
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: H_val, HE_val
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: VECTOR, RESULT
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : , : ) :: H2_val
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : , : ) :: J2_val
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATION, LINEAR
      CHARACTER ( len = 10 ), ALLOCATABLE, DIMENSION( : ) :: X_names, C_names
      REAL ( KIND = wp ) :: CPU( 2 ), CALLS( 7 )

!  open the problem data file

      OPEN ( input, FILE = 'c_OUTSDIF.d', FORM = 'FORMATTED', STATUS = 'OLD' )

!  allocate basic arrays

      WRITE( out, "( ' CALL CDIMEN ' )" )
      CALL CDIMEN( input, n, m )
      WRITE( out, "( ' * n = ', I0, ', m = ', I0 )" ) n, m
      l_h2_1 = n
      ALLOCATE( X( n ), X_l( n ), X_u( n ), G( n ), Ji( n ),                   &
                X_names( n ), X_type( n ), stat = alloc_stat )
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

!  set up SIF data

      efirst = .TRUE. ; lfirst = .TRUE. ; nvfrst = .TRUE.
      WRITE( out, "( ' CALL CSETUP ' )" )
      CALL CSETUP( input, out, n, m, X, X_l, X_u, n,                           &
                   EQUATION, LINEAR, Y, C_l, C_u, m, efirst, lfirst, nvfrst )
      CALL WRITE_X( out, n, X, X_l, X_u )
      CALL WRITE_Y( out, m, Y, C_l, C_u, EQUATION, LINEAR )

X = (/ 1.1_wp, 2.2_wp, 3.3_wp, 4.4_wp /)

!  obtain variable and problem names

      WRITE( out, "( ' CALL CNAMES' )" )
      CALL CNAMES( n, m, p_name, X_names, C_names )
      CALL WRITE_p_name( out, p_name )
      CALL WRITE_X_names( out, n, X_names )
      CALL WRITE_C_names( out, m, C_names )

!  obtain constraint names

      WRITE( out, "( ' Call CONNAMES' )" )
      CALL CONNAMES( m, C_names )
      CALL WRITE_C_names( out, m, C_names )

!  obtain variable types

      WRITE( out, "( ' CALL CVARTY' )" )
      CALL CVARTY( n, X_type )
      CALL WRITE_X_type( out, n, X_type )

!  compute the objective function value

      WRITE( out, "( ' CALL CFN' )" )
      CALL CFN( n, m, X, f, m, C )
      CALL WRITE_f( out, f )
      CALL WRITE_C( out, m, C )

!  compute the gradient and dense Jacobian values

      grlagf = .TRUE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CGR with grlagf = .TRUE. and jtrans = .TRUE.' )" )
      CALL CGR( n, m, X, grlagf, m, Y, G, jtrans, l_j2_1, l_j2_2, J2_val )
      CALL WRITE_G( out, n, G )
      CALL WRITE_JT_dense( out, n, m, l_j2_1, l_j2_2, J2_val )
      grlagf = .TRUE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CGR with grlagf = .TRUE. and jtrans = .FALSE.' )" )
      CALL CGR( n, m, X, grlagf, m, Y, G, jtrans, l_j2_1, l_j2_2, J2_val )
      CALL WRITE_G( out, n, G )
      CALL WRITE_J_dense( out, n, m, l_j2_1, l_j2_2, J2_val )
      grlagf = .FALSE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CGR with grlagf = .FALSE. and jtrans = .TRUE.' )" )
      CALL CGR( n, m, X, grlagf, m, Y, G, jtrans, l_j2_1, l_j2_2, J2_val )
      CALL WRITE_G( out, n, G )
      CALL WRITE_JT_dense( out, n, m, l_j2_1, l_j2_2, J2_val )
      grlagf = .FALSE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CGR with grlagf = .FALSE. and jtrans = .FALSE.' )" )
      CALL CGR( n, m, X, grlagf, m, Y, G, jtrans, l_j2_1, l_j2_2, J2_val )
      CALL WRITE_G( out, n, G )
      CALL WRITE_J_dense( out, n, m, l_j2_1, l_j2_2, J2_val )

!  compute the objective function and gradient values

      grad = .TRUE.
      WRITE( out, "( ' CALL COFG with grad = .TRUE.' )" )
      CALL COFG( n, X, f, G, grad )
      CALL WRITE_f( out, f )
      CALL WRITE_G( out, n, G )
      grad = .FALSE.
      WRITE( out, "( ' CALL COFG with grad = .FALSE.' )" )
      CALL COFG( n, X, f, G, grad )
      CALL WRITE_f( out, f )

!  compute the number of nonzeros in the sparse Jacobian

      WRITE( out, "( ' CALL CDIMSJ' )" )
      CALL CDIMSJ( J_ne )
      WRITE( out, "( ' * J_ne = ', I0 )" ) J_ne

      l_j = J_ne
      ALLOCATE( J_val( l_j ), J_fun( l_j ), J_var( l_j ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

!  compute the gradient and sparse Jacobian values

      grlagf = .TRUE.
      WRITE( out, "( ' CALL CSGR with grlagf = .TRUE.' )" )
      CALL CSGR( n, m, grlagf, m, Y, X, J_ne, l_j, J_val, J_var, J_fun )
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      grlagf = .FALSE.
      WRITE( out, "( ' CALL CSGR with grlagf = .FALSE.' )" )
      CALL CSGR( n, m, grlagf, m, Y, X, J_ne, l_j, J_val, J_var, J_fun )
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )

!  compute the constraint and dense Jacobian values

      grad = .TRUE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CCFG with grad = .TRUE. and jtrans = .TRUE.' )" )
      CALL CCFG( n, m, X, m, C, jtrans, l_j2_1, l_j2_2, J2_val, grad )
      CALL WRITE_C( out, m, C )
      CALL WRITE_JT_dense( out, n, m, l_j2_1, l_j2_2, J2_val )
      grad = .TRUE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CCFG with grad = .TRUE. and jtrans = .FALSE.' )" )
      CALL CCFG( n, m, X, m, C, jtrans, l_j2_1, l_j2_2, J2_val, grad )
      CALL WRITE_C( out, m, C )
      CALL WRITE_J_dense( out, n, m, l_j2_1, l_j2_2, J2_val )
      grad = .FALSE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CCFG with grad = .FALSE. and jtrans = .TRUE.' )" )
      CALL CCFG( n, m, X, m, C, jtrans, l_j2_1, l_j2_2, J2_val, grad )
      CALL WRITE_C( out, m, C )
      grad = .FALSE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CCFG with grad = .FALSE. and jtrans = .FALSE.' )" )
      CALL CCFG( n, m, X, m, C, jtrans, l_j2_1, l_j2_2, J2_val, grad )
      CALL WRITE_C( out, m, C )

!  compute the constraint and sparse Jacobian values ....

!     grad = .TRUE.
!     WRITE( out, "( ' CALL CSCFG with grad = .TRUE.' )" )
!     CALL CSCFG( n, m, X, m, C, J_ne, l_j, J_val, J_var, J_fun, grad )
!     CALL WRITE_C( out, m, C )
!     CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
!     grad = .FALSE.
!     WRITE( out, "( ' CALL CSCFG with grad = .FALSE.' )" )
!     CALL CSCFG( n, m, X, m, C, J_ne, l_j, J_val, J_var, J_fun, grad )
!     CALL WRITE_C( out, m, C )

!  ... and its current version

      grad = .TRUE.
      WRITE( out, "( ' CALL CCFSG with grad = .TRUE.' )" )
      CALL CCFSG( n, m, X, m, C, J_ne, l_j, J_val, J_var, J_fun, grad )
      CALL WRITE_C( out, m, C )
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      grad = .FALSE.
      WRITE( out, "( ' CALL CCFSG with grad = .FALSE.' )" )
      CALL CCFSG( n, m, X, m, C, J_ne, l_j, J_val, J_var, J_fun, grad )
      CALL WRITE_C( out, m, C )

!  compute an individual constraint and its dense gradient

      icon = 1
      grad = .TRUE.
      WRITE( out, "( ' CALL CCIFG with grad = .TRUE.' )" )
      CALL CCIFG( n, icon, X, ci, Ji, grad )
      CALL WRITE_CI( out, icon, ci )
      CALL WRITE_JI( out, n, icon, Ji )
      grad = .FALSE.
      WRITE( out, "( ' CALL CCIFG with grad = .FALSE.' )" )
      CALL CCIFG( n, icon, X, ci, Ji, grad )
      CALL WRITE_CI( out, icon, ci )

!  compute an individual constraint and its sparse gradient ....

!     grad = .TRUE.
!     WRITE( out, "( ' CALL CSCIFG with grad = .TRUE.' )" )
!     CALL CSCIFG( n, icon, X, ci, Ji_ne, n, Ji, J_var, grad )
!     CALL WRITE_CI( out, icon, ci )
!     CALL WRITE_SJI( out, icon, Ji_ne, n, Ji, J_var )
!     grad = .FALSE.
!     WRITE( out, "( ' CALL CSCIFG with grad = .FALSE.' )" )
!     CALL CSCIFG( n, icon, X, ci, Ji_ne, n, Ji, J_var, grad )
!     CALL WRITE_CI( out, icon, ci )

!  ... and its current version

      grad = .TRUE.
      WRITE( out, "( ' CALL CCIFSG with grad = .TRUE.' )" )
      CALL CCIFSG( n, icon, X, ci, Ji_ne, n, Ji, J_var, grad )
      CALL WRITE_CI( out, icon, ci )
      CALL WRITE_SJI( out, icon, Ji_ne, n, Ji, J_var )
      grad = .FALSE.
      WRITE( out, "( ' CALL CCIFSG with grad = .FALSE.' )" )
      CALL CCIFSG( n, icon, X, ci, Ji_ne, n, Ji, J_var, grad )
      CALL WRITE_CI( out, icon, ci )

!  compute the dense Hessian value

      WRITE( out, "( ' CALL CDH' )" )
      CALL CDH( n, m, X, m, Y, l_h2_1, H2_val )
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )

!  compute the dense Hessian value of the objective or a constraint

      iprob = 0
      WRITE( out, "( ' CALL CIDH for objective' )" )
      CALL CIDH( n, X, iprob, l_h2_1, H2_val )
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )
      iprob = 1
      WRITE( out, "( ' CALL CIDH for a constraint' )" )
      CALL CIDH( n, X, iprob, l_h2_1, H2_val )
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )

!  compute the gradient and dense Hessian values

      grlagf = .TRUE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CGRDH with grlagf = .TRUE. and jtrans = .TRUE.' )" )
      CALL CGRDH( n, m, X, grlagf, m, Y, G, jtrans,                            &
                  l_j2_1, l_j2_2, J2_val, l_h2_1, H2_val )
      CALL WRITE_G( out, n, G )
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )
      grlagf = .TRUE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CGRDH with grlagf = .TRUE. and jtrans = .FALSE.' )")
      CALL CGRDH( n, m, X, grlagf, m, Y, G, jtrans,                            &
                  l_j2_1, l_j2_2, J2_val, l_h2_1, H2_val )
      CALL WRITE_G( out, n, G )
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )
      grlagf = .FALSE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CGRDH with grlagf = .FALSE. and jtrans = .TRUE.' )")
      CALL CGRDH( n, m, X, grlagf, m, Y, G, jtrans,                            &
                  l_j2_1, l_j2_2, J2_val, l_h2_1, H2_val )
      CALL WRITE_G( out, n, G )
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )
      grlagf = .FALSE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CGRDH with grlagf = .FALSE. and jtrans = .FALSE.')")
      CALL CGRDH( n, m, X, grlagf, m, Y, G, jtrans,                            &
                  l_j2_1, l_j2_2, J2_val, l_h2_1, H2_val )
      CALL WRITE_G( out, n, G )
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )

!  compute the number of nonzeros in the sparse Hessian

      WRITE( out, "( ' CALL CDIMSH' )" )
      CALL CDIMSH( H_ne )
      WRITE( out, "( ' * H_ne = ', I0 )" ) H_ne

      l_h = H_ne
      ALLOCATE( H_val( l_h ), H_row( l_h ), H_col( l_h ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

!  compute the sparse Hessian value

      WRITE( out, "( ' CALL CSH' )" )
      CALL CSH( n, m, X, m, Y, H_ne, l_h, H_val, H_row, H_col )
      CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )

!  compute the sparse Hessian value without the objective

      WRITE( out, "( ' CALL CSH1' )" )
      CALL CSH1( n, m, X, m, Y, H_ne, l_h, H_val, H_row, H_col )
      CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )

!  compute the sparse Hessian value of the objective or a constraint

      iprob = 0
      WRITE( out, "( ' CALL CISH for objective' )" )
      CALL CISH( n, X, iprob, H_ne, l_h, H_val, H_row, H_col )
      CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )
      iprob = 1
      WRITE( out, "( ' CALL CISH for a constraint' )" )
      CALL CISH( n, X, iprob, H_ne, l_h, H_val, H_row, H_col )
      CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )

!  compute the gradient and sparse Hessian values

      grlagf = .TRUE.
      WRITE( out, "( ' CALL CSGRSH with grlagf = .TRUE.' )" )
      CALL CSGRSH( n, m, X, grlagf, m, Y, J_ne, l_j, J_val,                    &
                   J_var, J_fun, H_ne, l_h, H_val, H_row, H_col )
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )
      grlagf = .FALSE.
      WRITE( out, "( ' CALL CSGRSH with grlagf = .FALSE.' )" )
      CALL CSGRSH( n, m, X, grlagf, m, Y, J_ne, l_j, J_val,                    &
                   J_var, J_fun, H_ne, l_h, H_val, H_row, H_col )
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )

!  compute the number of nonzeros in the element Hessian

      WRITE( out, "( ' CALL CDIMSE' )" )
      CALL CDIMSE( HE_nel, HE_val_ne, HE_row_ne )
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
      WRITE( out, "( ' CALL CEH with byrows = .FALSE.' )" )
      CALL CEH( n, m, X, m, Y, HE_nel, HE_row, lhe_row, lhe_ptr, HE_row_ptr,   &
                HE_val, lhe_val, HE_val_ptr, byrows )
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )
      byrows = .TRUE.
      WRITE( out, "( ' CALL CEH with byrows = .TRUE.' )" )
      CALL CEH( n, m, X, m, Y, HE_nel, HE_row, lhe_row, lhe_ptr, HE_row_ptr,   &
                HE_val, lhe_val, HE_val_ptr, byrows )
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )

!  compute the gradient and element Hessian values

      grlagf = .TRUE. ; byrows = .TRUE.
      WRITE( out, "( ' CALL CSGREH with grlagf = .TRUE. and byrows = .TRUE.')" )
      CALL CSGREH( n, m, X, grlagf, m, Y, J_ne, l_j, J_val, J_var, J_fun,      &
                   HE_nel, HE_row, lhe_row, lhe_ptr, HE_row_ptr, HE_val,       &
                   lhe_val, HE_val_ptr, byrows )
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )
      grlagf = .TRUE. ; byrows = .FALSE.
      WRITE( out, "(' CALL CSGREH with grlagf = .TRUE. and byrows = .FALSE.')" )
      CALL CSGREH( n, m, X, grlagf, m, Y, J_ne, l_j, J_val, J_var, J_fun,      &
                   HE_nel, HE_row, lhe_row, lhe_ptr, HE_row_ptr, HE_val,       &
                   lhe_val, HE_val_ptr, byrows )
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )
      grlagf = .FALSE. ; byrows = .TRUE.
      WRITE( out, "( ' CALL CSGREH with grlagf = .FALSE. and byrows = .TRUE.')")
      CALL CSGREH( n, m, X, grlagf, m, Y, J_ne, l_j, J_val, J_var, J_fun,      &
                   HE_nel, HE_row, lhe_row, lhe_ptr, HE_row_ptr, HE_val,       &
                   lhe_val, HE_val_ptr, byrows )
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )
      grlagf = .FALSE. ; byrows = .FALSE.
      WRITE( out, "(' CALL CSGREH with grlagf = .FALSE. and byrows = .FALSE.')")
      CALL CSGREH( n, m, X, grlagf, m, Y, J_ne, l_j, J_val, J_var, J_fun,      &
                   HE_nel, HE_row, lhe_row, lhe_ptr, HE_row_ptr, HE_val,       &
                   lhe_val, HE_val_ptr, byrows )
      CALL WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )

!  compute a Hessian-vector product

      VECTOR = one
      goth = .FALSE.
      WRITE( out, "( ' Call CPROD with goth = .FALSE.' )" )
      CALL CPROD( n, m, goth, X, m, Y, VECTOR, RESULT )
      CALL WRITE_RESULT( out, n, VECTOR, RESULT )
      goth = .TRUE.
      WRITE( out, "( ' Call CPROD with goth = .TRUE.' )" )
      CALL CPROD( n, m, goth, X, m, Y, VECTOR, RESULT )
      CALL WRITE_RESULT( out, n, VECTOR, RESULT )

!  compute a Hessian-vector product ignoring the objective

      VECTOR = one
      goth = .FALSE.
      WRITE( out, "( ' Call CPROD1 with goth = .FALSE.' )" )
      CALL CPROD1( n, m, goth, X, m, Y, VECTOR, RESULT )
      CALL WRITE_RESULT( out, n, VECTOR, RESULT )
      goth = .TRUE.
      WRITE( out, "( ' Call CPROD1 with goth = .TRUE.' )" )
      CALL CPROD1( n, m, goth, X, m, Y, VECTOR, RESULT )
      CALL WRITE_RESULT( out, n, VECTOR, RESULT )

!  compute a Jacobian-vector product

      VECTOR = one
      gotj = .FALSE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CJPROD with gotj = .FALSE. and jtrans = .FALSE.')" )
      CALL CJPROD( n, m, gotj, jtrans, X, VECTOR, n, RESULT, m )
      CALL WRITE_RESULT2( out, n, VECTOR, m, RESULT )
      gotj = .TRUE. ; jtrans = .FALSE.
      WRITE( out, "( ' CALL CJPROD with gotj = .TRUE. and jtrans = .TRUE.' )" )
      CALL CJPROD( n, m, gotj, jtrans, X, VECTOR, n, RESULT, m )
      CALL WRITE_RESULT2( out, n, VECTOR, m, RESULT )
      gotj = .FALSE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CJPROD with gotj = .FALSE. and jtrans = .TRUE.')" )
      CALL CJPROD( n, m, gotj, jtrans, X, VECTOR, m, RESULT, n )
      CALL WRITE_RESULT2( out, m, VECTOR, n, RESULT )
      gotj = .TRUE. ; jtrans = .TRUE.
      WRITE( out, "( ' CALL CJPROD with gotj = .TRUE. and jtrans = .TRUE.' )" )
      CALL CJPROD( n, m, gotj, jtrans, X, VECTOR, m, RESULT, n )
      CALL WRITE_RESULT2( out, m, VECTOR, n, RESULT )

!  calls and time report

      WRITE( out, "( ' CALL CREPRT' )" )
      CALL CREPRT( CALLS, CPU )
      WRITE( out, "( ' CALLS(1-7) =', 7( 1X, I0 ) )" ) INT( CALLS( 1 : 7 ) )
      WRITE( out, "( ' CPU(1-2) =', 2F7.2 )" ) CPU( 1 : 2 )

!  terminal exit

      DEALLOCATE( X_type, H_row, H_col, HE_row, HE_row_ptr, HE_val_ptr, X,     &
                  X_l, X_u, G, Ji, Y, C_l, C_u, C, H_val, HE_val, H2_val,      &
                  J_val, J_var, J_fun, J2_val, VECTOR, RESULT,                 &
                  X_names, C_names, EQUATION, LINEAR, stat = alloc_stat )
      CLOSE( input )
      STOP

!  error exits

 990  CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) alloc_stat
      CLOSE( INPUT  )
      STOP

    CONTAINS

!  data printing subroutines

      SUBROUTINE WRITE_X( out, n, X, X_l, X_u )
      INTEGER :: n, out
      REAL ( KIND = wp ), DIMENSION( n ) :: X, X_l, X_u
      WRITE( out, "( ' *       i      X_l          X          X_u' )" )
      DO i = 1, n
        WRITE( out, "( ' * ', I7, 3ES12.4 )" ) i, X_l( i ), X( i ), X_u( i )
      END DO
      END SUBROUTINE WRITE_X

      SUBROUTINE WRITE_Y( out, m, Y, C_l, C_u, EQUATION, LINEAR )
      INTEGER :: m, out
      REAL ( KIND = wp ), DIMENSION( m ) :: Y, C_l, C_u
      LOGICAL, DIMENSION( m ) :: EQUATION, LINEAR
      WRITE( out, "( ' *       i      C_l         C_u          Y   ',          &
    &   '      EQUATION   LINEAR' )" )
      DO i = 1, m
        WRITE( out, "( ' * ', I7, 3ES12.4, 2L10 )" ) i, C_l( i ), C_u( i ),    &
          Y( i ), EQUATION( i ), LINEAR( i )
      END DO
      END SUBROUTINE WRITE_Y

      SUBROUTINE WRITE_X_type( out, n, X_type )
      INTEGER :: n, out
      INTEGER, DIMENSION( n ) :: X_type
      INTEGER :: i
      WRITE( out, "( ' *       i  X_type' )" )
      DO i = 1, n
        WRITE( out, "( ' * ', I7, 2X, I0 )" ) i, X_type( i )
      END DO
      END SUBROUTINE WRITE_X_type

      SUBROUTINE WRITE_p_name( out, p_name )
      INTEGER :: out
      CHARACTER ( len = 10 ) ::  p_name
      WRITE( out, "( ' * p_name = ', A )" ) p_name
      END SUBROUTINE WRITE_p_name

      SUBROUTINE WRITE_X_names( out, n, X_names )
      INTEGER :: n, out
      CHARACTER ( len = 10 ), DIMENSION( n ) :: X_names
      INTEGER :: i
      WRITE( out, "( ' *       i  X_name' )" )
      DO i = 1, n
        WRITE( out, "( ' * ', I7, 2X, A10 )" ) i, X_names( i )
      END DO
      END SUBROUTINE WRITE_X_names

      SUBROUTINE WRITE_C_names( out, m, C_names )
      INTEGER :: m, out
      CHARACTER ( len = 10 ), DIMENSION( m ) :: C_names
      INTEGER :: i
      WRITE( out, "( ' *       i  C_name' )" )
      DO i = 1, m
        WRITE( out, "( ' * ', I7, 2X, A10 )" ) i, C_names( i )
      END DO
      END SUBROUTINE WRITE_C_names

      SUBROUTINE WRITE_f( out, f )
      INTEGER :: out
      REAL ( KIND = wp ) :: f
      WRITE( out, "( ' * f = ', ES12.4 )" ) f
      END SUBROUTINE WRITE_f

      SUBROUTINE WRITE_C( out, m, C )
      INTEGER :: m, out
      REAL ( KIND = wp ), DIMENSION( m ) :: C
      INTEGER :: i
      WRITE( out, "( ' *       i       C' )" )
      DO i = 1, m
        WRITE( out, "( ' * ', I7, ES12.4 )" ) i, C( i )
      END DO
      END SUBROUTINE WRITE_C

      SUBROUTINE WRITE_CI( out, icon, ci )
      INTEGER :: icon, out
      REAL ( KIND = wp ) :: ci
      WRITE( out, "( ' * c(', I0, ') = ', ES12.4 )" ) icon, ci
      END SUBROUTINE WRITE_CI

      SUBROUTINE WRITE_G( out, n, G )
      INTEGER :: n, out
      REAL ( KIND = wp ), DIMENSION( n ) :: G
      INTEGER :: i
      WRITE( out, "( ' *       i       G' )" )
      DO i = 1, n
        WRITE( out, "( ' * ', I7, ES12.4 )" ) i, G( i )
      END DO
      END SUBROUTINE WRITE_G

      SUBROUTINE WRITE_JI( out, n, icon, Ji )
      INTEGER :: n, icon, out
      REAL ( KIND = wp ), DIMENSION( n ) :: Ji
      INTEGER :: i
      WRITE( out, "( ' *       i   J(', I0, ')' )" ) icon
      DO i = 1, n
        WRITE( out, "( ' * ', I7, ES12.4 )" ) i, Ji( i )
      END DO
      END SUBROUTINE WRITE_JI

      SUBROUTINE WRITE_SJI( out, icon, Ji_ne, lji, Ji, J_var )
      INTEGER :: icon, Ji_ne, lji, out
      INTEGER, DIMENSION( lji ) :: J_var
      REAL ( KIND = wp ), DIMENSION( lji ) :: Ji
      INTEGER :: i
      WRITE( out, "( ' *       i   J(', I0, ')' )" ) icon
      DO i = 1, Ji_ne
        WRITE( out, "( ' * ', I7, ES12.4 )" ) J_var( i ), Ji( i )
      END DO
      END SUBROUTINE WRITE_SJI

      SUBROUTINE WRITE_H_dense( out, n, l_h2_1, H2_val )
      INTEGER :: n, l_h2_1, out
      REAL ( KIND = wp ), DIMENSION( l_h2_1, n ) :: H2_val
      INTEGER :: i, j
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
      INTEGER :: n, m, l_J2_1, out
      REAL ( KIND = wp ), DIMENSION( l_j2_1, l_j2_2 ) :: J2_val
      INTEGER :: i, j
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
      INTEGER :: n, m, l_J2_1, out
      REAL ( KIND = wp ), DIMENSION( l_j2_1, l_j2_2 ) :: J2_val
      INTEGER :: i, j
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

      SUBROUTINE WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )
      INTEGER :: l_h, H_ne, out
      INTEGER, DIMENSION( l_h ) :: H_row, H_col
      REAL ( KIND = wp ), DIMENSION( l_h ) :: H_val
      INTEGER :: i
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

      SUBROUTINE WRITE_J_sparse( out, J_ne, l_j, J_val, J_fun, J_var )
      INTEGER :: l_j, J_ne, out
      INTEGER, DIMENSION( l_j ) :: J_fun, J_var
      REAL ( KIND = wp ), DIMENSION( l_j ) :: J_val
      INTEGER :: i
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
      INTEGER :: ne, lhe_ptr, lhe_row, lhe_val, out
      INTEGER, DIMENSION( lhe_ptr ) :: HE_row_ptr, HE_val_ptr
      INTEGER, DIMENSION( lhe_row ) :: HE_row
      REAL ( KIND = wp ), DIMENSION( lhe_val ) :: HE_val
      WRITE( out, "( ' * H(element)' )" )
      DO i = 1, ne
        IF (  HE_row_ptr( i + 1 ) > HE_row_ptr( i ) ) THEN
          WRITE( out, "( ' * element ', I0 )" ) i
          WRITE( out, "( ' * indices ', 5I12, /, ( ' *', 9X, 5I12 ) )" )       &
           HE_row( HE_row_ptr( i ) : HE_row_ptr( i + 1 ) - 1 )
          WRITE( out, "( ' * values  ', 5ES12.4, /, ( ' *', 9X, 5ES12.4 ) )" ) &
           HE_val( HE_val_ptr( i ) : HE_val_ptr( i + 1 ) - 1 )
        ELSE
          WRITE( out, "( ' * no indices' )" )
        END IF
      END DO
      END SUBROUTINE WRITE_H_element

      SUBROUTINE WRITE_RESULT( out, n, VECTOR, RESULT )
      INTEGER :: n, out
      REAL ( KIND = wp ), DIMENSION( n ) :: VECTOR, RESULT
      INTEGER :: i
      WRITE( out, "( ' *       i    VECTOR     RESULT' )" )
      DO i = 1, n
        WRITE( out, "( ' * ', I7, 2ES12.4 )" ) i, VECTOR( i ), RESULT( i )
      END DO
      END SUBROUTINE WRITE_RESULT

      SUBROUTINE WRITE_RESULT2( out, len_vector, VECTOR, len_result, RESULT )
      INTEGER :: len_vector, len_result, out
      REAL ( KIND = wp ), DIMENSION( len_vector ) :: VECTOR
      REAL ( KIND = wp ), DIMENSION( len_result ) :: RESULT
      INTEGER :: i
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

    END PROGRAM CUTEST_test_constrained_tools

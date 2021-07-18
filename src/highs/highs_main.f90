!     ( Last modified on 15 Jul 2021 at 12:50:00 )

  PROGRAM HiGHS_main

!  main program for HiGHS strictly-convex QP package

!  Nick Gould, July 2021

  USE, INTRINSIC :: iso_c_binding
  USE highs_fortran_api
  USE CUTEst_interface_double
  USE CUTEST_LQP_double

  IMPLICIT NONE

!  Parameters

  INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )
  INTEGER, PARAMETER :: input = 55
  INTEGER, PARAMETER :: io_buffer = 11
  INTEGER, PARAMETER :: out = 6
  INTEGER, PARAMETER :: input_specfile = 34
  INTEGER, PARAMETER :: spec = 29
  REAL ( KIND = wp ), PARAMETER :: ten = 10.0_wp
  REAL ( KIND = wp ), PARAMETER :: ac_tol = ten ** ( - 6 )
  REAL ( KIND = wp ), PARAMETER :: eq_tol = ten ** ( - 10 )

!  Highs parameters

  INTEGER ( KIND = c_int ), PARAMETER :: sense = 1
  INTEGER ( KIND = c_int ), PARAMETER :: aformat_colwise = 1
  INTEGER ( KIND = c_int ), PARAMETER :: qformat_triangular = 1
  INTEGER ( KIND = c_int ), PARAMETER :: modelstatus_optimal = 7
  INTEGER ( KIND = c_int ), PARAMETER :: runstatus_error = - 1
  INTEGER ( KIND = c_int ), PARAMETER :: runstatus_ok = 0
  INTEGER ( KIND = c_int ), PARAMETER :: runstatus_warning = - 1
  REAL ( KIND = c_double ), PARAMETER :: offset = 0
  LOGICAL, PARAMETER :: no_highs_logging = .TRUE.
  LOGICAL ( KIND = c_bool ), PARAMETER :: logical_false = .false.
  LOGICAL ( KIND = c_bool ), PARAMETER :: logical_true = .true.
  INTEGER ( KIND = c_int ) :: iteration_count
  REAL ( KIND = c_double ) :: objective_function_value
  TYPE ( c_ptr ) :: highs

!  local variables

  INTEGER :: status, n, m, nea, neh, nname, lenc, ncolh, iobj, ns, ninf
  INTEGER :: i, ir, ic, iter, j, k, l, l1, l2, row, col
  INTEGER :: mfixed, mdegen, nfixed, ndegen, mequal, mredun
  REAL ( KIND = wp ) :: f, sinf, obj, val, res_p, res_d, TIMES( 4 ), CALLS( 7 )
  REAL ( KIND = wp ) :: gcol, max_d
  LOGICAL :: qp, filexst, fulsol = .FALSE.
  CHARACTER ( LEN = 1 ) :: equal, start, c_dummy( 1 )
  CHARACTER ( LEN = 8 ) :: prob
  CHARACTER ( LEN = 10 ) :: p_name
  CHARACTER ( LEN = 80 ) :: option_name, option_value
  INTEGER, ALLOCATABLE, DIMENSION( : ) :: HELAST, HS, I_w, I_user
  INTEGER, ALLOCATABLE, DIMENSION( : ) :: A_ptr, A_row, H_ptr, H_row
  REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: G, X_0, X, X_l, X_u
  REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: Z, Y, C_l, C_u, C, G_l
  REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: B_l, B_u, R_w, R_user
  REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: A_val, H_val
  CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: X_names, C_names
  CHARACTER ( LEN =  5 ) :: state
  CHARACTER ( LEN = 8 ), ALLOCATABLE, DIMENSION( : )  :: C_w, C_user

!  local C variables

  INTEGER :: modelstatus, runstatus
  INTEGER ( KIND = c_int ) :: i_value, num_primal_infeasibilities
  INTEGER ( KIND = c_int ) :: num_dual_infeasibilities
  REAL ( KIND = c_double ) :: r_value,  max_primal_infeasibility
  REAL ( KIND = c_double ) :: max_dual_infeasibility
  LOGICAL ( KIND = c_bool ) :: b_value
  CHARACTER ( KIND = c_char, LEN = 80 ) :: s_value
  INTEGER ( KIND = c_int ) :: numcol, numrow, numnz, hessian_numnz
  INTEGER ( KIND = c_int ), ALLOCATABLE, DIMENSION( : ) :: astart
  INTEGER ( KIND = c_int ), ALLOCATABLE, DIMENSION( : ) :: aindex
  INTEGER ( KIND = c_int ), ALLOCATABLE, DIMENSION( : ) :: qstart
  INTEGER ( KIND = c_int ), ALLOCATABLE, DIMENSION( : ) :: qindex
  INTEGER ( KIND = c_int ), ALLOCATABLE, DIMENSION( : ) :: integrality
  INTEGER ( KIND = c_int ), ALLOCATABLE, DIMENSION( : ) :: colbasisstatus
  INTEGER ( KIND = c_int ), ALLOCATABLE, DIMENSION( : ) :: rowbasisstatus
  REAL ( KIND = c_double ), ALLOCATABLE, DIMENSION( : ) :: colcost
  REAL ( KIND = c_double ), ALLOCATABLE, DIMENSION( : ) :: collower
  REAL ( KIND = c_double ), ALLOCATABLE, DIMENSION( : ) :: colupper
  REAL ( KIND = c_double ), ALLOCATABLE, DIMENSION( : ) :: rowlower
  REAL ( KIND = c_double ), ALLOCATABLE, DIMENSION( : ) :: rowupper
  REAL ( KIND = c_double ), ALLOCATABLE, DIMENSION( : ) :: avalue
  REAL ( KIND = c_double ), ALLOCATABLE, DIMENSION( : ) :: qvalue
  REAL ( KIND = c_double ), ALLOCATABLE, DIMENSION( : ) :: sol
  REAL ( KIND = c_double ), ALLOCATABLE, DIMENSION( : ) :: colvalue
  REAL ( KIND = c_double ), ALLOCATABLE, DIMENSION( : ) :: coldual
  REAL ( KIND = c_double ), ALLOCATABLE, DIMENSION( : ) :: rowvalue
  REAL ( KIND = c_double ), ALLOCATABLE, DIMENSION( : ) :: rowdual
  
!  results summary output if required (set output_summary > 10) 

! INTEGER :: output_summary = 0
  INTEGER :: output_summary = 47
  CHARACTER ( LEN = 10 ) :: summary_filename = 'HIGHS.res'

!  build the QP using column storage

  OPEN( input, file = 'OUTSDIF.d', form = 'FORMATTED', status = 'OLD' )
  REWIND( input )

  CALL CUTEST_LQP_create( status, input, io_buffer, out, n, m, f, G, X_0,      &
                          X_l, X_u, Z, Y, C_l, C_u, p_name, X_names, C_names,  &
                          A_row = A_row, A_ptr = A_ptr, A_val = A_val,         &
                          H_col = H_row, H_ptr = H_ptr, H_val = H_val,         &
                          dont_terminate_cutest = .TRUE. )

!  check that the build succeeded

  IF ( status /= 0 ) GO TO 910
  CLOSE( input )

!  is it an LP or a QP?

  qp = H_ptr( n + 1 ) /= 1

!  append a summary of the results to a file if required

  IF ( output_summary > 10 ) THEN
    INQUIRE( FILE = summary_filename, EXIST = filexst )
    IF ( filexst ) THEN
       OPEN( output_summary, FILE = summary_filename,                          &
             FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND',          &
             IOSTAT = status )
    ELSE
       OPEN( output_summary, FILE = summary_filename,                          &
             FORM = 'FORMATTED', STATUS = 'NEW', IOSTAT = status )
    END IF
    IF ( status /= 0 ) THEN
      WRITE( out, "( ' IOSTAT = ', I6, ' when opening file ', A,               &
     & '. Stopping ' )" ) status, summary_filename
      STOP
    END IF
    WRITE( output_summary, "( A10 )" ) p_name
  END IF

!  transform the data into the structure required by HiGHS, allocating
!  space as required, and deallocating the reciprocal data after it
!  has been transfered

  DEALLOCATE( X_0, Z, STAT = status )
  IF ( status /= 0 ) GO TO 990

!  set dimensions

  numcol = n
  numrow = m
  numnz = A_ptr( n + 1 ) - 1
  hessian_numnz = H_ptr( n + 1 ) - 1

!  transfer the linear term for the objective function

!write(6,*) ' G ', G
  ALLOCATE( colcost( numcol ), STAT = status )
  IF ( status /= 0 ) GO TO 990
  colcost( : numcol ) = G( : numcol )
  DEALLOCATE( G, STAT = status )
  IF ( status /= 0 ) GO TO 990
  
!  transfer the lower and upper variable bounds

  ALLOCATE( collower( numcol ), colupper( numcol ), STAT = status )
  IF ( status /= 0 ) GO TO 990
  collower( : numcol ) = X_l( : numcol )
  colupper( : numcol ) = X_u( : numcol )
  DEALLOCATE( X_l, X_u, STAT = status )
  IF ( status /= 0 ) GO TO 990

!  transfer the lower and upper constraint bounds

  ALLOCATE( rowlower( numrow ), rowupper( numrow ), STAT = status )
  IF ( status /= 0 ) GO TO 990
  rowlower( : numrow ) = C_l( : numrow )
  rowupper( : numrow ) = C_u( : numrow )
  DEALLOCATE( C_l, C_u, STAT = status )
  IF ( status /= 0 ) GO TO 990

!  transfer the constraint matrix

!write(6,*) ' A_ptr ', A_ptr
!write(6,*) ' A_row ', A_row
!write(6,*) ' A_val ', A_val

  ALLOCATE( astart( numcol ), aindex( numnz ), avalue( numnz ), STAT = status )
  IF ( status /= 0 ) GO TO 990
  astart( : numcol ) = A_ptr( : numcol ) - 1
  aindex( : numnz ) = A_row( : numnz ) - 1
  avalue( : numnz ) = A_val( : numnz )
!write(6,*) ' astart ', astart
!write(6,*) ' aindex ', aindex
!write(6,*) ' avalue ', avalue
  DEALLOCATE( A_ptr, A_row, A_val, STAT = status )
  IF ( status /= 0 ) GO TO 990

!  transfer the Hessian term for the objective function

!write(6,*) ' qp ', qp
  IF ( qp ) THEN
!DO i = 1, n
!  WRITE( 6, "( I6, ' : ', ( 10I6 ) )" ) i, ( H_row( H_ptr( i ) : H_ptr( i + 1 ) - 1 ) )
!END DO
!write(6,*) ' H_ptr ', H_ptr
!write(6,*) ' H_row ', H_row
!write(6,*) ' H_val ', H_val

    ALLOCATE( qstart( numcol ), qindex( hessian_numnz ),                       &
              qvalue( hessian_numnz ), STAT = status )
    IF ( status /= 0 ) GO TO 990
    qstart( : numcol ) = H_ptr( : numcol ) - 1
    qindex( : hessian_numnz ) = H_row( : hessian_numnz ) - 1
    qvalue( : hessian_numnz ) = H_val( : hessian_numnz )
!write(6,*) ' qstart ', qstart
!write(6,*) ' qindex ', qindex
!write(6,*) ' qvalue ', qvalue
  END IF
  DEALLOCATE( H_ptr, H_row, H_val, STAT = status )
  IF ( status /= 0 ) GO TO 990

!  allocate other arrays needed by HiGHS

  ALLOCATE( integrality( numcol ), STAT = status )
  IF ( status /= 0 ) GO TO 990
  integrality = 0

  ALLOCATE( sol( numcol ), STAT = status )
  IF ( status /= 0 ) GO TO 990
  ALLOCATE( colvalue( numcol ), STAT = status )
  IF ( status /= 0 ) GO TO 990
  colvalue = 0.0_wp
  ALLOCATE( coldual( numcol ), STAT = status )
  IF ( status /= 0 ) GO TO 990
  coldual = 0.0_wp
  ALLOCATE( rowvalue( numrow ), STAT = status )
  IF ( status /= 0 ) GO TO 990
  rowvalue = 0.0_wp
  ALLOCATE( rowdual( numrow ), STAT = status )
  IF ( status /= 0 ) GO TO 990
  rowdual = 0.0_wp
  ALLOCATE( colbasisstatus( numcol ), STAT = status )
  IF ( status /= 0 ) GO TO 990
  ALLOCATE( rowbasisstatus( numrow ), STAT = status )
  IF ( status /= 0 ) GO TO 990

!  create the HiGHS environment

  highs = Highs_create( )

!  read options file

  OPEN( spec, file = 'HiGHS.SPC', form = 'FORMATTED', status = 'OLD' )
  REWIND( spec )
  DO 
    READ( spec, *, END = 100 ) option_name, equal, option_value
    option_name = ADJUSTL( option_name )
    IF ( option_name( 1 : 1 ) == ' ' .OR. option_name( 1 : 1 ) == '#' ) CYCLE
!   WRITE( 6, "( A )" ) TRIM( option_name )
    SELECT CASE ( TRIM( option_name ) )
    CASE( "time_limit", "infinite_cost", "infinite_bound",                     &
          "small_matrix_value", "large_matrix_value",                          &
          "primal_feasibility_tolerance",                                      &
          "dual_feasibility_tolerance",                                        &
          "objective_bound", "objective_target" )
      READ( option_value, * ) r_value
      runstatus = Highs_setDoubleOptionValue( highs,                           &
         TRIM( option_name ) // C_NULL_CHAR, r_value )
    CASE( "highs_random_seed", "highs_debug_level",                            &
          "highs_analysis_level", "simplex_strategy",                          &
          "simplex_scale_strategy", "simplex_crash_strategy",                  &
          "simplex_dual_edge_weight_strategy",                                 &
          "simplex_primal_edge_weight_strategy",                               &
          "simplex_iteration_limit", "simplex_update_limit",                   &
          "highs_min_threads", "highs_max_threads" )
      READ( option_value, * ) i_value
      runstatus = Highs_setIntOptionValue( highs,                              &
         TRIM( option_name ) // C_NULL_CHAR, i_value )
    CASE( "output_flag", "write_solution_to_file",                             &
          "write_solution_pretty", "log_to_console" )
      READ( option_value, * ) b_value
      runstatus = Highs_setBoolOptionValue( highs,                             &
         TRIM( option_name ) // C_NULL_CHAR, b_value )
    CASE ( "solution_file", "log_file" )
      READ( option_value, * ) s_value
      runstatus = Highs_setStringOptionValue( highs,                           &
         TRIM( option_name ) // C_NULL_CHAR, TRIM( s_value ) )
    CASE ( "print_full_solution" )
      READ( option_value, * ) b_value
      fulsol = b_value
    CASE DEFAULT
       WRITE( 6, "( A, ' not recognised ' )" ) TRIM( option_name )
    END SELECT
  END DO
  100 CONTINUE
  CLOSE( spec )

!  transfer the data before solution

  highs = Highs_create( )

  IF ( no_highs_logging ) THEN
    runstatus = Highs_setBoolOptionValue(highs,  &
                  "output_flag" // C_NULL_CHAR, logical_false )
  endif

  runstatus = Highs_passLp( highs, numcol, numrow, numnz, aformat_colwise,     &
                            sense, offset, colcost, collower, colupper,        &
                            rowlower, rowupper, astart, aindex, avalue )

  IF ( qp ) THEN
    runstatus = Highs_passHessian( highs, numcol, hessian_numnz,               &
                                   qformat_triangular, qstart, qindex, qvalue )

!   runstatus = Highs_qpCall( numcol, numrow, numnz, hessian_numnz,            &
!                             aformat_colwise, qformat_triangular, sense,      &
!                             offset, colcost, collower, colupper, rowlower,   &
!                             rowupper, astart, aindex, avalue, qstart, qindex,&
!                             qvalue, colvalue, coldual, rowvalue, rowdual,    &
!                             colbasisstatus, rowbasisstatus, modelstatus )
  ELSE
!   runstatus = Highs_lpCall( numcol, numrow, numnz, aformat_colwise, sense,   &
!                             offset, colcost, collower, colupper, rowlower,   &
!                             rowupper, astart, aindex, avalue,                &
!                             colvalue, coldual, rowvalue, rowdual,            &
!                             colbasisstatus, rowbasisstatus, modelstatus)
  END IF

!  solve the problem

  runstatus = Highs_run( highs )
  modelstatus = Highs_getModelStatus( highs )

!  recover the objective function value and iteration count

  runstatus = Highs_getDoubleInfoValue( highs,                                 &
    "objective_function_value" // C_NULL_CHAR, objective_function_value )
  IF ( qp ) THEN
    runstatus = Highs_getIntInfoValue( highs,                                  &
      "qp_iteration_count" // C_NULL_CHAR, iteration_count )
  ELSE
    runstatus = Highs_getIntInfoValue( highs,                                  &
      "simplex_iteration_count" // C_NULL_CHAR, iteration_count )
  END IF

!  compute the number and maximum values of the primal and dual infeasibilities

  runstatus = Highs_getIntInfoValue( highs,                                    &
      "num_primal_infeasibilities" // C_NULL_CHAR, num_primal_infeasibilities )
  runstatus = Highs_getDoubleInfoValue( highs,                                 &
      "max_primal_infeasibility" // C_NULL_CHAR, max_primal_infeasibility )
  runstatus = Highs_getIntInfoValue( highs,                                    &
      "num_dual_infeasibilities" // C_NULL_CHAR, num_dual_infeasibilities )
  runstatus = Highs_getDoubleInfoValue( highs,                                 &
      "max_dual_infeasibility" // C_NULL_CHAR, max_dual_infeasibility )

!  get the primal and dual solution ...

  runstatus = Highs_getSolution( highs, colvalue, coldual, rowvalue, rowdual )

!  ... and the basis

  runstatus = Highs_getBasis( highs, colbasisstatus, rowbasisstatus )

  IF ( runstatus /= runstatus_ok ) THEN
    WRITE( 6, "( ' Highs_lpCall run status is not ', I0, ' but ', I0 )" )      &
      runstatus_ok, runstatus
  ELSE
    WRITE( 6, "( ' Run status = ', I0, ', model status = ', I0 )" )            &
       runstatus, modelstatus
    IF ( modelstatus == modelstatus_optimal ) THEN

! report the column primal and dual values, and basis status
    
!     IF ( qp ) THEN
!       DO col = 1, n
!         WRITE( 6, "( ' Col ', I6, ' = ', ES12.4, ' dual = ', ES12.4 )" )     &
!           col, colvalue( col ), coldual( col )
!       END DO
!     ELSE
!       DO col = 1, n
!         WRITE( 6, "( ' Col ', I6, ' = ', ES12.4, ' dual = ', ES12.4,         &
!        & ' status = ', I6 )" ) col, colvalue( col ), coldual( col ),         &
!           colbasisstatus( col )
!       END DO
!     END IF

! report the row primal and dual values, and basis status

!     IF ( qp ) THEN
!        DO row = 1, m
!         WRITE( 6, "( ' Row ', I6, ' = ', ES12.4, ' dual = ', ES12.4 )" )     &
!           row, rowvalue(row), rowdual( row )
!       END DO
!     ELSE
!       DO row = 1, m
!         WRITE( 6, "( ' Row ', I6, ' = ', ES12.4, ' dual = ', ES12.4,         &
!        & ' status = ',  I6 )" ) row, rowvalue(row), rowdual( row ),          &
!           rowbasisstatus( row )
!       END DO
!     END IF

! compute the objective function value

      obj = f + DOT_PRODUCT( colvalue, colcost )
      IF ( qp ) THEN
        DO col = 1, n
          l1 = qstart( col ) + 1
          IF ( col < n ) THEN
            l2 = qstart( col + 1 )
          ELSE
            l2 = hessian_numnz
          END IF
          obj = obj + 0.5_wp * colvalue( col ) * qvalue( l1 ) * colvalue( col )
          DO l = l1 + 1, l2
            row = qindex( l ) + 1
            obj = obj + colvalue( col ) * qvalue( l ) * colvalue( row )
          END DO
        END DO
      END IF
!     WRITE( 6, "( /, ' Optimal objective value =', ES22.14 )" ) obj
    END IF
  END IF

!  compute the primal and dual residuals if necessary

  IF ( output_summary > 10 ) THEN
    ALLOCATE( C( m ), G_l( n ), STAT = status )
    IF ( status /= 0 ) GO TO 990
    G_l( : n ) = colcost( : n )
    IF ( qp ) THEN
      DO col = 1, n
        l1 = qstart( col ) + 1
        IF ( col < n ) THEN
          l2 = qstart( col + 1 )
        ELSE
          l2 = hessian_numnz
        END IF
        G_l( col ) = G_l( col ) + qvalue( l1 ) * colvalue( col )
        DO l = l1 + 1, l2
          row = qindex( l ) + 1
          G_l( col ) = G_l( col ) + qvalue( l ) * colvalue( row )
          G_l( row ) = G_l( row ) + qvalue( l ) * colvalue( col )
        END DO
      END DO
    END IF
    G_l( : n ) = G_l( : n ) - coldual( : n )  ; C( : m ) = 0.0_wp
    DO col = 1, n
      gcol = G_l( col) + coldual( col )
      l1 = astart( col ) + 1
      IF ( col < n ) THEN
        l2 = astart( col + 1 )
      ELSE
        l2 = numnz
      END IF
      DO l = l1, l2
        row = aindex( l ) + 1
        G_l( col ) = G_l( col ) - avalue( l ) * rowdual( row )
        C( row ) = C( row ) + avalue( l ) * colvalue( col )
      END DO
!     IF ( ABS( G_l( col ) ) > 0.00000001 ) write(6,"(I6, 4ES12.4)") col,      &
!       G_l( col ), gcol, G_l( col ) - gcol - coldual( col ), coldual( col )
    END DO
    C( : m ) = MIN( MAX( rowlower( : m ), C( : m ) ), rowupper( : m ) )        &
                 - C( : m )
    res_p = MAXVAL( ABS( C( : m ) ) ) ; res_d = MAXVAL( ABS( G_l( : n ) ) )
!   WRITE( 6, "( ' Primal and dual infeasibility =', 2ES22.14 )" ) res_p, res_d
    DEALLOCATE( G_l, C, STAT = status )
    IF ( status /= 0 ) GO TO 990

!  WRITE( 6, "( 1X, I0, ' iterations required' )" ) iteration_count
!  WRITE( 6, "( ' number of & maximum primal infeasibilities: ', I0, ES12.4 )")&
!    num_primal_infeasibilities,  max_primal_infeasibility
!  WRITE( 6, "( ' number of & maximum dual infeasibilities: ', I0, ES12.4 )" ) &
!    num_dual_infeasibilities,  max_dual_infeasibility

!  output a summary of the results to a file if required

    BACKSPACE( output_summary )
    iter = iteration_count
    SELECT CASE ( runstatus )
    CASE ( runstatus_ok )
      WRITE( output_summary,                                                   &
        "( A10, 1X, I8, 1X, I8, ES16.8, 2ES9.1, bn, I9, F12.2, I6 )" )         &
       p_name, n, m, obj, res_p, res_d, iter, TIMES( 4 ), runstatus
    CASE DEFAULT
      WRITE( output_summary,                                                   &
        "( A10,  1X, I8, 1X, I8, ES16.8, 2ES9.1, bn, I9, F12.2, I6 )" )        &
        p_name, n, m, obj, res_p, res_d, -iter, -TIMES( 4 ), runstatus
    END SELECT
    CLOSE( output_summary )
  END IF

!  write details

! WRITE( out, "(' Final objective value = ', ES11.3 )" ) obj
! WRITE( out, "(' Optimal X = ', 7F9.2 )" ) X( : n )

  CALL CUTEST_creport( status, CALLS, TIMES )
  WRITE( out, "( /, 24('*'), ' CUTEst statistics ', 24('*') //                 &
 &              ,' Package used            :  HiGHS',    /                     &
 &              ,' Problem                 :  ', A10,    /                     &
 &              ,' # variables             =      ', I10 /                     &
 &              ,' # constraints           =      ', I10 /                     &
 &              ,' Exit code               =      ', I10 /                     &
 &              ,' Final f                 = ', ES15.7 /                       &
 &              ,' Set up time             =      ', 0P, F10.2, ' seconds' /   &
 &              ,' Solve time              =      ', 0P, F10.2, ' seconds' //  &
 &               66('*') / )" ) p_name, n, m, runstatus, obj,                  &
     TIMES( 1 ), TIMES( 2 )

  l = 4 ; IF ( fulsol ) l = n

!  Print details of the primal and dual variables

  WRITE( 6, "( ' Solution : ', /, '                              ',            &
 &             '        <------ Bounds ------> ', /                            &
 &             '      # name       state    value   ',                         &
 &             '    Lower       Upper       Dual' )" )
  DO j = 1, 2
    IF ( j == 1 ) THEN
      ir = 1 ; ic = MIN( l, n )
    ELSE
      IF ( ic < n - l ) WRITE( 6, "( '      . .          .....',               &
     & ' ..........  ..........  ..........  .......... ' )" )
      ir = MAX( ic + 1, n - ic + 1 ) ; ic = n
    END IF
    DO i = ir, ic
      state = ' FREE'
      IF ( ABS( colvalue( i ) - collower( i ) ) < ac_tol ) state = 'LOWER'
      IF ( ABS( colvalue( i ) - colupper( i ) ) < ac_tol ) state = 'UPPER'
      IF ( ABS( collower( i ) - colupper( i ) ) < eq_tol ) state = 'FIXED'
      WRITE( 6, "( I7, 1X, A10, A6, 4ES12.4 )" ) i, X_names( i ), state,       &
        colvalue( i ), collower( i ), colupper( i ), coldual( i )
    END DO
  END DO

!  Compute the number of fixed and degenerate variables.

  nfixed = 0 ; ndegen = 0
  DO i = 1, n
    IF ( ABS( colupper( i ) - collower( i ) ) < eq_tol ) THEN
      nfixed = nfixed + 1
      IF ( ABS( coldual( i ) ) < ac_tol ) ndegen = ndegen + 1
    ELSE IF ( MIN( ABS( colvalue( i ) - collower( i ) ),                       &
              ABS( colvalue( i ) - colupper( i ) ) ) <=                        &
              MAX( ac_tol, ABS( coldual( i ) ) ) ) THEN
      nfixed = nfixed + 1
      IF ( ABS( coldual( i ) ) < ac_tol ) ndegen = ndegen + 1
    END IF
  END DO

!  Print details of the constraints.

  IF ( m > 0 ) THEN
    WRITE( out, "( /, ' Constraints : ', /, '                  ',              &
   &       '        <------ Bounds ------> ', /                                &
   &       '      # name       state    value   ',                             &
   &       '    Lower       Upper     Multiplier ' )" )
    l = 2  ; IF ( fulsol ) l = m
    DO j = 1, 2
      IF ( j == 1 ) THEN
        ir = 1 ; ic = MIN( l, m )
      ELSE
        IF ( ic < m - l ) WRITE( out, "( '      . .          .....',           &
       & ' ..........  ..........  ..........  .......... ' )" )
        ir = MAX( ic + 1, m - ic + 1 ) ; ic = m
      END IF
      DO i = ir, ic
        state = ' FREE'
        IF ( ABS( rowvalue( I ) - rowlower( i ) ) < ac_tol ) state = 'LOWER'
        IF ( ABS( rowvalue( I ) - rowupper( i ) ) < ac_tol ) state = 'UPPER'
        IF ( ABS( rowlower( i ) - rowupper( i ) ) < eq_tol ) state = 'EQUAL'
        WRITE( out, "( I7, 1X, A10, A6, 4ES12.4 )" ) i, C_names( i ),          &
          state, rowvalue( i ), rowlower( i ), rowupper( i ), rowdual( i )
      END DO
    END DO

!  Compute the number of equality, fixed inequality and degenerate constraints

    mequal = 0 ; mfixed = 0 ; mdegen = 0 ; mredun = 0
    DO i = 1, m
     IF ( ABS( rowlower( i ) - rowupper( i ) ) < eq_tol ) THEN
        mequal = mequal + 1
        IF ( ABS( rowdual( i ) ) < ac_tol ) mredun = mredun + 1
      ELSE IF ( MIN( ABS( rowvalue( i ) - rowlower( i ) ),                     &
                ABS( rowvalue( i ) - rowupper( i ) ) ) <=                      &
           MAX( ac_tol, ABS( rowdual( i ) ) ) ) THEN
        mfixed = mfixed + 1
        IF ( ABS( rowdual( i ) ) < ac_tol ) mdegen = mdegen + 1
      END IF
    END DO
  END IF
  max_d = MAX( MAXVAL( ABS( rowdual( : m ) ) ),                                &
               MAXVAL( ABS( coldual( : n ) ) ) )
  WRITE( out, "( /, ' Of the ', I0, ' variables, ', I0,                        &
    ' are on bounds & ', I0, ' are dual degenerate' )" ) n, nfixed, ndegen
  IF ( m > 0 ) THEN
    WRITE( out, "( ' Of the ', I0, ' constraints, ', I0,                       &
   &  ' are equations, & ', I0, ' are redundant' )" ) m, mequal, mredun
     IF ( m /= mequal ) WRITE( out, "( ' Of the ', I0, ' inequalities, ',      &
   &  I0, ' are on bounds, & ', I0, ' are degenerate' )" ) m - mequal,         &
      mfixed, mdegen
  END IF
  WRITE( out, "( /, ' Final objective function value  ', ES22.14, /,           &
 &                  ' Maximum dual variable           ', ES22.14, /,           &
 &                  ' Maximum constraint violation    ', ES22.14, /,           &
 &                  ' Maximum dual infeasibility      ', ES22.14, /,           &
 &                  ' Number of HiGHS iterations = ', I0 )" )                  &
    obj, max_d, res_p, res_d, iter

!  destroy the HiGHS environment

  CALL Highs_destroy( highs )

!  deallocate workspace

  DEALLOCATE( colcost, collower, colupper, rowlower, rowupper, astart,         &
              aindex, avalue, sol, colvalue, coldual, rowvalue, rowdual,       &
              colbasisstatus, rowbasisstatus, STAT = status )
  DEALLOCATE( X_names, C_names, STAT = status )
  IF ( qp ) DEALLOCATE( qstart, qindex, qvalue, STAT = status )

  STOP

  910 CONTINUE
  WRITE( out, "( ' build of QP failed' )" )
  WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )" ) status
  STOP

  990 CONTINUE
  WRITE( out, "( ' Allocation error, status = ', i0 )" ) status
  STOP

  END PROGRAM HiGHS_main

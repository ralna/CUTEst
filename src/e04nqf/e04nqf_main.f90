!     ( Last modified on 10 Mar 2021 at 14:50:00 )

  PROGRAM E04NQF_main

!  main program for the NAG convex qp package E04NQF

!  Nick Gould, March 2021

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
  INTEGER, PARAMETER :: len_c_w = 600
  INTEGER, PARAMETER :: len_r_w = 600
  INTEGER, PARAMETER :: len_i_w = 600

!  local variables

  INTEGER :: status, n, m, nea, neh, nname, lenc, ncolh, iobj, ns, ninf, ifail
  REAL ( KIND = wp ) :: f, sinf, obj, TIMES( 4 ), CALLS( 7 )
  CHARACTER ( LEN = 1 ) :: start, c_dummy( 1 )
  CHARACTER ( LEN = 8 ) :: prob
  CHARACTER ( LEN = 10 ) :: p_name
  INTEGER, ALLOCATABLE, DIMENSION( : ) :: HELAST, HS, I_w, I_user
  INTEGER, ALLOCATABLE, DIMENSION( : ) :: A_ptr, A_row, H_ptr, H_row
  REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: G, X_0, X, X_l, X_u
  REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: Z, Y, C_l, C_u
  REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: B_l, B_u, R_w, R_user
  REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: A_val, H_val
  CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: X_names, C_names
  CHARACTER ( LEN = 8 ), ALLOCATABLE, DIMENSION( : )  :: C_w, C_user
  EXTERNAL :: E04NQF_QPHX

  OPEN( input, file = 'OUTSDIF.d', form = 'FORMATTED', status = 'OLD' )
  REWIND( input )

!  build the QP using column storage

  CALL CUTEST_LQP_create( status, input, io_buffer, out, n, m, f, G, X_0,      &
                          X_l, X_u, Z, Y, C_l, C_u, p_name, X_names, C_names,  &
                          A_row = A_row, A_ptr = A_ptr, A_val = A_val,         &
                          H_row = H_row, H_ptr = H_ptr, H_val = H_val,         &
                          dont_terminate_cutest = .TRUE. )

!  check that the build succeeded

  IF ( status /= 0 ) GO TO 910
  CLOSE( input )

!  check that there are constraints

  IF ( m <= 0 ) GO TO 920

!  transfer data into the format required by E04NQF

  start = 'C'
  nname = 1
  lenc = n
  ncolh = n
  iobj = 0
  ns = 0
  prob = p_name( 1 : 8 )

!  manipulate vectors so that they conform to E04NQF's structures

  nea = A_ptr( n + 1 ) - 1
  neh = H_ptr( n + 1 ) - 1
  DEALLOCATE( X_names, C_names, Z, STAT = status )
  IF ( status /= 0 ) GO TO 990
  ALLOCATE( B_l( n + m ), B_u( n + m ), STAT = status )
  IF ( status /= 0 ) GO TO 990
  B_l( : n ) = X_l( : n ) ; B_l( n + 1 : n + m ) = C_l( : m )
  B_u( : n ) = X_u( : n ) ; B_u( n + 1 : n + m ) = C_u( : m )
  DEALLOCATE( X_l, X_u, C_l, C_u, STAT = status )
  IF ( status /= 0 ) GO TO 990
  ALLOCATE( HELAST( n + m ), HS( n + m ), X( n + m ), Z( n + m ),              &
            STAT = status )
  IF ( status /= 0 ) GO TO 990
  HELAST( : n + m ) = 3 ; HS( : n + m ) = 0
  X( : n ) = X_0( : n ) ; X( n + 1 : n + m ) = 0.0_wp
  DEALLOCATE( X_0, STAT = status )

!  record the sparse matrix H in E04NQF's user data structure

  ALLOCATE( C_w( len_c_w ), R_w( len_r_w ), I_w( len_i_w ) , STAT = status )
  IF ( status /= 0 ) GO TO 990
  ALLOCATE( C_user( 1 ), R_user( neh ), STAT = status )
  IF ( status /= 0 ) GO TO 990
  R_user( : neh ) = H_val( : neh )
  DEALLOCATE( H_val, STAT = status )
  IF ( status /= 0 ) GO TO 990
  ALLOCATE( I_user( neh + n + 1 ), STAT = status )
  IF ( status /= 0 ) GO TO 990
  I_user( : n + 1 ) = H_ptr( : n + 1 )
  I_user( n + 2 : neh + n + 1 ) = H_row( : neh )
  DEALLOCATE( H_ptr, H_row, STAT = status )
  IF ( status /= 0 ) GO TO 990

!  set up the internal structures

  ifail = - 1
  CALL E04NPF( C_w, len_c_w, I_w, len_i_w, R_w, len_r_w, ifail )
  IF ( ifail /= 0 ) GO TO 910

!  read options file

  OPEN( spec, file = 'E04NQF.SPC', form = 'FORMATTED', status = 'OLD' )
  REWIND( spec )
  ifail = - 1
  CALL E04NRF( spec, C_w, I_w, R_w, ifail )
  IF ( ifail /= 0 ) GO TO 910
  CLOSE( spec )

!  solve the problem

  ifail = - 1
  CALL E04NQF( start, E04NQF_qphx, m, n, nea, nname, lenc, ncolh, iobj, f,     &
               prob, A_val, A_row, A_ptr, B_l, B_u, G, c_dummy, HELAST, HS,    &
               X, Y, Z, ns, ninf, sinf, obj, C_w, len_c_w, I_w, len_i_w,       &
               R_w, len_r_w, C_user, I_user, R_user, ifail )

!  write details

! WRITE( out, "(' Final objective value = ', ES11.3 )" ) obj
! WRITE( out, "(' Optimal X = ', 7F9.2 )" ) X( : n )

  CALL CUTEST_creport( status, CALLS, TIMES )
  WRITE( out, "( /, 24('*'), ' CUTEst statistics ', 24('*') //                 &
 &              ,' Package used            :  E04NQF',    /                    &
 &              ,' Problem                 :  ', A10,    /                     &
 &              ,' # variables             =      ', I10 /                     &
 &              ,' # constraints           =      ', I10 /                     &
 &              ,' Exit code               =      ', I10 /                     &
 &              ,' Final f                 = ', E15.7 /                        &
 &              ,' Set up time             =      ', 0P, F10.2, ' seconds' /   &
 &              ,' Solve time              =      ', 0P, F10.2, ' seconds' //  &
 &               66('*') / )" ) p_name, n, m, ifail, obj, TIMES( 1 ), TIMES( 2 )

!  deallocate workspace

  DEALLOCATE( A_val, A_row, A_ptr, B_l, B_u, G, HELAST, HS, X, Y, Z,           &
               C_w, I_w, R_w, C_user, I_user, R_user, STAT = status )

  STOP

  910 CONTINUE
  WRITE( out, "( ' build of QP failed' )" )
  WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") status
  STOP

  920 CONTINUE
  WRITE( out, "( ' Unfortunately E04NQF requires at least 1 constraint' )" )
  STOP

  990 CONTINUE
  WRITE( out, "( ' Allocation error, status = ', i0 )" ) status
  STOP

  END PROGRAM E04NQF_main

  SUBROUTINE E04NQF_QPHX( ncolh, X, HX, nstate, C_user, I_user, R_user )

!  given x, compute hx = H*x

!  dummy arguments

  INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )
  INTEGER, INTENT ( IN ) :: ncolh, nstate
  INTEGER, INTENT ( INOUT ) :: I_user( * )
  REAL ( KIND = wp ), INTENT( IN ) :: X( ncolh )
  REAL ( KIND = wp ), INTENT( INOUT ) :: R_user( * )
  REAL ( KIND = wp ), INTENT( OUT ) :: HX( ncolh )
  CHARACTER ( len = 8 ), INTENT( INOUT ) :: C_user( * )

!  local variables

  INTEGER :: i, j, l, n_row

!  initialize 

  n_row = I_user( ncolh + 1 )
  HX = 0.0_wp

!  loop over the columns of H, remembering that only one triangle of H is stored

  DO j = 1, ncolh
    DO l = I_user( j ), I_user( j + 1 ) - 1 
      i = I_user( n_row + l )
      HX( i ) = HX( i ) + R_user( l ) * X( j )
      IF ( i /= j ) HX( j ) = HX( j ) + R_user( l ) * X( i )
    END DO
  END DO

  RETURN

!  end of subroutine E04NQF_QPHX

  END SUBROUTINE E04NQF_QPHX

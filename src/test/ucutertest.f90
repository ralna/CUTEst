! THIS VERSION: CUTEST 1.0 - 17/11/2012 AT 10:00 GMT.

!- C U T E S T  t e s t _ u n c o n s t r a i n e d _ t o o l s  P R O G R A M -

    PROGRAM CUTER_test_unconstrained_tools

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released November 2012

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

      INTEGER :: i, n, HE_nel, HE_val_ne, HE_row_ne, alloc_stat
      INTEGER :: l_h2_1, l_h, lhe_ptr, H_ne, lhe_val, lhe_row
      REAL ( KIND = wp ) :: f
      LOGICAL :: grad, byrows, goth
      CHARACTER ( len = 10 ) ::  p_name
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: X_type, H_row, H_col
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: HE_row, HE_row_ptr, HE_val_ptr
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: X, X_l, X_u, G
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: H_val, HE_val
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: VECTOR, RESULT
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : , : ) :: H2_val, H_band
      CHARACTER ( len = 10 ), ALLOCATABLE, DIMENSION( : ) :: X_names
      REAL ( KIND = wp ) :: CPU( 4 ), CALLS( 4 )

!  open the problem data file

      OPEN ( input, FILE = 'u_OUTSDIF.d', FORM = 'FORMATTED', STATUS = 'OLD' )

!  allocate basic arrays

      WRITE( out, "( ' Call UDIMEN ' )" )
      CALL UDIMEN( input, n )
      WRITE( out, "( ' * n = ', I0 )" ) n
      l_h2_1 = n
      ALLOCATE( X( n ), X_l( n ), X_u( n ), G( n ), VECTOR( n ), RESULT( n ),  &
                X_names( n ), X_type( n ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990
      ALLOCATE( H2_val( l_h2_1, n ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

!  set up SIF data

      WRITE( out, "( ' Call USETUP ' )" )
      CALL USETUP( input, out, n, X, X_l, X_u, n )
      CALL WRITE_X( out, n, X, X_l, X_u )

!  obtain variable and problem names

      WRITE( out, "( ' Call UNAMES' )" )
      CALL UNAMES( n, p_name, X_names )
      CALL WRITE_p_name( out, p_name )
      CALL WRITE_X_names( out, n, X_names )

!  obtain problem name

      WRITE( out, "( ' Call PBNAME' )" )
      CALL PBNAME( n, p_name )
      CALL WRITE_p_name( out, p_name )

!  obtain variable names

      WRITE( out, "( ' Call VARNAMES' )" )
      CALL VARNAMES( n, X_names )
      CALL WRITE_X_names( out, n, X_names )

!  obtain variable types

      WRITE( out, "( ' Call UVARTY' )" )
      CALL UVARTY( n, X_type )
      CALL WRITE_X_type( out, n, X_type )

!  compute the objective function value

      WRITE( out, "( ' Call UFN' )" )
      CALL UFN( n, X, f )
      CALL WRITE_f( out, f )

!  compute the gradient value

      WRITE( out, "( ' Call UGR' )" )
      CALL UGR( n, X, G )
      CALL WRITE_G( out, n, G )

!  compute the objective function and gradient values

      grad = .TRUE.
      WRITE( out, "( ' Call UOFG with grad = .TRUE.' )" )
      CALL UOFG( n, X, f, G, grad )
      CALL WRITE_f( out, f )
      CALL WRITE_G( out, n, G )

      grad = .FALSE.
      WRITE( out, "( ' Call UOFG with grad = .FALSE.' )" )
      CALL UOFG( n, X, f, G, grad )
      CALL WRITE_f( out, f )

!  compute the dense Hessian value

      WRITE( out, "( ' Call UDH' )" )
      CALL UDH( n, X, l_h2_1, H2_val )
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )

!  compute the gradient and dense Hessian values

      WRITE( out, "( ' Call UGRDH' )" )
      CALL UGRDH( n, X, G, l_h2_1, H2_val )
      CALL WRITE_G( out, n, G )
      CALL WRITE_H_dense( out, n, l_h2_1, H2_val )

!  compute the number of nonzeros in the sparse Hessian

      WRITE( out, "( ' Call UDIMSH' )" )
      CALL UDIMSH( H_ne )
      WRITE( out, "( ' * H_ne = ', I0 )" ) H_ne

      l_h = H_ne
      ALLOCATE( H_val( l_h ), H_row( l_h ), H_col( l_h ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

!  compute the sparse Hessian value

      WRITE( out, "( ' Call USH' )" )
      CALL USH( n, X, H_ne, l_h, H_val, H_row, H_col )
      CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )

!  compute the gradient and sparse Hessian values

      WRITE( out, "( ' Call UGRSH' )" )
      CALL UGRSH( n, X, G, H_ne, l_h, H_val, H_row, H_col )
      CALL WRITE_G( out, n, G )
      CALL WRITE_H_sparse( out, H_ne, l_h, H_val, H_row, H_col )

!  compute the number of nonzeros in the element Hessian

      WRITE( out, "( ' Call UDIMSE' )" )
      CALL UDIMSE( HE_nel, HE_val_ne, HE_row_ne )
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
      WRITE( out, "( ' Call UEH with byrows = .FALSE.' )" )
      CALL UEH( n, X, HE_nel, HE_row, lhe_row, lhe_ptr, HE_row_ptr,            &
                HE_val, lhe_val, HE_val_ptr, byrows )
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )
      byrows = .TRUE.
      WRITE( out, "( ' Call UEH with byrows = .TRUE.' )" )
      CALL UEH( n, X, HE_nel, HE_row, lhe_row, lhe_ptr, HE_row_ptr,            &
                HE_val, lhe_val, HE_val_ptr, byrows )
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )

!  compute the gradient and element Hessian values

      byrows = .FALSE.
      WRITE( out, "( ' Call UGREH with byrows = .FALSE' )" )
      CALL UGREH( n, X, G, HE_nel, HE_row, lhe_row, lhe_ptr, HE_row_ptr,       &
                  HE_val, lhe_val, HE_val_ptr, byrows )
      CALL WRITE_G( out, n, G )
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )
      byrows = .TRUE.
      WRITE( out, "( ' Call UGREH with byrows = .TRUE.' )" )
      CALL UGREH( n, X, G, HE_nel, HE_row, lhe_row, lhe_ptr, HE_row_ptr,       &
                  HE_val, lhe_val, HE_val_ptr, byrows )
      CALL WRITE_G( out, n, G )
      CALL WRITE_H_element( out, HE_nel, lhe_ptr, HE_row_ptr,                  &
                            HE_val_ptr, lhe_row, HE_row, lhe_val, HE_val )

!  compute a Hessian-vector product

      VECTOR = one
      goth = .FALSE.
      WRITE( out, "( ' Call UPROD with goth = .FALSE.' )" )
      CALL UPROD( n, goth, X, VECTOR, RESULT )
      CALL WRITE_RESULT( out, n, VECTOR, RESULT )
      goth = .TRUE.
      WRITE( out, "( ' Call UPROD with goth = .TRUE.' )" )
      CALL UPROD( n, goth, X, VECTOR, RESULT )
      CALL WRITE_RESULT( out, n, VECTOR, RESULT )

!  compute a band of the Hessian

      nsemib = n / 2
      lbandh = nsemib
      ALLOCATE( H_band( 0 : lbandh, n ), stat = alloc_stat )
      IF ( alloc_stat /= 0 ) GO TO 990

      goth = .FALSE.
      WRITE( out, "( ' Call UBANDH with goth = .FALSE.' )" )
      CALL UBANDH( n, goth, X, nsemib, H_band, lbandh )
      CALL WRITE_H_BAND( out, n, lbandh, H_band, nsemib )
      goth = .TRUE.
      WRITE( out, "( ' Call UBANDH with goth = .TRUE.' )" )
      CALL UBANDH( n, goth, X, nsemib, H_band, lbandh )
      CALL WRITE_H_BAND( out, n, lbandh, H_band, nsemib )

!  calls and time report

      WRITE( out, "( ' CALL UREPRT' )" )
      CALL UREPRT( CALLS, CPU )
      WRITE( out, "( ' CALLS(1-4) =', 4( 1X, I0 ) )" ) INT( CALLS( 1 : 4 ) )
      WRITE( out, "( ' CPU(1-4) =', 4F7.2 )" ) CPU( 1 : 4 )


!  terminal exit

      DEALLOCATE( X_type, H_row, H_col, HE_row, HE_row_ptr, HE_val_ptr, X,     &
                  X_l, X_u, G, H_val, HE_val, VECTOR, RESULT, H2_val, H_band,  &
                  X_names, stat = alloc_stat )
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

      SUBROUTINE WRITE_f( out, f )
      INTEGER :: out
      REAL ( KIND = wp ) :: f
      WRITE( out, "( ' * f = ', ES12.4 )" ) f
      END SUBROUTINE WRITE_f

      SUBROUTINE WRITE_G( out, n, G )
      INTEGER :: n, out
      REAL ( KIND = wp ), DIMENSION( n ) :: G
      INTEGER :: i
      WRITE( out, "( ' *       i       G' )" )
      DO i = 1, n
        WRITE( out, "( ' * ', I7, ES12.4 )" ) i, G( i )
      END DO
      END SUBROUTINE WRITE_G

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

      SUBROUTINE WRITE_H_BAND( out, n, lbandh, H_band, nsemib )
      INTEGER :: n, lbandh, out
      REAL ( KIND = wp ), DIMENSION( 0 : lbandh, n ):: H_band
      INTEGER :: i, j
      WRITE( out, "( ' * H(band)' )" )
      WRITE( out, "( ' *       i   band', I7, 4I12 )" ) ( j, j = 0, nsemib )
      DO i = 1, n
        WRITE( out, "( ' * ', I7, 6X, 5ES12.4 )" )                             &
          i, ( H_band( j, i ), j = 0, nsemib )
      END DO
      END SUBROUTINE WRITE_H_BAND

    END PROGRAM CUTER_test_unconstrained_tools

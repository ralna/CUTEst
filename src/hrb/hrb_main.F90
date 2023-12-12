! THIS VERSION: CUTEST 2.2 - 2023-11-23 AT 09:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

    PROGRAM HRB_main

      USE CUTEST_KINDS_precision

      IMPLICIT NONE

!  ----------------------------------------------------------------

!  Write out SIF data in Harwell-Boeing or Rutherford-Boeing Format

!  Nick Gould, October 1996
!  CUTEst evolution, January 2013

!  ----------------------------------------------------------------

!  scalar arguments

      INTEGER ( KIND = ip_ ) :: n, m, matmax, i, j, k, ntotal, plast, status
      INTEGER ( KIND = ip_ ) :: na, ne, nh, nj, nv, nrow, ncol, nnz, colmax
      INTEGER ( KIND = ip_ ), PARAMETER :: in = 5, out = 6, input = 55
      INTEGER ( KIND = ip_ ), PARAMETER :: output = 56, outrhs = 57
      INTEGER ( KIND = ip_ ), PARAMETER :: io_buffer = 11
      REAL ( KIND = rp_ ) :: f
      REAL ( KIND = rp_ ), PARAMETER :: one = 1.0_rp_, zero = 0.0_rp_
      REAL ( KIND = rp_ ), PARAMETER :: biginf = 10.0_rp_ ** 19
      REAL ( KIND = rp_ ), PARAMETER :: penalty = 0.1_rp_
      LOGICAL :: hb, rb
      CHARACTER ( LEN = 1 ) :: matype, maform, hrb
      CHARACTER ( LEN = 10 ) :: pname
      CHARACTER ( LEN = 12 ) :: prbout
      CHARACTER ( LEN = 17 ) :: prbrhs
      CHARACTER ( LEN = 80 ) :: line1
      CHARACTER ( LEN = 70 ) :: line2, line3
      CHARACTER ( LEN = 72 ) :: line4
      CHARACTER ( LEN = 42 ) :: line5
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ROW, COL, IP, IW
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, BL, BU, VAL
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: B, C, Y, CL, CU
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: SLACK
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: VNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: GNAMES
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATN, LINEAR

!  Open the relevant file

      OPEN( input , FILE = 'OUTSDIF.d', FORM = 'FORMATTED', STATUS = 'OLD'  )
      REWIND( input )

!  compute problem dimensions

      CALL CUTEST_cdimen_r( status, input, n, m )
      IF ( status /= 0 ) GO TO 910

!  allocate space 

      colmax = n + m + 1
      ALLOCATE( IP( colmax + 1 ), IW( colmax + 1 ), X( n ), BL( n ), BU( n ),  &
                CL( m ), CU( m ), Y( m ), B( m ), SLACK( m ),                  &
                EQUATN( m ), LINEAR( m ), VNAMES( n ), GNAMES( m ),            &
                STAT = status )
      IF ( status /= 0 ) GO TO 990

!  Set up the data structures necessary to hold the group partially
!  separable function

      CALL CUTEST_csetup_r( status, input, out, io_buffer, N, M, X, BL, BU, Y, &
                            CL, CU, EQUATN, LINEAR, 0, 0, 0 )
      IF ( status /= 0 ) GO TO 910

!  compute the numbers of nonzeros in the problem Jacobian and Hessian

      CALL CUTEST_cdimsj_r( status, nj )
      IF ( status /= 0 ) GO TO 910
      CALL CUTEST_cdimsh_r( status, nh )
      IF ( status /= 0 ) GO TO 910

!  allocate more space 

      matmax = nj + nh + m
      ALLOCATE( ROW( matmax ), COL( matmax ), VAL( MATMAX ), STAT = status )
      IF ( status /= 0 ) GO TO 990

!  Determine whether Harwell-Boeing or Rutherford-Boeing format is required

   10 CONTINUE
      WRITE( out, 2050 )
      READ( in, 1000 ) hrb
      IF ( hrb == 'h' ) hrb = 'H'
      IF ( hrb == 'r' ) hrb = 'R'
      IF ( hrb /= 'H' .AND. hrb /= 'R' ) THEN
         WRITE( out, 2060 ) hrb
         GO TO 10
      END IF
      hb = hrb == 'H'
      rb = .NOT. hb

!  Determine required matrix type

      IF ( m > 0 ) THEN
   20   CONTINUE
        WRITE( out, 2010 )
        READ( in, 1000 ) matype
        IF ( matype == 'a' ) matype = 'A'
        IF ( matype == 'h' ) matype = 'H'
        IF ( matype == 'j' ) matype = 'J'
        IF ( matype == 't' ) matype = 'T'
        IF ( matype /= 'A' .AND. matype /= 'H' .AND.                           &
             matype /= 'J' .AND. matype /= 'T' ) THEN
           WRITE( out, 2020 ) matype
           GO TO 20
        END IF
        maform = 'A'
      ELSE
        matype = 'H'
      END IF

      IF ( matype == 'H' ) THEN

!  Determine required matrix format

   30   CONTINUE
        WRITE( out, 2030 )
        READ( in, 1000 ) maform
        IF ( maform == 'a' ) maform = 'A'
        IF ( maform == 'e' ) maform = 'E'
        IF ( maform /= 'A' .AND. maform /= 'E' ) THEN
          WRITE( out, 2040 ) maform
          GO TO 30
        END IF
      END IF

!  Determine the names of the problem, variables and constraints

      CALL CUTEST_probname_r( status, pname )
      IF ( status /= 0 ) GO TO 910
      DO plast = 8, 1, - 1
        IF ( pname( plast : plast ) /= ' ' ) EXIT
      END DO

!  Name output file

      prbout = '            '
      IF ( matype == 'H' ) THEN
        IF ( maform == 'A' ) THEN
          prbout = pname( 1 : plast ) // '.hes'
        ELSE
          prbout = pname( 1 : plast ) // '.ele'
        END IF
      ELSE IF ( matype == 'A' ) THEN
        prbout = pname( 1 : plast ) // '.aug'
      ELSE IF ( matype == 'J' ) THEN
        prbout = pname( 1 : plast ) // '.jac'
      ELSE
        prbout = pname( 1 : plast ) // '.jat'
      END IF

!  Open output file

      OPEN( output , FILE = PRBOUT, FORM = 'FORMATTED', STATUS = 'UNKNOWN'  )
      REWIND( output )

!  For RB format, the RHS is stored separately

      IF ( rb ) THEN
        IF ( matype == 'H' ) THEN
          IF ( maform == 'A' ) THEN
            prbrhs = pname( 1 : plast ) // '.hes.rhsr'
          ELSE
            prbrhs = pname( 1 : plast ) // '.ele.rhsr'
          END IF
        ELSE IF ( matype == 'A' ) THEN
          prbrhs = pname( 1 : plast ) // '.aug.rhsr'
        ELSE IF ( matype == 'J' ) THEN
          prbrhs = pname( 1 : plast ) // '.jac.rhsr'
        ELSE
          prbrhs = pname( 1 : plast ) // '.jat.rhsr'
        END IF

!  Open output file for RHS

        OPEN( outrhs , FILE = PRBRHS, FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
        REWIND( outrhs )

      END IF

!  Move X into the feasible region

      DO i = 1, n
        X( i ) = MIN( BU( i ), MAX( BL( i ), X( i ) ) )
      END DO

!  Evaluate the constant terms of the objective and constraint functions

      CALL CUTEST_cfn_r( status, n, m, X, f, B )
      IF ( status /= 0 ) GO TO 910
      B( 1 : m ) = - B( 1 : m )

!  Use IW to store positions of diagonal entries. Initialize IW as 0

      IW( 1 : n + m ) = 0

!  Evaluate the linear terms of the objective and constraint functions
!  in a sparse format

      CALL CUTEST_csgr_r( status, n, m, X, Y, .FALSE., nj, matmax,             &
                          VAL, COL, ROW )
      IF ( status /= 0 ) GO TO 910
      SLACK( 1 : m ) = ONE

      ntotal = n
      DO i = 1, m
        IF ( .NOT. EQUATN( i ) ) ntotal = ntotal + 1
      END DO

      ALLOCATE( C( ntotal ), STAT = status )
      IF ( status /= 0 ) GO TO 990

      C = zero

!  Remove zero entries

      na = 0
      DO i = 1, nj
        IF ( VAL( i ) /= zero ) THEN
          IF ( ROW( i ) > 0 ) THEN
            na = na + 1
            j = ROW( i )  
            ROW( na ) = J
            COL( na ) = COL( i )  
            VAL( na ) = VAL( i )
            SLACK( j ) = MAX( SLACK( j ), ABS( VAL( na ) ) )
          ELSE
            C( COL( i ) ) = - VAL( i )
          END IF  
        END IF
      END DO

!  Determine the status of each slack variable.
!  Introduce slack variables for inequality constraints

      ntotal = n
      DO i = 1, m
        IF ( .NOT. EQUATN( i ) ) THEN
          ntotal = ntotal + 1
          na = na + 1 ; ROW( na ) = i ; COL( na ) = ntotal 
          IF ( ( CL( i ) == zero .AND. CU( i ) > biginf ) .OR.                 &
               ( CU( i ) == zero .AND. CL( i ) < - biginf ) ) THEN
            IF ( CL( i ) == zero ) THEN
              VAL( na ) = - SLACK( i )
            ELSE
              VAL( na ) = SLACK( i )
            END IF
          ELSE
            VAL( na ) = - SLACK( i )
          END IF
!  remove
!         VAL( na ) = - one
        END IF
      END DO

!  remove
!     C = C - one

      IF ( matype == 'H' ) THEN
        nnz = 0
      ELSE
        nnz = na
      END IF

      IF ( matype == 'A' .OR. matype == 'H' ) THEN

!  The matrix is to be assembled

        IF ( maform == 'A' ) THEN

!  Evaluate the Hessian of the Lagrangian function at the initial point

          CALL CUTEST_csh_r( status, n, m, X, Y, nh, matmax - na,              &
                             VAL( na + 1 ), ROW( na + 1 ), COL( na + 1 ) )
          IF ( status /= 0 ) GO TO 910

!  Remove zero entries

          DO i = na + 1, na + nh
            IF ( VAL( i ) /= zero ) THEN
              nnz = nnz + 1
              IF ( ROW( i ) >= COL( i ) ) THEN
                ROW( nnz ) = ROW( i )  
                COL( nnz ) = COL( i )  
              ELSE
                ROW( nnz ) = COL( i )  
                COL( nnz ) = ROW( i )  
              END IF  
              IF ( ROW( i ) == COL( i ) ) IW( ROW( i ) ) = nnz
              VAL( nnz ) = VAL( i )
            END IF
          END DO

!  Include terms representing penalties

          DO i = 1, n
            IF ( BL( i ) > - biginf .OR. BU( i ) < biginf ) THEN
              j = IW( i )
              IF ( j == 0 ) THEN
                nnz = nnz + 1 ; ROW( nnz ) = i ; COL( nnz ) = i
                IF ( BL( i ) > - biginf .AND. BU( i ) < biginf ) THEN
                  VAL( nnz ) = penalty + penalty
                ELSE
                  VAL( nnz ) = penalty
                END IF
              ELSE
                IF ( BL( i ) > - biginf .AND. BU( i ) < biginf ) THEN
                  VAL( j ) = VAL( j ) + penalty + penalty
                ELSE
                  VAL( j ) = VAL( j ) + penalty
                END IF
              END IF
            END IF 
          END DO

          ntotal = n
          DO i = 1, m
            IF ( .NOT. EQUATN( i ) ) THEN
              ntotal = ntotal + 1
              nnz = nnz + 1
              ROW( nnz ) = ntotal
              COL( nnz ) = ntotal 
              IF ( CL( i ) > - biginf .AND. CU( i ) < biginf ) THEN
                 VAL( nnz ) = penalty + penalty
              ELSE
                 VAL( nnz ) = penalty
              END IF
            END IF
          END DO

!  The matrix is unassembled

        ELSE

!  Evaluate the Hessian of the Lagrangian function at the initial point

          CALL CUTEST_ceh_r( status, n, m, X, Y, ne, colmax, IP, COL,          &
                             matmax, ROW, matmax, VAL, .TRUE. )
          IF ( status /= 0 ) GO TO 910

!  Include terms representing penalties

          nv = COL( ne + 1 ) - 1
          DO i = 1, n
            IF ( BL( i ) > - biginf .OR. BU( i ) < biginf ) THEN
              ne = ne + 1
              nv = nv + 1
              ROW( IP( ne ) ) = i
              IP( ne + 1 ) = IP( ne ) + 1
              IF ( BL( i ) > - biginf .AND. BU( i ) < biginf ) THEN
                 VAL( nv ) = penalty + penalty
              ELSE
                 VAL( nv ) = penalty
              END IF
            END IF
          END DO
          nnz = IP( ne + 1 ) - 1
        END IF
      END IF

      n = ntotal
      WRITE( 6, 2000 ) pname, n, m

      LINE1( 1 : 10 ) = pname
      DO i = 11, 80, 10
        LINE1( i : i + 9 ) = '          '
      END DO
      LINE1( 73 : 80 ) = pname( 1 : 8 )

!  Format header cards

      IF ( matype == 'A' ) THEN
        nrow = n + m ; ncol = nrow
        LINE1( 12 : 30 ) = 'augmented system - '
        WRITE( UNIT = LINE1( 31: 38 ), FMT = "( I8 )" ) nrow
        LINE1( 39 : 43 ) = ' rows'
        ROW( 1 : na ) =  ROW( 1 : na ) + n 
      ELSE IF ( matype == 'H' ) THEN
        nrow = n ; ncol = nrow
        LINE1( 12 : 21 ) = 'Hessian - '
        WRITE( UNIT = LINE1( 22: 29 ), FMT = "( I8 )" ) nrow
        LINE1( 30 : 34 ) = ' rows'
        IF ( maform == 'E' ) LINE1( 35 : 51 ) = ' - element format'
      ELSE IF ( matype == 'J' ) THEN
        nrow = m ; ncol = n
        LINE1( 12 : 22 ) = 'Jacobian - '
        WRITE( UNIT = LINE1( 23: 30 ), FMT = "( I8 )" ) nrow
        LINE1( 31 : 35 ) = ' rows'
        WRITE( UNIT = LINE1( 36: 43 ), FMT = "( I8 )" ) ncol
        LINE1( 44 : 51 ) = ' columns'
      ELSE IF ( matype == 'T' ) THEN
        nrow = n ; ncol = m
        LINE1( 12 : 32 ) = 'Jacobian transpose - '
        WRITE( UNIT = LINE1( 33: 40 ), FMT = "( I8 )" ) nrow
        LINE1( 41 : 45 ) = ' rows'
        WRITE( UNIT = LINE1( 46: 53 ), FMT = "( I8 )" ) ncol
        LINE1( 54 : 61 ) = ' columns'
        DO i = 1, na
          j = COL( i )
          COL( i ) =  ROW( i )
          ROW( i ) = j
        END DO
      END IF

!  Transform from co-ordinate to column format

      IF ( maform == 'A' ) CALL REORDER( ncol, nnz, ROW, COL, VAL, IP, IW )

      IF ( .FALSE. ) THEN
        IF ( matype == 'J' ) THEN
          write(74,"( ' J' )" )
          DO j = 1, ncol
            DO k = IP( j ), IP( j + 1 ) - 1
              write(74,"( 2I8, ES12.4 )" ) row( k ), j, val( k )
            END DO
          END DO
        ELSE IF ( matype == 'T' ) THEN
          write(75,"( ' JT' )" )
          DO j = 1, ncol
            DO k = IP( j ), IP( j + 1 ) - 1
              write(75,"( 2I8, ES12.4 )" ) j, row( k ), val( k )
            END DO
          END DO
        END IF 
      END IF 

!  Harwell-Boeing format

      IF ( hb ) THEN

!  Continue formatting header cards

        IF ( maform == 'A' ) THEN
          i = ( ncol + 10 ) / 10 + ( nnz + 9 ) / 10 +                          &
              ( nnz + 2 ) / 3  + ( nrow + 2 ) / 3
        ELSE
          i = ( ncol + 10 ) / 10 + ( nnz + 9 ) / 10 +                          &
              ( nv + 2 ) / 3  + ( nrow + 2 ) / 3
        END IF
        WRITE( UNIT = LINE2( 1: 14 ), FMT = "( I14 )" ) i
        i = ( ncol + 10 )/10 
        WRITE( UNIT = LINE2( 15: 28 ), FMT = "( I14 )" ) i
        i = ( nnz + 9 ) / 10
        WRITE( UNIT = LINE2( 29: 42 ), FMT = "( I14 )" ) i
        IF ( maform == 'A' ) THEN
          i = ( nnz + 2 ) / 3
        ELSE
          i = ( NV + 2 ) / 3
        END IF
        WRITE( UNIT = LINE2( 43: 56 ), FMT = "( I14 )" ) i
        i = ( nrow + 2 ) / 3
        WRITE( UNIT = LINE2( 57: 70 ), FMT = "( I14 )" ) i
   
        LINE3( 1: 1 ) = 'R'
        IF ( matype == 'A' .OR. matype == 'H' ) THEN
          LINE3( 2: 2 ) = 'S'
        ELSE
          LINE3( 2: 2 ) = 'U'
        END IF
        IF ( maform == 'A' ) THEN
          LINE3( 3: 3 ) = 'A'
        ELSE
          LINE3( 3: 3 ) = 'E'
        END IF
        WRITE( UNIT = LINE3( 4: 14 ), FMT = "( 11X )" ) 
        WRITE( UNIT = LINE3( 15: 28 ), FMT = "( I14 )" ) nrow
        WRITE( UNIT = LINE3( 29 : 42 ), FMT = "( I14 )" ) ncol
        WRITE( UNIT = LINE3( 43 : 56 ), FMT = "( I14 )" ) nnz
        IF ( maform == 'A' ) THEN
          i = 0
        ELSE
          i = nv
        END IF
        WRITE( UNIT = LINE3( 57 : 70 ), FMT = "( I14 )" ) i
   
        WRITE( UNIT = LINE4( 1: 16 ), FMT = "( '(10I8)          ' )" ) 
        WRITE( UNIT = LINE4( 17: 32 ), FMT = "( '(10I8)          ' )" ) 
!S      WRITE( UNIT = LINE4( 33: 52 ), FMT = "( '(1P, 3E24.16)   ' )" ) 
        WRITE( UNIT = LINE4( 33: 52 ), FMT = "( '(1P, 3D24.16)   ' )" ) 
!S      WRITE( UNIT = LINE4( 53: 72 ), FMT = "( '(1P, 3E24.16)   ' )" ) 
        WRITE( UNIT = LINE4( 53: 72 ), FMT = "( '(1P, 3D24.16)   ' )" ) 
   
        LINE5( 1: 3 ) = 'F  '
        WRITE( UNIT = LINE5( 4: 14 ), FMT = "( 11X )" ) 
        i = 1
        WRITE( UNIT = LINE5( 15: 28 ), FMT = "( I14 )" ) I
        WRITE( UNIT = LINE5( 29: 42 ), FMT = "( I14 )" ) nrow

!  Write out the header

        WRITE( output, "( A80, /, A70, /, A70, /, A72, /, A42 )" )             &
               LINE1, LINE2, LINE3, LINE4, LINE5

!  Rutherford-Boeing format

      ELSE

!  Continue formatting header cards

        IF ( maform == 'A' ) THEN
          i = ( ncol + 10 ) / 10 + ( nnz  + 9 ) / 10 + ( nnz  +  2 ) / 3
        ELSE
          i = ( ncol + 10 ) / 10 + ( nnz  + 9 ) / 10 + ( NV   +  2 ) / 3
        END IF
        WRITE( UNIT = LINE2( 1: 14 ), FMT = "( 1X, I13 )" ) I
        i = ( ncol + 10 )/10 
        WRITE( UNIT = LINE2( 15: 28 ), FMT = "( 1X, I13 )" ) I
        i = ( nnz + 9 ) / 10
        WRITE( UNIT = LINE2( 29: 42 ), FMT = "( 1X, I13 )" ) I
        IF ( maform == 'A' ) THEN
          i = ( nnz + 2 ) / 3
        ELSE
          i = ( nv + 2 ) / 3
        END IF
        WRITE( UNIT = LINE2( 43: 56 ), FMT = "( 1X, I13 )" ) i
   
        LINE3( 1: 1 ) = 'r'
        IF ( matype == 'A' .OR. matype == 'H' ) THEN
           LINE3( 2: 2 ) = 's'
        ELSE
           LINE3( 2: 2 ) = 'u'
        END IF
        IF ( maform == 'A' ) THEN
           LINE3( 3: 3 ) = 'a'
        ELSE
           LINE3( 3: 3 ) = 'e'
        END IF
        WRITE( UNIT = LINE3( 4: 14 ), FMT = "( 11X )" ) 
        WRITE( UNIT = LINE3( 15: 28 ), FMT = "( 1X, I13 )" ) nrow
        WRITE( UNIT = LINE3( 29 : 42 ), FMT = "( 1X, I13 )" ) ncol
        WRITE( UNIT = LINE3( 43 : 56 ), FMT = "( 1X, I13 )" ) nnz
        IF ( maform == 'A' ) THEN
          i = 0
        ELSE
          i = nv
        END IF
        WRITE( UNIT = LINE3( 57 : 70 ), FMT = "( 1X, I13 )" ) I
   
        WRITE( UNIT = LINE4( 1: 16 ),  FMT = "( '(10I8)          ' )" ) 
        WRITE( UNIT = LINE4( 17: 32 ), FMT = "( '(10I8)          ' )" ) 
!S      WRITE( UNIT = LINE4( 33: 52 ), FMT = "( '(1P, 3E25.16)   ' )" ) 
        WRITE( UNIT = LINE4( 33: 52 ), FMT = "( '(1P, 3D25.16)   ' )" ) 
   
!  Write out the header

        WRITE( OUTPUT, "( A80, /, A56, /, A70, /, A52 )" )                     &
                 LINE1, LINE2( 1 : 56 ), LINE3, LINE4( 1 : 52 )

!  Do the same for the RHS file

        LINE1( 64 : 72 ) = ' - RHS - '

        WRITE( UNIT = LINE2( 1: 5 ), FMT = "( 'rhsrd' )" )
        IF ( matype == 'A' ) THEN
             WRITE( UNIT = LINE2( 6: 14 ), FMT = "( ' aug sys ' )" )
        ELSE IF ( matype == 'A' ) THEN
             WRITE( UNIT = LINE2( 6: 14 ), FMT = "( ' hessian ' )" )
        ELSE IF ( matype == 'J' ) THEN
             WRITE( UNIT = LINE2( 6: 14 ), FMT = "( ' jacobian' )" )
        ELSE IF ( matype == 'T' ) THEN
             WRITE( UNIT = LINE2( 6: 14 ), FMT = "( ' jactrans' )" )
        END IF

        WRITE( UNIT = LINE2( 15: 16 ), FMT = "( ' r' )" )
        WRITE( UNIT = LINE2( 17: 30 ), FMT = "( 1X, I13 )" ) nrow
        WRITE( UNIT = LINE2( 31: 44 ), FMT = "( 1X, I13 )" ) 1
        WRITE( UNIT = LINE2( 45: 58 ), FMT = "( 1X, I13 )" ) nrow

        WRITE( UNIT = LINE3( 1: 20 ), FMT = "( '(1P, 3D25.16)', 7X )" ) 
        WRITE( UNIT = LINE3( 21: 40 ), FMT = "( 20X )" )
        WRITE( UNIT = LINE3( 41: 60 ), FMT = "( 20X )" )

!  Write out the RHS header

        WRITE( OUTRHS, "( A80, /, A58, /, A60 )" )                             &
               LINE1, LINE2( 1 : 58 ), LINE3( 1 : 60 )
      END IF

!  Write out the desired matrix

      IF ( hb ) THEN
        WRITE( OUTPUT, "( (10I8) )" ) ( IP( i ), i = 1, ncol + 1 )
        WRITE( OUTPUT, "( (10I8) )" ) ( ROW( i ), i = 1, nnz )
        IF ( maform == 'A' ) THEN
!S        WRITE( OUTPUT, "( (1P, 3E24.16) )" ) ( VAL( i ), i = 1, nnz )
          WRITE( OUTPUT, "( (1P, 3D24.16) )" ) ( VAL( i ), i = 1, nnz )
        ELSE
!S        WRITE( OUTPUT, "( (1P, 3E24.16) )" ) ( VAL( i ), i = 1, nv )
          WRITE( OUTPUT, "( (1P, 3D24.16) )" ) ( VAL( i ), i = 1, nv )
        END IF
      ELSE
        WRITE( OUTPUT, "( (10I8) )" ) ( IP( i ), i = 1, ncol + 1 )
        WRITE( OUTPUT, "( (10I8) )" ) ( ROW( i ), i = 1, nnz )
        IF ( maform == 'A' ) THEN
!S        WRITE( OUTPUT, "( (1P, 3E25.16) )" ) ( VAL( i ), i = 1, nnz )
          WRITE( OUTPUT, "( (1P, 3D25.16) )" ) ( VAL( i ), i = 1, nnz )
        ELSE
!S        WRITE( OUTPUT, "( (1P, 3E25.16) )" ) ( VAL( i ), i = 1, nv )
          WRITE( OUTPUT, "( (1P, 3D25.16) )" ) ( VAL( i ), i = 1, nv )
        END IF
      END IF

      IF ( hb ) THEN
        IF ( matype == 'A' ) THEN
!S        WRITE( OUTPUT, "( (1P, 3E24.16) )" ) ( C( i ), i = 1, n ),
          WRITE( OUTPUT, "( (1P, 3D24.16) )" ) ( C( i ), i = 1, n ),           &
                                                ( B( i ), i = 1, m )
        ELSE IF ( matype == 'J' ) THEN
!S        WRITE( OUTPUT, "( (1P, 3E24.16) )" ) ( B( i ), i = 1, m )
          WRITE( OUTPUT, "( (1P, 3D24.16) )" ) ( B( i ), i = 1, m )
        ELSE
!S        WRITE( OUTPUT, "( (1P, 3E24.16) )" ) ( C( i ), i = 1, n )
          WRITE( OUTPUT, "( (1P, 3D24.16) )" ) ( C( i ), i = 1, n )
        END IF
      ELSE
        IF ( matype == 'A' ) THEN
!S        WRITE( OUTRHS, "( (1P, 3E25.16) )" ) ( C( i ), i = 1, n ),
          WRITE( OUTRHS, "( (1P, 3D25.16) )" ) ( C( i ), i = 1, n ),           &
                                               ( B( i ), i = 1, m )
        ELSE IF ( matype == 'J' ) THEN
!S        WRITE( OUTRHS, "( (1P, 3E25.16) )" ) ( B( i ), i = 1, m )
          WRITE( OUTRHS, "( (1P, 3D25.16) )" ) ( B( i ), i = 1, m )
        ELSE
!S        WRITE( OUTRHS, "( (1P, 3E25.16) )" ) ( C( i ), i = 1, n )
          WRITE( OUTRHS, "( (1P, 3D25.16) )" ) ( C( i ), i = 1, n )
        END IF
      END IF

      CLOSE( OUTPUT )
      IF ( rb ) CLOSE( OUTRHS )
      CALL CUTEST_uterminate_r( status )
      STOP

 910 CONTINUE
     WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )" )  status
     STOP

 990 CONTINUE
     WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
     STOP

!  Non-executable statements

 1000 FORMAT( A1 )
 2000 FORMAT( /, ' Problem name: ', A8,                                        &
              /, ' Number of variables     = ', I8,                            &
              /, ' Number of equations     = ', I8 )
 2010 FORMAT( /, ' Please state required matrix type. ',                       &
              /, ' A (augmented system) or H (Hessian) or',                    &
              /, ' J (Jacobian) or T (Transpose of Jacobian) : ' )
 2020 FORMAT( /, ' Matrix type ', A1, ' not recognized. Please try again ' )
 2030 FORMAT( /, ' Please state required matrix format. ',                     &
              /, ' A (assembled) or E (elemental): ' )
 2040 FORMAT( /, ' Matrix format ', A1, ' not recognized. Please try again ' )
 2050 FORMAT( /, ' Please state whether you require'                           &
              /, ' Harwell-Boeing or Rutherford-Boeing format. ', /,           &
                 ' H (Harwell-Boeing) or R (Rutherford-Boeing) : ' )
 2060 FORMAT( /, ' Your response ', A1, ' not recognized. Please try again ' )

    CONTAINS
  
      SUBROUTINE REORDER( nc, nnz, IRN, JCN, A, IP, IW )
      INTEGER ( KIND = ip_ ) :: nc, nnz
      INTEGER ( KIND = ip_ ) :: IRN( nnz ), JCN( nnz )
      INTEGER ( KIND = ip_ ) :: IW( nc + 1 ), IP( nc + 1 )
      REAL ( KIND = rp_ ) :: A( nnz )

!  Sort a sparse matrix from arbitrary order to column order

!  Nick Gould
!  7th November, 1990

      INTEGER :: i, j, k, l, ic, ncp1, itemp, jtemp,  locat
      REAL ( KIND = rp_ ) :: anext , atemp

!  Initialize the workspace as zero

      ncp1 = nc + 1
      IW( 1 : ncp1 ) = 0

!  Pass 1. Count the number of elements in each column

      DO k = 1, nnz
        j  = JCN( k )
        IW( j ) = IW( j ) + 1
      END DO

!  Put the positions where each column begins in
!  a compressed collection into IP and IW

      IP( 1 ) = 1
      DO j = 2, ncp1
        IP( j ) = IW( j - 1 ) + IP( j - 1 )
        IW( j - 1 ) = IP( j - 1 )
      END DO

!  Pass 2. Reorder the elements into column order. 
!          Fill in each column in turn

      DO ic = 1, nc

!  Consider the next unfilled position in column ic

        DO k = IW( ic ), IP( ic + 1 ) - 1

!  The entry should be placed in column j

          i = IRN( K )
          j = JCN( K )
          anext = A( K )

!  See if the entry is already in place

          DO l = 1, nnz
             IF ( j == ic ) EXIT
             locat = IW( j )

!  As a new entry is placed in column J, increase the pointer 
!  IW( j ) by one

             IW( j  ) = locat + 1

!  Record details of the entry which currently occupies location LOCAT

             itemp = IRN( locat )
             jtemp = JCN( locat )
             atemp = A( locat )

!  Move the new entry to its correct place

             IRN( locat ) = i 
             JCN( locat ) = j  
             A( locat ) = anext

!  Make the displaced entry the new entry

             i = itemp
             j = jtemp
             anext = atemp
          END DO

!  Move the new entry to its correct place 

          JCN( k ) = j
          IRN( k ) = i
          A( k ) = anext
        END DO
      END DO

!  Now sort the entries in each column so that their row indices increase

      DO i = 1, nc
        locat = IP( i )
        ic = IP( i + 1 ) - locat
        j = MAX( ic, 1 )
        IF ( ic > 0 ) CALL SORT_ascending( ic, j, IRN( locat ), A( locat ) )
      END DO
      RETURN

!  End of REORDER

      END SUBROUTINE REORDER

      SUBROUTINE SORT_ascending( n, lind, IND, VAL )
      INTEGER ( KIND = ip_ ) :: n, lind
      INTEGER ( KIND = ip_ ) :: IND( lind )
      REAL ( KIND = rp_ ) :: VAL( lind )

!  Sort n numbers into ascending order. Yes, we should use
!  quicksort but ...well, we are hoping that n won't be too big

      INTEGER :: i, j, curmin, indmin
      REAL ( KIND = rp_ ) :: valmin

! Find the I-th smallest value

      DO i = 1, n
         curmin = i
         indmin = IND( curmin )
         DO j = i + 1, n
           IF ( IND( j ) < indmin ) THEN
             curmin = j
             indmin = IND( curmin )
           END IF
         END DO

! If the I-th smallest value is out of place, swap it to poiition CURMIN

         IF ( curmin /= i ) THEN
           valmin = VAL( curmin )
           IND( curmin ) = IND( i )
           VAL( curmin ) = VAL( i )
           IND( i ) = indmin
           VAL( i ) = valmin
         END IF
      END DO
      RETURN

!  End of SORT_ascending

      END SUBROUTINE SORT_ascending

!  End of HRB_main

    END PROGRAM HRB_main

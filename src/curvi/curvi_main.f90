!     ( Last modified on 9 Apr 2019 at 11:00:00 )

      PROGRAM CURVI_main

!  CURVI test driver for problems derived from SIF files.

!  Nick Gould, April 2019

      INTEGER :: n, derivs, maxit, i, status, ibound, lwa, nf, ng, nh, nit
      INTEGER :: idiff, kmax, g
      INTEGER, PARAMETER :: input = 55, out = 6, inspec = 56
      INTEGER, PARAMETER :: io_buffer = 11
      DOUBLE PRECISION :: biginf, eps, fopt
      DOUBLE PRECISION, PARAMETER :: tiny = 1.0D-6
      CHARACTER ( LEN = 10 ) :: pname
      DOUBLE PRECISION :: CPU( 2 ), CALLS( 4 )
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: JBOUND
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: X, BL, BU, WA
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: XNAMES
      EXTERNAL :: CURVI_evalf, CURVI_evalg, CURVI_evalh

!  Open the spec file

      OPEN( inspec, FILE = 'CURVI.SPC', FORM = 'FORMATTED',                    &
            STATUS = 'OLD' )
      REWIND( inspec )

!  read input Spec data

! DERIVS      <--  Derivatives available (<=0 none, 1 1st, >=2 1st and 2nd)
! BIGINF      <--  Bounds larger than biginf in magnitude are infinite
! EPS         <--  Stopping tolerance
! NF          <--  Maximum number of function evaluations (0 -> 1000*n)
! IDIFF       <--  Forward (<=1) or central (>=2) differences
! KMAX        <--  Hessian recomputation interval

!     READ( inspec, "( I10 )" ) derivs
!     READ( inspec, "( E10.3 )" ) biginf
!     READ( inspec, "( E10.3 )" ) eps
!     READ( inspec, "( I10 )" ) nf
!     READ( inspec, "( I10 )" ) idiff
!     READ( inspec, "( I10 )" ) kmax

      READ( inspec, "( I10, /, E10.3, /, E10.3, /, I10, /, I10, /, I10 )" ) &
        derivs, biginf, eps, nf, idiff, kmax
!     READ( inspec, "( I10, /, 2( E10.3, /), (2I10, / ), I10 )" )              &
!       derivs, biginf, eps, nf, idiff, kmax
      idiff = MIN( MAX( 1, idiff ), 2 )
      kmax = MAX( kmax, 1 )

!  close input file

      CLOSE( inspec )

!  open the input data file

      OPEN( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',                     &
            STATUS = 'OLD' )
      REWIND( input )

!  find the problem dimension

      CALL CUTEST_udimen( status, input, n )
      IF ( status /= 0 ) GO TO 910

!  allocate workspace

      lwa = n * ( n + 1 ) / 2
      IF ( derivs <= 0 ) THEN
        lwa = 9 * n + lwa + n * n + max( 7 * n - lwa, 0 )
      ELSE IF ( derivs == 1 ) THEN
        lwa = 9 * n + lwa + n * n + max( 9 * n - lwa, 0 )
      ELSE
        lwa = 9 * n + lwa + n * n + max( 9 * n - lwa, 0 )
      END IF
      ALLOCATE( X( n ), BL( n ), BU( n ), WA( lwa ), XNAMES( n ),              &
                STAT = status )
      IF ( status /= 0 ) GO TO 990

!  set up SIF data

      CALL CUTEST_usetup( status, input, out, io_buffer, n, X, BL, BU )

!  obtain variable names

      CALL CUTEST_unames( status, n, pname, XNAMES )
      IF ( status /= 0 ) GO TO 910

!  record whether there are simple bounds, and the status of each bound

      ibound = 0
      DO i = 1, n
        IF ( BL( i ) > - biginf .OR. BU( i ) < biginf ) THEN
          ibound = 1
          EXIT
        END IF
      END DO

      IF ( ibound == 1 ) THEN
        ALLOCATE( JBOUND( n ), STAT = status )
        IF ( status /= 0 ) GO TO 990
        DO i = 1, n
          IF ( BL( i ) > - biginf ) THEN  ! finite lower bound
            IF ( BU( i ) < biginf ) THEN  ! finite upper bound
              JBOUND( i ) = 3
            ELSE  ! infinite upper bound
              JBOUND( i ) = 2
            END IF
          ELSE  ! infinite lower bound
            IF ( BU( i ) < biginf ) THEN  ! finite upper bound
              JBOUND( i ) = 1
            ELSE  ! infinite upper bound
              JBOUND( i ) = 0
            END IF
          END IF
        END DO
      ELSE
        ALLOCATE( JBOUND( n ), STAT = status )
        IF ( status /= 0 ) GO TO 990
      END IF

!  call the optimizer

      IF ( derivs <= 0 ) THEN
        CALL curvif( CURVI_evalf, n, X, fopt, eps,                             &
                     ibound, JBOUND, BL, BU, WA, nf, nit, idiff, kmax, ier )
      ELSE IF ( derivs == 1 ) THEN
        CALL curvig( CURVI_evalf, CURVI_evalg, n, X, fopt, eps,                &
                     ibound, JBOUND, BL, BU, WA, nf, ng, nit, ier )
      ELSE
        CALL curvih( CURVI_evalf, CURVI_evalg, CURVI_evalh, n, X, fopt, eps,   &
                     itrid, ibound, JBOUND, BL, BU, WA, nf, ng, nh, nit, ier )
      END IF

!  output solution

      CALL CUTEST_ureport( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910

      g = n + n*n
      gnorm = MAXVAL( ABS( WA( g + 1 : g + n ) ) )
      WRITE( out, "( /, '                XL          X',                      &
     &                  '           XU        PROJ(G)' )" )
      DO i = 1, n
        WRITE( out, "(  A10, 4ES12.4 )" )                                      &
          XNAMES( i ), BL( i ), X( i ), BU( i ), WA( g + i )
      END DO
      WRITE ( out, "( /, 24('*'), ' CUTEst statistics ', 24('*') //,           &
     &         ' Code used               :  CURVI',   /,                       &
     &         ' Problem                 :  ', A10,    /,                      &
     &         ' # variables             =      ', I10, /,                     &
     &         ' # objective functions   =        ', F8.2, /,                  &
     &         ' # objective gradients   =        ', F8.2, /,                  &
     &         ' # objective Hessians    =        ', F8.2, /,                  &
     &         ' # iterations            =      ', I10, /,                     &
     &         ' Exit code               =      ', I10 /,                      &
     &         ' Final f                 = ', ES15.7, /,                       &
     &         ' Final ||g||             = ', ES15.7, /,                       &
     &         ' Set up time             =      ', 0P, F10.2, ' seconds', /,   &
     &         ' Solve time              =      ', 0P, F10.2, ' seconds', //,  &
     &           66('*') / )" ) pname, n, ( CALLS( i ), i = 1, 3 ),            &
                   nit, ier, fopt, gnorm, CPU( 1 ), CPU( 2 )
      CLOSE( input  )
      STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', I0, ', stopping' )" ) status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP
      END

      SUBROUTINE CURVI_evalf( n, X, f )
      INTEGER :: n
      DOUBLE PRECISION :: f, X( n )
      INTEGER :: status
      INTEGER, PARAMETER :: out = 6
      CALL CUTEST_ufn( status, n, X, f )
      IF ( status /= 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )" )       &
          status
        STOP
      END IF
      RETURN
      END

      SUBROUTINE CURVI_evalg( n, X, G )
      INTEGER :: n
      DOUBLE PRECISION :: X( n ), G( n )
      INTEGER :: status
      INTEGER, PARAMETER :: out = 6
      CALL CUTEST_ugr( status, n, X, G )
      IF ( status /= 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )" )       &
          status
        STOP
      END IF
      RETURN
      END

      SUBROUTINE CURVI_evalh( n, X, H )
      INTEGER :: n
      DOUBLE PRECISION :: X( n ), H( n * ( n + 1 ) / 2)
      DOUBLE PRECISION, ALLOCATABLE :: H_sym( :, : )
      INTEGER :: status, i, j, l
      INTEGER, PARAMETER :: out = 6
      IF ( .NOT. ALLOCATED( H_sym ) ) ALLOCATE( H_sym( n, n ), STAT = status )
      IF ( status /= 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )" )       &
          status
        STOP
      END IF
      CALL CUTEST_udh( status, n, X, n, H_sym )
      IF ( status /= 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )" )       &
          status
        STOP
      END IF
      l = 0
      DO j = 1, n
        DO i = 1, j
          l = l + 1
          H( l ) = H_sym( i, j )
        END DO
      END DO
      RETURN
      END

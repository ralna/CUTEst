C     ( Last modified on 7 Jan 2013 at 08:20:00 )

      PROGRAM          STENMIN_main

C  STENMIN test driver for problems derived from SIF files.

C  Ph. Toint, January 1996, for CGT Productions.
C  Revised for CUTEst, Nick Gould, January 2013

      INTEGER :: n, nz, lirn, licn, ilim, method, grdflg, hsnflg, ndigit
      INTEGER :: nnzh, msg, lwrk, liwrk, termcd, inform, i, status
      INTEGER, PARAMETER :: input = 55, out = 6, inspec = 46
      INTEGER, PARAMETER :: io_buffer = 11
      DOUBLE PRECISION :: fscale, gradtl, steptl, fpls, stepmx, gnorm
      DOUBLE PRECISION :: typxvl
      DOUBLE PRECISION, PARAMETER :: biginf = 9.0D+19, zero = 0.0D+0
      LOGICAL :: bounds
      CHARACTER ( LEN = 10 ) :: pname
      DOUBLE PRECISION :: CPU( 4 ), CALLS( 4 )
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: IRN, ICN, IWRK
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: X, TYPX, VECTOR
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: XPLS, GPLS
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: HESS, WRK
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: XNAMES
      EXTERNAL :: STENMIN_evalf, STENMIN_evalg, STENMIN_evalsh

C  open the Spec file for the method

      OPEN ( inspec, FILE = 'STNMIN.SPC', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND( inspec )

C  read input Spec data
C
C     ILIM  :  the maximum number of iterations
C     GRADTL:  the relative gradient stopping tolerance
C     GRDFLG:  the gradient availability and checking flag
C     HSNFLG:  the Hessian availability and checking flag
C     FSCALE:  the typical value of the objective function
C     TYPXVL:  the typical value of the problem's variables
C     METHOD:  the method used (0 = Newton, 1 = tensor )
C     NDIGIT:  the number of accurate digits in function values
C     MSG   :  the output specifier

      READ ( inspec, 1000 ) ilim, gradtl, grdflg, hsnflg, fscale,
     *                      typxvl, method, ndigit, msg

C  close input file

      CLOSE ( inspec )

C  open the relevant file

      OPEN ( INPUT, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND INPUT

C  find the problem dimension

      CALL CUTEST_udimen( status, input, n )
      IF ( status /= 0 ) GO TO 910

C  allocate workspace

      ALLOCATE( X( n ), TYPX( n ), XPLS( n ), GPLS( n ), VECTOR( n ),
     *          XNAMES( n ), STAT = status )
      IF ( status /= 0 ) GO TO 990

C  set up SIF data

      CALL CUTEST_usetup( status, INPUT, out, io_buffer, 
     *                    n, X, XPLS, GPLS )
      IF ( status /= 0 ) GO TO 910

C  obtain variable names

      CALL CUTEST_unames( status, n, PNAME, XNAMES )
      IF ( status /= 0 ) GO TO 910

C  compute the number of nonzeros in the Hessian

      CALL CUTEST_udimsh( status, nnzh )
      IF ( status /= 0 ) GO TO 910

      lirn = 2 * nnzh
      licn = 4 * nnzh
      liwrk = 2 * lirn + 12 * n + 2
      lwrk = 7 * n

C  allocate futher workspace

      ALLOCATE( IRN( lirn ), ICN( licn ), IWRK( liwrk ), HESS( licn ), 
     *          WRK( lwrk ), STAT = status )
      IF ( status /= 0 ) GO TO 990

C  set up algorithmic input data

      BOUNDS  = .FALSE.
      DO 10 i = 1, n
        TYPX( i ) = typxvl
        IF ( XPLS( i ) .GT. - biginf .OR. GPLS( i ) .LT. biginf )
     *     bounds = .TRUE.
   10 CONTINUE
      IF ( bounds ) WRITE( out, 2030 )
      inform = 0
      steptl = gradtl * gradtl
      stepmx = zero

C  call the optimizer

      CALL STUMCD( N, X, NZ, IRN, LIRN, ICN, LICN, 
     *             STENMIN_evalf, STENMIN_evalg, STENMIN_evalsh,
     *             TYPX, fscale, gradtl, steptl, ilim, stepmx,
     *             out, method, grdflg, hsnflg, ndigit, msg, XPLS, 
     *             FPLS, GPLS, HESS, WRK, lwrk, IWRK, liwrk, termcd, 
     *             VECTOR, INFORM )
      CALL CUTEST_ureport( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910

      gnorm = zero
      DO 20 i  = 1, n
         gnorm = MAX( gnorm, ABS( GPLS( i ) ) )
   20 CONTINUE
      WRITE ( out, 2040 ) gnorm
      WRITE ( out, 2010 )
      DO 30 i = 1, n
         WRITE( out, 2020 ) XNAMES( i ), XPLS( i ), GPLS( i )
   30 CONTINUE
      WRITE ( out, 2000 ) pname, n, CALLS( 1 ), CALLS( 2 ), CALLS( 3 ), 
     *                    termcd, fpls, CPU( 1 ), CPU( 2 ) 
      CLOSE( INPUT  )
      CALL CUTEST_uterminate( status )
      STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP

C  non-executable statements

 1000 FORMAT( I10, /, D10.3, 2(/,I10), 2(/,D10.3), 3(/,I10) )
 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *    ,' Package used            :  STENMIN',  /
     *    ,' Problem                 :  ', A10,    /
     *    ,' # variables             =      ', I10 /
     *    ,' # objective functions   =        ', F8.2 /
     *    ,' # objective gradients   =        ', F8.2 / 
     *    ,' # objective Hessians    =        ', F8.2 /
     *    ,' Exit code               =      ', I10 /
     *    ,' Final f                 = ', E15.7 /
     *    ,' Set up time             =      ', 0P, F10.2, ' seconds' /
     *    ,' Solve time              =      ', 0P, F10.2, ' seconds' //
     *     66('*') / )
 2010 FORMAT( /, '                 X         G ' )
 2020 FORMAT(  A10, 1P, 2D12.4 )
 2030 FORMAT(  /, ' ** Warning from STNMA. The problem as stated',
     *            ' includes simple bounds. ', /,
     *            '    These bounds will be ignored. ' )
 2040 FORMAT( ' Final gradient norm = ', D12.4 / )
      END

      SUBROUTINE STENMIN_evalf( n, X, f )
      INTEGER :: n
      DOUBLE PRECISION :: f, X( n )

C  Interface for STENMIN (Chow, Schnabel, Eskow, 1993)

      INTEGER :: status
      INTEGER, PARAMETER :: out = 6
      CALL CUTEST_ufn( status, n, X, f )
      IF ( status .NE. 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *     status
        STOP
      END IF
      RETURN
      END

      SUBROUTINE STENMIN_evalg( n, X, G )
      INTEGER :: n
      DOUBLE PRECISION :: X( n ), G( n )
      INTEGER :: status
      INTEGER, PARAMETER :: out = 6
      CALL CUTEST_ugr( status, n, X, G )
      IF ( status .NE. 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *     status
        STOP
      END IF
      RETURN
      END

      SUBROUTINE STENMIN_evalsh( n, X, nnzh, lh, H_val, H_row, H_col )
      INTEGER :: n, lh, nnzh
      INTEGER :: H_row( lh ), H_col( lh )
      DOUBLE PRECISION :: X( n )
      DOUBLE PRECISION :: H_val( lh )
      INTEGER :: status
      INTEGER, PARAMETER :: out = 6
      CALL CUTEST_ush( status, n, X, nnzh, lh, H_val, H_row, H_col )
      IF ( status .NE. 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *     status
        STOP
      END IF
      RETURN
      END


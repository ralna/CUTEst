C THIS VERSION: CUTEST 2.2 - 2023-11-26 AT 15:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      PROGRAM STENMIN_main

      USE CUTEST_KINDS_precision

C  STENMIN test driver for problems derived from SIF files.

C  Ph. Toint, January 1996, for CGT Productions.
C  Revised for CUTEst, Nick Gould, January 2013

      INTEGER ( KIND = ip_ ) :: n, nz, lirn, licn, ilim, method, grdflg
      INTEGER ( KIND = ip_ ) :: hsnflg, ndigit, i, status
      INTEGER ( KIND = ip_ ) :: nnzh, msg, lwrk, liwrk, termcd, inform
      INTEGER ( KIND = ip_ ),  PARAMETER :: input = 55, out = 6
      INTEGER ( KIND = ip_ ),  PARAMETER :: io_buffer = 11, inspec = 46
      REAL ( KIND = rp_ ) :: fscale, gradtl, steptl, fpls, stepmx, gnorm
      REAL ( KIND = rp_ ) :: typxvl
      REAL ( KIND = rp_ ), PARAMETER :: biginf = REAL( 9.0D+19, rp_ )
      REAL ( KIND = rp_ ), PARAMETER :: zero = 0.0_rp_
      LOGICAL :: bounds
      CHARACTER ( LEN = 10 ) :: pname
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 4 )
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IRN, ICN
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IWRK
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, TYPX
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: VECTOR
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: XPLS, GPLS
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: HESS, WRK
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

      CALL CUTEST_udimen_r( status, input, n )
      IF ( status /= 0 ) GO TO 910

C  allocate workspace

      ALLOCATE( X( n ), TYPX( n ), XPLS( n ), GPLS( n ), VECTOR( n ),
     *          XNAMES( n ), STAT = status )
      IF ( status /= 0 ) GO TO 990

C  set up SIF data

      CALL CUTEST_usetup_r( status, INPUT, out, io_buffer, 
     *                    n, X, XPLS, GPLS )
      IF ( status /= 0 ) GO TO 910

C  obtain variable names

      CALL CUTEST_unames_r( status, n, PNAME, XNAMES )
      IF ( status /= 0 ) GO TO 910

C  compute the number of nonzeros in the Hessian

      CALL CUTEST_udimsh_r( status, nnzh )
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
      CALL CUTEST_ureport_r( status, CALLS, CPU )
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
      CALL CUTEST_uterminate_r( status )
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
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n
      REAL ( KIND = rp_ ) :: f, X( n )

C  Interface for STENMIN (Chow, Schnabel, Eskow, 1993)

      INTEGER ( KIND = ip_ ) :: status
      INTEGER ( KIND = ip_ ), PARAMETER :: out = 6
      CALL CUTEST_ufn_r( status, n, X, f )
      IF ( status /= 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *     status
        STOP
      END IF
      RETURN
      END

      SUBROUTINE STENMIN_evalg( n, X, G )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n
      REAL ( KIND = rp_ ) :: X( n ), G( n )
      INTEGER ( KIND = ip_ ) :: status
      INTEGER ( KIND = ip_ ), PARAMETER :: out = 6
      CALL CUTEST_ugr_r( status, n, X, G )
      IF ( status /= 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *     status
        STOP
      END IF
      RETURN
      END

      SUBROUTINE STENMIN_evalsh( n, X, nnzh, lh, H_val, H_row, H_col )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n, lh, nnzh
      INTEGER ( KIND = ip_ ) :: H_row( lh ), H_col( lh )
      REAL ( KIND = rp_ ) :: X( n )
      REAL ( KIND = rp_ ) :: H_val( lh )
      INTEGER ( KIND = ip_ ) :: status
      INTEGER ( KIND = ip_ ), PARAMETER :: out = 6
      CALL CUTEST_ush_r( status, n, X, nnzh, lh, H_val, H_row, H_col )
      IF ( status /= 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *     status
        STOP
      END IF
      RETURN
      END


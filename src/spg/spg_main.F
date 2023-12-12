! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      PROGRAM SPG_main

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     Driver for running the Spectral Projected Gradient (SPG) method
C     on CUTEst problems.
C
C     Derived from SPG spgma.f and cuterwrapper.f from the TANGO home page
C       www.ime.usp.br/~egbirgin/tango/
C     CUTEst evolution February 2013, Nick Gould
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C  Set up parameters, variables and arrays required by unconstrained tools

      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ), PARAMETER :: input = 55, indr = 46
      INTEGER ( KIND = ip_ ), PARAMETER :: out = 6
      INTEGER ( KIND = ip_ ) :: inform, iprint, n, iter, maxit, fcnt
      INTEGER ( KIND = ip_ ) :: status, maxfc, spginfo
      REAL ( KIND = rp_ ) :: epsopt, f, gpsupn
      CHARACTER * 10 :: pname
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 4 )
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X

C  Open the relevant file

      OPEN ( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND( input )

C  compute problem dimensions

      CALL CUTEST_udimen_r( status, input, n )
      IF ( status /= 0 ) GO TO 910
C  close input so that inip can open it again!
      CLOSE( input )

C  allocate space

      ALLOCATE( X( n ), STAT = status )
      IF ( status /= 0 ) GO TO 990

C  open the Spec file for the package

      OPEN( indr, FILE = 'SPG.SPC', FORM = 'FORMATTED', STATUS = 'OLD')
      REWIND( indr )

C  set up algorithmic input data

C   iprint  controls output level (0 = no print)
C   maxit   maximum number of iterations
C   maxfc   maximum number of function evaluations
C   epsopt  tolerance for the convergence criterion

      READ ( indr, "( ( G10.8 ) )" ) iprint, maxit, maxfc, epsopt
      CLOSE ( indr )

C  initialize data

      CALL inip( n, X )

C  call the optimizer

      CALL spg( n, X, epsopt, maxit, maxfc, iprint, f, gpsupn, iter,
     *          fcnt, spginfo, inform)

C  output information

      CALL CUTEST_ureport_r( status, CALLS, CPU )
      CALL CUTEST_probname_r( status, pname )
      IF ( out .GT. 0 ) WRITE ( out, 2000 ) pname, n, CALLS( 1 ),
     *       CALLS( 2 ), inform, f, CPU( 1 ), CPU( 2 )

      DEALLOCATE( X, STAT = status )
      CALL CUTEST_uterminate_r( status )
      STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )")
     *   status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP

C  Non-executable statements

 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *    ,' Package used            :  SPG',    /
     *    ,' Problem                 :  ', A10,    /
     *    ,' # variables             =      ', I10 /
     *    ,' # objective functions   =        ', F8.2 /
     *    ,' # objective gradients   =        ', F8.2 /
     *    ,' Exit code               =      ', I10 /
     *    ,' Final f                 = ', E15.7 /
     *    ,' Set up time             =      ', 0P, F10.2, ' seconds' /
     *    ,' Solve time              =      ', 0P, F10.2, ' seconds' //
     *     66('*') / )
      END

C     *****************************************************************
C     *****************************************************************

      subroutine inip(n,x)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      integer ( kind = ip_ ) n

C     ARRAY ARGUMENTS
      real ( kind = rp_ ) x(*)

C     PARAMETERS
      integer ( kind = ip_ ) input, iout, nmax
      parameter ( nmax  = 100000 )
      parameter ( input = 55 )
      parameter ( iout  = 6 )
      INTEGER ( KIND = ip_ ), PARAMETER :: io_buffer = 11
      INTEGER ( KIND = ip_ ) :: status

C     COMMON ARRAYS
      real ( kind = rp_ ) l( nmax ), u( nmax )

C     COMMON BLOCKS
      common /bounds/ l, u
      save   /bounds/

C     EXTERNAL SUBROUTINES

      open(input,file='OUTSDIF.d',form='FORMATTED',status='OLD')
      rewind( input )

      call CUTEST_usetup_r( status, input, iout, io_buffer, n, x, l, u )
      if ( status .ne. 0 ) then
        write( 6, "( ' CUTEst error, status = ', i0, ', stopping' )" )
     *     status
        stop
      end if

      close(input)

      end

C     *****************************************************************
C     *****************************************************************

      subroutine evalf(n,x,f,flag)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      integer ( kind = ip_ ) flag, n
      real ( kind = rp_ ) f

C     ARRAY ARGUMENTS
      real ( kind = rp_ ) x( n )

C     EXTERNAL SUBROUTINES
      external ufn

      flag = 0
      call CUTEST_ufn_r( flag, n, x, f )

      end

C     *****************************************************************
C     *****************************************************************

      subroutine evalg(n,x,g,flag)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      integer ( kind = ip_ ) flag,n

C     ARRAY ARGUMENTS
      real ( kind = rp_ ) g( n ), x( n )

C     EXTERNAL SUBROUTINES
      external ugr

      flag = 0
      call CUTEST_ugr_r( flag, n, x, g )

      end

C     *****************************************************************
C     *****************************************************************

      subroutine proj(n,x,flag)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      integer ( kind = ip_ ) flag,n

C     ARRAY ARGUMENTS
      real ( kind = rp_ ) x(n)

C     PARAMETERS
      integer ( kind = ip_ ) nmax
      parameter ( nmax = 100000 )

C     COMMON ARRAYS
      real ( kind = rp_ ) l(nmax), u(nmax)

C     LOCAL SCALARS
      integer ( kind = ip_ ) i

C     COMMON BLOCKS
      common /bounds/ l,u
      save   /bounds/

      flag = 0

      do i = 1,n
          x(i) = max( l(i), min( x(i), u(i) ) )
      end do

      end

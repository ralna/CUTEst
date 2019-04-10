C     ( Last modified on 18 Feb 2013 at 13:00:00 )

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

      INTEGER, PARAMETER :: input = 55, indr = 46, out = 6
      INTEGER :: inform, iprint, n, iter, maxit, fcnt, maxfc, spginfo
      INTEGER :: status
      DOUBLE PRECISION :: epsopt, f, gpsupn
      CHARACTER * 10 :: pname
      DOUBLE PRECISION :: CPU( 2 ), CALLS( 4 )
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: X

C  Open the relevant file

      OPEN ( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND( input )

C  compute problem dimensions

      CALL CUTEST_udimen( status, input, n )
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

      CALL CUTEST_ureport( status, CALLS, CPU )
      CALL CUTEST_probname( status, pname )
      IF ( out .GT. 0 ) WRITE ( out, 2000 ) pname, n, CALLS( 1 ),
     *       CALLS( 2 ), inform, f, CPU( 1 ), CPU( 2 )

      DEALLOCATE( X, STAT = status )
      CALL CUTEST_uterminate( status )
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

      implicit none

C     SCALAR ARGUMENTS
      integer n

C     ARRAY ARGUMENTS
      double precision x(*)

C     PARAMETERS
      integer input,iout,nmax
      parameter ( nmax  = 100000 )
      parameter ( input =     55 )
      parameter ( iout  =      6 )
      INTEGER, PARAMETER :: io_buffer = 11
      INTEGER :: status

C     COMMON ARRAYS
      double precision l(nmax),u(nmax)

C     COMMON BLOCKS
      common /bounds/ l,u
      save   /bounds/

C     EXTERNAL SUBROUTINES

      open(input,file='OUTSDIF.d',form='FORMATTED',status='OLD')
      rewind( input )

      call CUTEST_usetup(status,input,iout,io_buffer,n,x,l,u)
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

      implicit none

C     SCALAR ARGUMENTS
      integer flag,n
      double precision f

C     ARRAY ARGUMENTS
      double precision x(n)

C     EXTERNAL SUBROUTINES
      external ufn

      flag = 0
      call CUTEST_ufn(flag,n,x,f)

      end

C     *****************************************************************
C     *****************************************************************

      subroutine evalg(n,x,g,flag)

      implicit none

C     SCALAR ARGUMENTS
      integer flag,n

C     ARRAY ARGUMENTS
      double precision g(n),x(n)

C     EXTERNAL SUBROUTINES
      external ugr

      flag = 0
      call CUTEST_ugr(flag,n,x,g)

      end

C     *****************************************************************
C     *****************************************************************

      subroutine proj(n,x,flag)

      implicit none

C     SCALAR ARGUMENTS
      integer flag,n

C     ARRAY ARGUMENTS
      double precision x(n)

C     PARAMETERS
      integer nmax
      parameter ( nmax = 100000 )

C     COMMON ARRAYS
      double precision l(nmax),u(nmax)

C     LOCAL SCALARS
      integer i

C     COMMON BLOCKS
      common /bounds/ l,u
      save   /bounds/

      flag = 0

      do i = 1,n
          x(i) = max( l(i), min( x(i), u(i) ) )
      end do

      end

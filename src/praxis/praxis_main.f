C     ( Last modified on 6 Jan 2013 at 12:00:00 )

      PROGRAM PRAXIS_main

C  PRAXIS test driver for problems derived from SIF files.

C  Ph. Toint, for CGT Productions.
C  January 1996.
C  Revised for CUTEst, Nick Gould, January 2013

      INTEGER :: n, nl, nf, lp, jprint, illcin, ktm, i
      INTEGER :: nfmax, jranch, nmx, status
C Nick - stupid use of common does not allow allocatable arrays
      INTEGER, PARAMETER :: nmax = 1000
      INTEGER, PARAMETER :: input = 55, out = 6, inspec = 46
      INTEGER, PARAMETER :: io_buffer = 11
      DOUBLE PRECISION :: dmin, epsmch, fx, h, qd0, qd1, qf1
      DOUBLE PRECISION :: small, t, xldt, xm2, xm4, dseed, scbd
      DOUBLE PRECISION, PARAMETER :: one = 1.0D0
      CHARACTER ( LEN = 10 ) :: pname
      DOUBLE PRECISION :: CPU( 2 ), CALLS( 4 )
      DOUBLE PRECISION, DIMENSION( nmax ) :: X, D, Q0, Q1
      DOUBLE PRECISION, DIMENSION( nmax, nmax ) :: V
      CHARACTER ( LEN = 10 ), DIMENSION( nmax )  :: XNAMES
      DOUBLE PRECISION :: PRAXIS_evalf
      EXTERNAL :: PRAXIS_evalf
      COMMON / CPRAX / V, X, D, Q0, Q1, dmin, epsmch, fx, h, qd0, qd1,
     *                 qf1, small, t, xldt, xm2, xm4, dseed, scbd, n,
     *                 nl, nf, lp, jprint, nmx, illcin, ktm, nfmax,
     *                 jranch
      SAVE / CPRAX /

C  lp is the logical unit number for printed output

      lp = out

C  h is an estimate of the distance from the initial point
C  to the solution.

      h = one

C  epsmch is the smallest floating point (real or double precision)
C  number which, when added to one, gives a result greater than one.

      epsmch = EPSILON( one )

C  JRANCH = 1 to use BRENT's random,
C  JRANCH = 2 to use function DRANDM.

      jranch = 1
      CALL RANINI( 4.0D+0 )

C  DSEED is an initial seed for DRANDM,
C  a subroutine that generates pseudorandom numbers
C  uniformly distributed on (0,1).

      dseed = 1234567.0D+0

C  open the Spec file for the method.

      OPEN ( inspec, FILE = 'PRAXIS.SPC', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND( inspec )

C  read input Spec data.

C     NFMAX :  the maximum number of function calls
C     T     :  the stopping tolerance
C     SCBD  : the upper bound on the scale factors
C     ILLCIN: the "ill-conditioning" flag
C     KTM   :  the maximum number of iterations without improvement
C     JPRINT: the printing specifier
C
      READ ( inspec, 1000 ) NFMAX, T, SCBD, ILLCIN, KTM, JPRINT
C     WRITE( 6, 1000 ) NFMAX, T, SCBD, ILLCIN, KTM, JPRINT

C  close input file

      CLOSE ( inspec )

C  open the input data file

      OPEN ( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND( input )

C  find the problem dimension

      CALL CUTEST_udimen( status, input, n )
      IF ( status /= 0 ) GO TO 910
      IF ( n > nmax ) GO TO 990

C  maximum dimension

      nmx = nmax

C  set up SIF data

      CALL CUTEST_usetup( status, input, out, io_buffer, n, X, Q0, Q1 )
      IF ( status /= 0 ) GO TO 910
      CLOSE( input )

C  obtain variable names

      CALL CUTEST_unames( status, N, PNAME, XNAMES )
      IF ( status /= 0 ) GO TO 910

C  call the optimizer

      CALL PRAXIS( PRAXIS_evalf )

C  exit. Unfortunately, PRAXIS does not provide any termination
C  status to flag a successful call

      CALL CUTEST_ureport( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910
      WRITE ( out, 2010 )
      DO 40 i = 1, n
        WRITE( out, 2020 ) XNAMES( i ), X( i )
   40 CONTINUE
      WRITE ( out, 2000 ) pname, n, CALLS( 1 ), fx, CPU( 1 ), CPU( 2 )
      STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )")
     *   status
      STOP

  990 CONTINUE
      WRITE( out, 2100 ) n, nmax
      STOP

C  non-executable statements

 1000 FORMAT( I10, 2( /, D10.3), 3( /, I10 ) )
 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *    , ' Code used               :  PRAXIS', /
     *    , ' Problem                 :  ', A10,  /
     *    , ' # variables             =      ', I10 /
     *    , ' # objective functions   =        ', F8.2 /
     *    , ' Final f                 = ', E15.7 /
     *    , ' Set up time             =      ', 0P, F10.2, ' seconds' /
     *      ' Solve time              =      ', 0P, F10.2, ' seconds' //
     *      66('*') / )
 2010 FORMAT( /, '                 X' )
 2020 FORMAT(  1X, A10, 1P, D12.4 )
 2100 FORMAT( ' Dimension ', I0, ' larger than largest allowed',
     *  ' nmax = ', I0, ', stopping' )
      END

      DOUBLE PRECISION FUNCTION PRAXIS_evalf( X, n )
      INTEGER :: n
      DOUBLE PRECISION X( n )
      INTEGER :: status
      INTEGER, PARAMETER :: out = 6
      CALL CUTEST_ufn( status, n, X, PRAXIS_evalf )
      IF ( status .NE. 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )")
     *     status
        STOP
      END IF
      RETURN
      END



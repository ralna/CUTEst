C  THIS VERSION: CUTEST 2.2 - 2023-11-29 AT 16:00 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      PROGRAM PDSMA

      USE CUTEST_KINDS_precision
      IMPLICIT NONE
C
C  PDS test driver for problems derived from SIF files.
C
C  A. R Conn and Ph. Toint for CGT Productions.
C  January 1995, substantially modified September 1996
C  Revised for CUTEst, Nick Gould, January 2013

      INTEGER ( KIND = ip_ ) :: i, cnt, debug, error, unique, maxitr, n
      INTEGER ( KIND = ip_ ) :: ifact, type, resize, sss, status
      INTEGER ( KIND = ip_ ), PARAMETER :: input = 55, out = 6
      INTEGER ( KIND = ip_ ), PARAMETER :: inspec = 46
      INTEGER ( KIND = ip_ ), PARAMETER :: indr = 46, res = 56, sch = 48
      INTEGER ( KIND = ip_ ), PARAMETER :: io_buffer = 11
C Nick - what are these? Presumably they relate to the dimension n?
CTOY  INTEGER ( KIND = ip_ ), PARAMETER :: dim  = 10, imax = 2000   
CMED  INTEGER ( KIND = ip_ ), PARAMETER :: dim  = 20, imax = 4000
      INTEGER ( KIND = ip_ ), PARAMETER :: dim  = 30, imax = 6000
      INTEGER ( KIND = ip_ ), PARAMETER :: limit = ( dim + 2 ) * imax
      REAL ( KIND = rp_ ) factor, fbest, scale, tol, length
      REAL ( KIND = rp_ ), PARAMETER :: biginf = 9.0E+19_rp_
      CHARACTER ( LEN = 10 ) :: pname
      LOGICAL :: bounds
      INTEGER ( KIND = ip_ ) :: SCHEME( limit ), LIST( limit )
      INTEGER ( KIND = ip_ ) :: INDEX( limit )
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 4 )
      REAL ( KIND = rp_ ) WORK( dim ), S( dim * ( dim + 1 ) )
      REAL ( KIND = rp_ ) WORK1( - 3 : dim + 1 ), WORK2( - 3 : dim + 1 )
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, BL, BU
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: XNAMES
      EXTERNAL :: pds_evalf

C  open the Spec file for the method

      OPEN( indr, FILE = 'PDS.SPC', FORM = 'FORMATTED',
     *      STATUS = 'OLD' )
      REWIND indr
C
C  read input Spec data
C
C   TOL    = the stopping tolerance for the step size
C   MAXITR = the maximum number of iterations allowed
C   TYPE     specifies the type of initial simplex
C            0   User provides the initial simplex
C            1   Automatic generation of a right-angled simplex
C            2   Automatic generation of a regular simplex
C            3   Automatic generation of a scaled right-angled simplex
C   SCALE    If the simplex is automatically generated, scale should
C            contain both the base length and the orientation of the edges
C   DEBUG    should be set to 0, 1, 2, 3 or 4, it controls the amount of 
C            printing.
C            0   no debugging output
C            1   display the iteration count, the best vertex 
C                and its function value
C            2   include the simplex and flag whether or
C                not strict decrease was obtained
C            3   include all vertices constructed and their 
C                finction values
C            4   Include the points that define the search scheme
C   SSS      number of points used in the search strategy

c  Set up algorithmic input data

      READ ( indr, 1000 ) tol, maxitr, type, scale, debug, sss

C  close SPEC file

      CLOSE( INDR )

C  open the input data file

      OPEN( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED', 
     *      STATUS = 'OLD' )
      REWIND( input )

C  find the problem dimension

      CALL CUTEST_udimen_r( status, input, n )
      IF ( status /= 0 ) GO TO 910

C  allocate workspace

      ALLOCATE( X( N ), BU( N ), BL( N ), XNAMES( n ), STAT = status )
      IF ( status /= 0 ) GO TO 990

C  set up SIF data

      CALL CUTEST_usetup_r( status, INPUT, out, io_buffer, 
     *                      N, X, BL, BU )
      IF ( status /= 0 ) GO TO 910
      CLOSE( input )

C  obtain variable names

      CALL CUTEST_unames_r( status, n, pname, XNAMES )
      IF ( status /= 0 ) GO TO 910

C  set up algorithmic input data

      bounds = .FALSE.
      DO 10 i = 1, n
        IF ( BL( i ) .GT. - biginf .OR. BU( i ) .LT. biginf )
     *    bounds = .TRUE.
   10 CONTINUE
      IF ( bounds ) WRITE( out, 2030 )
      DO 20 i = 1, n
         S( i ) = X( i )
   20 CONTINUE

C  form the scheme

      OPEN( sch, FILE = 'SCHEME', STATUS = 'UNKNOWN', 
     *      FORM = 'UNFORMATTED')
      CALL SEARCH( n, sch, limit, SCHEME, INDEX, LIST, unique, ifact, 
     *             error )
      CLOSE( sch )

      OPEN( res, FILE = 'RESULT', STATUS = 'UNKNOWN',FORM = 'FORMATTED')
      factor = FLOAT( ifact )
      IF ( error .EQ. 0 ) THEN
        WRITE( res, 100 ) n
        WRITE( res, 200 ) unique
        WRITE( res, 300 ) ifact
      ELSE
        WRITE( res, 400 )
      ENDIF

C  re-open the search scheme file

      OPEN( sch, FILE = 'SCHEME', STATUS = 'OLD', FORM = 'UNFORMATTED')
      REWIND( sch )

C  read in the search scheme and determine the "shrink" factor (which
C  depends on the size of the search scheme that has been specified).

      CALL GETSS( n, sch, sss, SCHEME, FACTOR, resize, error )

C  Close the file for the search scheme

      CLOSE( sch )
C
C  Call the optimizer if no error occured so far
C
      IF ( error .EQ. 0 ) THEN     
        CALL PDS( n, out, type, Scale, debug, tol, maxitr, 
     *            sss, PDS_evalf, factor, SCHEME, resize, S,
     *            INDEX, fbest, length, cnt, WORK, WORK1, WORK2 )
C
C  Write the results to a file and on the standard output.
C
        WRITE ( out, 2010 )
        DO 30 i = 1, n
          WRITE( out, 2020 ) XNAMES( i ), S( i )
   30   CONTINUE
        CALL RESULT( n, cnt, S, FBEST, INDEX, RES )
      ELSE IF ( error .EQ. 1 ) THEN
        WRITE( res, 500 )
      ELSE IF ( error .EQ. 2 ) THEN
        WRITE( res, 600 )
      ENDIF
      CLOSE( res )
C
C  Write results on the standard output
C
      CALL CUTEST_ureport_r( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910
      WRITE ( out, 2000 ) pname, n, CALLS( 1 ), error, fbest, 
     *                    CPU( 1 ), CPU( 2 ) 
      STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP
C
C  Non-executable statements.
C
 100  FORMAT( 'Successfully completed a search strategy for problems '
     *        ,'of dimension', I6 )
 200  FORMAT( 'The total number of unique points available is', I26 )
 300  FORMAT( 'The factor needed to restore these points to their ',
     *        'real values is', I7 )
 400  FORMAT( 'Returned without a completed search strategy because', /
     *      , 'of internal stack overflow in the QUICKSORT routines.', /
     *      , 'Check the documentation for further details.')
 500  FORMAT( //, ' Search scheme was of the wrong dimension.', /
     *      , ' Exited without calling PDS.' // )
 600  FORMAT( //, ' Insufficient number of points in scheme.', /
     *      , ' Exited without calling PDS.' // )
 1000 FORMAT( D10.3, /, I10, /, I10, /, D10.3, /, I10, /, I10 )
 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *    , ' Code used               :  PDS', /
     *    , ' Problem                 :  ', A10,  /
     *    , ' # variables             =      ', I10 /
     *    , ' # objective functions   =        ', F8.2 /
     *    , ' Exit code               =      ', I10 /
     *    , ' Final f                 = ', E15.7 /
     *    , ' Set up time             =      ', 0P, F10.2, ' seconds' /
     *    , ' Solve time              =      ', 0P, F10.2, ' seconds' //
     *      66('*') / )
 2010 FORMAT( /, '                 X  ' )
 2020 FORMAT(  A10, 1P, D12.4 )
 2030 FORMAT(  /, ' ** Warning from PDS_main. The problem as stated',
     *            ' includes simple bounds. ', /,
     *            '    These bounds will be ignored. ' )
C
C  End of PDSMA
C
      END

      SUBROUTINE PDS_evalf( n, X, f )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n
      REAL ( KIND = rp_ ) :: f, X( n )
      INTEGER ( KIND = ip_ ) :: status
      INTEGER ( KIND = ip_ ), PARAMETER :: out = 6
      CALL CUTEST_ufn_r( status, n, X, f )
      IF ( status .NE. 0 ) THEN
        WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *     status
        STOP
      END IF
      RETURN
      END

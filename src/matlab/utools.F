C THIS VERSION: CUTEST 1.0 - 16/01/2013 AT 10:20 GMT.

C-*-*-  C U T E S T    U T O O L S   G A T E W A Y    S U B R O U T I N E  -*-*-

C  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
C  Principal authors: Ingrid Bongartz, Nick Gould and Dominique Orban

C  History -
C   originally released 1994
C   fortran 2003 version released in CUTEst, 16th January 2013

C  MATLAB Fortran gateway routine to unconstrained CUTEst tools

      SUBROUTINE  MEXFUNCTION( nlhs, PLHS, nrhs, PRHS )
      INTEGER * 4 nlhs, nrhs
      INTEGER * 4 PLHS( * ), PRHS( * )

      INTEGER * 4 MXCREATEFULL, MXGETPR
      INTEGER * 4 MXGETM, MXGETN
      REAL * 8    MXGETSCALAR

C  Parameters which may be changed by the user.

      INTEGER * 4     NMAX
C  make this dynamic!!
      PARAMETER     ( NMAX = 500 )
      INTEGER * 4     NNZMAX
      PARAMETER     ( NNZMAX = 10*NMAX )
      INTEGER * 4     LBANDH
      PARAMETER     ( LBANDH = ( NMAX / 2 ) + 1 )
      INTEGER * 4     input, out, io_buffer
      PARAMETER     ( input = 55, out = 6, io_buffer = 11 )

C  End of Parameters which may be changed by the user.

      CHARACTER * 64  prbdat
      CHARACTER * 10  pname
      CHARACTER * 10  XNAMES( NMAX )
      INTEGER * 4     nvar, nnzsh, nsemib
      INTEGER * 4     JCOL( NMAX + 1 )
      INTEGER * 4     IRNSH( NNZMAX ), ICNSH( NNZMAX ), IROW( NNZMAX )
      REAL * 8        f
      REAL * 8        X( NMAX ), BL( NMAX ), BU( NMAX ), G( NMAX ),
     *                P( NMAX ), Q( NMAX )
      REAL * 8        DH( NMAX*NMAX )
      REAL * 8        SH( NNZMAX ), A( NNZMAX )
      REAL * 8        BANDH( LBANDH*NMAX )
      LOGICAL         grad, goth

C  Common block to store problem dimensions.

      COMMON / SIZE / nvar

C  Local variables

      INTEGER * 4     pr, ir, jc, ierr, iptr, n, nn, np1, nsemp1,
     *                i, j, status, maxsbw, nnz
      INTEGER * 4     IVARTY( NMAX )
      REAL * 8        IVARTY_real( NMAX )
      REAL * 8        WORKN( NMAX*10 ), N_real
      CHARACTER * 2   msg_status
      CHARACTER * 7   TOOL, TSETUP, TNAMES, TFN, TGR, TOFG, TDH,
     *                TGRDH, TSH, TGRSH, TPROD, TBANDH, TDIMEN,
     *                TDIMSH, TVARTY, TREPRT
      CHARACTER * 150 msg
      REAL * 8        CALLS( 4 ), TIME( 2 )

C  External functions

      CHARACTER * 100 NULLSTR, UPPER
      LOGICAL         EQUAL

C  Check for at least one input argument, 
C  which is the name of the tool to be called.

      IF ( nrhs .EQ. 0 ) THEN
         msg = 'UTOOLS requires at least one input argument.'
         CALL MEXERRMSGTXT( msg )
      END IF

C  Get name of unconstrained tool being called.
C  Null-terminate this name and convert it to upper case.

      ierr = MXGETSTRING( PRHS( 1 ), TOOL, 6 )
      IF ( ierr .EQ. 0 ) THEN 
         TOOL = NULLSTR( TOOL )
         TOOL = UPPER  ( TOOL )
      ELSE
         msg = 'ERROR copying tool name from first input argument.'
         CALL MEXERRMSGTXT( msg )
      END IF

C  Set up tool names.

      TSETUP = NULLSTR( 'CUTEST_usetup' )
      TNAMES = NULLSTR( 'CUTEST_unames' )
      TFN    = NULLSTR( 'CUTEST_ufn' )
      TGR    = NULLSTR( 'CUTEST_ugr' )
      TOFG   = NULLSTR( 'CUTEST_uofg' )
      TDH    = NULLSTR( 'CUTEST_udh' )
      TGRDH  = NULLSTR( 'CUTEST_ugrdh' )
      TSH    = NULLSTR( 'CUTEST_ush' )
      TGRSH  = NULLSTR( 'CUTEST_ugrsh' )
      TPROD  = NULLSTR( 'CUTEST_uhprod' )
      TBANDH = NULLSTR( 'CUTEST_ubandh' )
      TDIMEN = NULLSTR( 'CUTEST_udimen' )
      TDIMSH = NULLSTR( 'CUTEST_udimsh' )
      TVARTY = NULLSTR( 'CUTEST_uvartype' )
      TREPRT = NULLSTR( 'CUTEST_ureport' )

C  Call the requested tool.

	 IF ( EQUAL( TOOL, TSETUP ) ) THEN

C  Check number of output arguments.

         IF ( nlhs .NE. 3 ) THEN
            msg = 'CUTEST_usetup returns three output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Build data input file name.

         prbdat = 'OUTSDIF.d'


C  Open the relevant file.

         OPEN ( input, FILE = prbdat, FORM = 'FORMATTED',
     *          STATUS = 'OLD' )
         REWIND input


C  Set up the problem data structures.

         CALL CUTEST_usetup( status, input, out, io_buffer, 
     *                       nvar, X, BL, BU, NMAX )
         IF ( status .NE. 0 ) GO TO 910

C  Copy X.

         PLHS( 1 ) = MXCREATEFULL( nvar, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( X, pr, nvar )

C  Copy BL.

         PLHS( 2 ) = MXCREATEFULL( nvar, 1, 0 )
         pr = MXGETPR( PLHS( 2 ) )
         CALL MXCOPYREAL8TOPTR( BL, pr, nvar )

C  Copy BU.

         PLHS( 3 ) = MXCREATEFULL( nvar, 1, 0 )
         pr = MXGETPR( PLHS( 3 ) )
         CALL MXCOPYREAL8TOPTR( BU, pr, nvar )

	 CLOSE( input )

      ELSE IF ( EQUAL( TOOL, TNAMES ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 1 .AND. nrhs .NE. 2 ) THEN
            msg = 'CUTEST_unames requires zero or one input arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 2 ) THEN
            msg = 'CUTEST_unames returns two output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Get number of variable names requested.

         n = nvar
         IF ( nrhs. EQ. 2 ) THEN
            n = NINT( MXGETSCALAR( PRHS( 2 ) ) )
            IF ( n .LT. 1 .OR. n .GT. nvar ) THEN
               msg = 'invalid number of variable names requested.'
               CALL MEXERRMSGTXT( msg )
            END IF
         END IF
         CALL CUTEST_unames( status, nvar, pname, XNAMES )
         IF ( status .NE. 0 ) GO TO 910

C  Copy pname and XNAMES.  First convert XNAMES to a REAL*8 array.

         PLHS( 1 ) = MXCREATESTRING( pname )
         CALL CNVCHR( XNAMES, n, 10, WORKN )
         PLHS( 2 ) = MXCREATEFULL( n, 10, 0 )
         nn = n * 10
         pr = MXGETPR( PLHS( 2 ) )
         CALL MXCOPYREAL8TOPTR( WORKN, pr, nn )

      ELSE IF ( EQUAL( TOOL, TFN ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 2 ) THEN
            msg = 'CUTEST_ufn requires x as input.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_ufn returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second input argument into X.
C 
         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Get the objective function value.

         CALL CUTEST_ufn( status, n, X, F )
         IF ( status .NE. 0 ) GO TO 910

C  Copy F. 

         PLHS( 1 ) = MXCREATEFULL( 1, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( f, pr, 1 )

      ELSE IF ( EQUAL( TOOL, TGR ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 2 ) THEN
            msg = 'CUTEST_ugr requires x as input.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_ugr returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Get the gradient of the objective function.

         CALL CUTEST_ugr( status, n, X, G )
         IF ( status .NE. 0 ) GO TO 910

C  Copy G.

         PLHS( 1 ) = MXCREATEFULL( n, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( G, pr, n )

      ELSE IF ( EQUAL( TOOL, TOFG ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 2 ) THEN
            msg = 'CUTEST_uofg requires x as input.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 .AND. nlhs .NE. 2 ) THEN
	    msg = 'CUTEST_uofg returns one or two output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Decide if the gradient is required.

         grad = .FALSE.
         IF ( nlhs .EQ. 2 ) grad = .TRUE.

C  Get the value of the objective function, and possibly its gradient.

         CALL CUTEST_uofg( status, n, X, f, G, grad )
         IF ( status .NE. 0 ) GO TO 910

C  Copy F.

         PLHS( 1 ) = MXCREATEFULL( 1, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( f, pr, 1 )

C  Copy G, if it is required.

         IF ( grad ) THEN
            PLHS( 2 ) = MXCREATEFULL( n, 1, 0 )
            pr = MXGETPR( PLHS( 2 ) )
            CALL MXCOPYREAL8TOPTR( G, pr, n )
         END IF

      ELSE IF ( EQUAL( TOOL, TDH ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 2 ) THEN
            msg = 'CUTEST_udh requires x as input.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_udh returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Get the dense Hessian evaluated at X.

         CALL CUTEST_udh( status, n, X, n, DH )
         IF ( status .NE. 0 ) GO TO 910

C  Copy H.

         PLHS( 1 ) = MXCREATEFULL( n, n, 0 )
         nn = n * n
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( DH, pr, nn )

      ELSE IF ( EQUAL( TOOL, TGRDH ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 2 ) THEN
            msg = 'CUTEST_ugrdh requires x as input.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 2 ) THEN
            msg = 'CUTEST_ugrdh returns two output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Get the gradient and dense Hessian evaluated at X.

         CALL CUTEST_ugrdh( status, n, X, G, n, DH )
         IF ( status .NE. 0 ) GO TO 910

C  Copy G.

         PLHS( 1 ) = MXCREATEFULL( n, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( G, pr, n )

C  Copy H.

         PLHS( 2 ) = MXCREATEFULL( n, n, 0 )
         nn = n * n
         pr = MXGETPR( PLHS( 2 ) )
         CALL MXCOPYREAL8TOPTR( DH, pr, nn )

      ELSE IF ( EQUAL( TOOL, TSH ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 2 ) THEN
            msg = 'CUTEST_ush requires x as input.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_ush returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Get the sparse Hessian evaluated at X.

         CALL CUTEST_ush( status, n, X, nnzsh, NNZMAX, SH, 
     *                    IRNSH, ICNSH )
         IF ( status .NE. 0 ) GO TO 910

C  Make sure NNZMAX is big enough.

         IF ( nnzsh .GT. NNZMAX ) THEN
            msg = 'number of nonzeros in sparse Hessian exceeds ' // 
     *            'declared size of sparse Hessian in MEX-file.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Remove any zeros and count how many nonzeros there are in the
C  combined lower and upper triangular parts

         j = 0
         nnz = 0
         DO 111 i = 1, nnzsh
           IF ( SH( i ) .NE. 0.0D+0 ) THEN
             j = j + 1
             nnz = nnz + 1
             IF ( IRNSH( i ) .NE. ICNSH( i ) ) nnz = nnz + 1
             IRNSH( j ) = IRNSH( i )
             ICNSH( j ) = ICNSH( i )
             SH( j ) = SH( i )
           END IF
  111    CONTINUE
         nnzsh = J
         IF ( nnz .GT. NNZMAX ) THEN
            msg = 'number of nonzeros in sparse Hessian exceeds ' // 
     *            'declared size of sparse Hessian in MEX-file.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Include both lower and upper triangular parts (for Matlab)

         j = nnzsh
         DO 112 i = 1, nnzsh
           IF ( IRNSH( i ) .NE. ICNSH( i ) ) THEN
             j = j + 1
             IRNSH( j ) = ICNSH( i )
             ICNSH( j ) = IRNSH( i )
             SH( j ) = SH( i )
           END IF
  112    CONTINUE
         nnzsh = J

C  Convert from the CUTE sparse matrix to a MATLAB sparse matrix.
C  Put the MATLAB sparse matrix in A, IROW, and JCOL.

         CALL CNVSPR( n, nnzsh, SH, IRNSH, ICNSH, A, IROW, JCOL )

C  Copy the sparse Hessian.

         PLHS( 1 ) = MXCREATESPARSE( n, n, nnzsh, 0 )
         iptr = PLHS( 1 )
         pr = MXGETPR( iptr )
         CALL MXCOPYREAL8TOPTR( A, pr, nnzsh )
         ir = MXGETIR( iptr )
         CALL MXCOPYINTEGER4TOPTR( IROW, ir, nnzsh )
         np1 = n + 1
         jc = MXGETJC( iptr )
         CALL MXCOPYINTEGER4TOPTR( JCOL, jc, np1 )

      ELSE IF ( EQUAL( TOOL, TGRSH ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 2 ) THEN
            msg = 'CUTEST_ugrsh requires x as input.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 2 ) THEN
            msg = 'CUTEST_ugrsh returns two output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Get the gradient and sparse Hessian evaluated at X.

         CALL CUTEST_ugrsh( status, n, X, G, nnzsh, NNZMAX, SH, 
     *                      IRNSH, ICNSH )
         IF ( status .NE. 0 ) GO TO 910

C  Make sure NNZMAX is big enough.

         IF ( nnzsh .GT. NNZMAX ) THEN
            msg = 'number of nonzeros in sparse Hessian exceeds ' // 
     *            'declared size of sparse Hessian in MEX-file.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Remove any zeros and count how many nonzeros there are in the
C  combined lower and upper triangular parts

         j = 0
         nnz = 0
         DO 113 i = 1, nnzsh
           IF ( SH( i ) .NE. 0.0D+0 ) THEN
             j = j + 1
             nnz = nnz + 1
             IF ( IRNSH( i ) .NE. ICNSH( i ) ) nnz = nnz + 1
             IRNSH( j ) = IRNSH( i )
             ICNSH( j ) = ICNSH( i )
             SH( j ) = SH( i )
           END IF
  113    CONTINUE
         nnzsh = J
         IF ( nnz .GT. NNZMAX ) THEN
            msg = 'number of nonzeros in sparse Hessian exceeds ' // 
     *            'declared size of sparse Hessian in MEX-file.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Include both lower and upper triangular parts (for Matlab)

         j = nnzsh
         DO 114 i = 1, nnzsh
           IF ( IRNSH( i ) .NE. ICNSH( i ) ) THEN
             j = j + 1
             IRNSH( j ) = ICNSH( i )
             ICNSH( j ) = IRNSH( i )
             SH( j ) = SH( i )
           END IF
  114    CONTINUE
         nnzsh = J

C  Convert from the CUTE sparse matrix to a MATLAB sparse matrix.
C  Put the MATLAB sparse matrix in A, IROW, and JCOL.

         CALL CNVSPR( n, nnzsh, SH, IRNSH, ICNSH, A, IROW, JCOL )

C  Copy G.

         PLHS( 1 ) = MXCREATEFULL( n, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( G, pr, n )

C  Copy the sparse Hessian.

         PLHS( 2 ) = MXCREATESPARSE( n, n, nnzsh, 0 )
         iptr = PLHS( 2 )
         pr = MXGETPR( iptr )
         CALL MXCOPYREAL8TOPTR( A, pr, nnzsh )
         ir = MXGETIR( iptr )
         CALL MXCOPYINTEGER4TOPTR( IROW, ir, nnzsh )
         np1 = n + 1
         jc = MXGETJC( iptr )
         CALL MXCOPYINTEGER4TOPTR( JCOL, jc, np1 )

      ELSE IF ( EQUAL( TOOL, TPROD ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 3 .AND. nrhs .NE. 4 ) THEN
            msg = 'CUTEST_uhprod requires x and another vector as ' // 
     *        'input.  The goth flag is optional.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_uhprod returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second input argument into X and third input argument into P.
C  Make sure X and P are the same length.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )
         iptr = PRHS( 3 )
         CALL GETVEC( iptr, P, nvar, nn )
         IF ( n .NE. nn ) THEN
            msg = 'x and vector to be multiplied must have same length.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Check for setting of goth flag.

         goth = .FALSE.
         IF ( nrhs .EQ. 4 ) THEN
            i = NINT( MXGETSCALAR( PRHS( 4 ) ) )
            IF ( i .NE. 0 ) goth = .TRUE.
         END IF

C  Get the product of the Hessian with P.

         CALL CUTEST_uhprod( status, n, goth, X, P, Q )
         IF ( status .NE. 0 ) GO TO 910

C  Copy Q.

         PLHS( 1 ) = MXCREATEFULL( n, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( Q, pr, n )

      ELSE IF ( EQUAL( TOOL, TBANDH ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 3 .AND. nrhs .NE. 4 ) THEN
            msg = 'CUTEST_ubandh requires x and required ' // 
     *        'semi-bandwidth as input.  The goth flag is optional.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_ubandh returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second input argument into X and third input argument into nsemib.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )
         nsemib = NINT( MXGETSCALAR( PRHS( 3 ) ) )

C  Check for setting of goth flag.

         goth = .FALSE.
         IF ( nrhs .EQ. 4 ) THEN
            i = NINT( MXGETSCALAR( PRHS( 4 ) ) )
            IF ( i .NE. 0 ) goth = .TRUE.
         END IF

C  Get the elements of the Hessian that lie within a give semi-bandwidth
C  of its diagonal.

         CALL CUTEST_ubandh( status, n, X, nsemib, BANDH, nsemib, 
     *                       maxsbw )
         IF ( status .NE. 0 ) GO TO 910

C  Copy BANDH.

         nsemp1 = nsemib + 1
         PLHS( 1 ) = MXCREATEFULL( nsemp1, n, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         nn = nsemp1 * N
         CALL MXCOPYREAL8TOPTR( BANDH, pr, nn )

      ELSE IF ( EQUAL( TOOL, TDIMEN ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 1 ) THEN
            msg = 'CUTEST_udimen does not require an input argument.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_udimen returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Build data input file name.

         prbdat = 'OUTSDIF.d'

C  Open the relevant file.

         OPEN ( input, FILE = prbdat, FORM = 'FORMATTED',
     *          STATUS = 'OLD' )
	 REWIND input

C  Call CUTEST_udimen

	 CALL CUTEST_udimen( status, input, n )
         IF ( status .NE. 0 ) GO TO 910

	 N_real = N

	 PLHS( 1 ) = MXCREATEFULL( 1, 1, 0 )
	 pr = MXGETPR( PLHS( 1 ) )
	 CALL MXCOPYREAL8TOPTR( N_real, pr, 1 )

	 CLOSE( input )

      ELSE IF ( EQUAL( TOOL, TDIMSH ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 1 ) THEN
            msg = 'CUTEST_udimsh does not require an input argument.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_udimsh returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF


C  Call CUTEST_udimsh

	 CALL CUTEST_udimsh( status, n )
         IF ( status .NE. 0 ) GO TO 910

	 N_real = N

	 PLHS( 1 ) = MXCREATEFULL( 1, 1, 0 )
	 pr = MXGETPR( PLHS( 1 ) )
	 CALL MXCOPYREAL8TOPTR( N_real, pr, 1 )

      ELSE IF ( EQUAL( TOOL, TVARTY ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 1 ) THEN
            msg = 'CUTEST_uvartype does not require an input argument.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_uvartype returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Call CUTEST_uvartype

	 CALL CUTEST_uvartype( status, nvar, IVARTY )
         IF ( status .NE. 0 ) GO TO 910

C  Convert to real

	 N_real = nvar
	 DO 121 i = 1, nvar
	   IVARTY_real( i ) = IVARTY( i )
  121    CONTINUE	 

C  Make room for output

	 PLHS( 1 ) = MXCREATEFULL( nvar, 1, 0 )
	 pr  = MXGETPR( PLHS( 1 ) )

C  Copy IVARTY

	 CALL MXCOPYREAL8TOPTR( IVARTY_real, pr, nvar )

      ELSE IF ( EQUAL( TOOL, TREPRT ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 1 ) THEN
            msg = 'CUTEST_ureport does not require an input argument.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_ureport returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Call CUTEST_ureport

	 CALL CUTEST_ureport( status, TIME, CALLS )
         IF ( status .NE. 0 ) GO TO 910

C  Make room for output - only report CALLS

	 PLHS( 1 ) = MXCREATEFULL( 4, 1, 0 )
	 pr  = MXGETPR( PLHS( 1 ) )

C  Copy CALLS

	 CALL MXCOPYREAL8TOPTR( CALLS, pr, 4 )

      ELSE
         msg = 'requested unconstrained tool does not exist:  ' // TOOL
         CALL MEXERRMSGTXT( msg )
      END IF
      RETURN

  910 CONTINUE
      WRITE( msg_status, "( I2 )" ) status
      msg = '** CUTEst error, status = ' // msg_status // ', aborting'
      CALL MEXERRMSGTXT( msg )

      END



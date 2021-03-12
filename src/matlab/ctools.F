C THIS VERSION: CUTEST 1.5 - 18/10/2016 AT 08:30 GMT.

C-*-*-  C U T E S T    C T O O L S   G A T E W A Y    S U B R O U T I N E  -*-*-

C  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
C  Principal authors: Ingrid Bongartz, Nick Gould and Dominique Orban

C  History -
C   originally released 1994
C   fortran 2003 version released in CUTEst, 16th January 2013

C  Thanks to Philip Gill for corrections

C MATLAB Fortran gateway routine to constrained CUTEst tools

      SUBROUTINE  MEXFUNCTION( nlhs, PLHS, nrhs, PRHS )
      INTEGER * 4 nlhs, nrhs
      INTEGER * 4 PLHS( * ), PRHS( * )

      INTEGER * 4 MXCREATEFULL, MXGETPR
C     INTEGER * 4 MXGETM, MXGETN
      REAL * 8    MXGETSCALAR

C  Parameters which may be changed by the user.

      INTEGER * 4      NMAX  , MMAX
C  make this dynamic!!
      PARAMETER      ( NMAX =2000, MMAX =2000 )
      INTEGER * 4      NNZMAX, NNZIMAX
      PARAMETER      ( NNZMAX = 10*NMAX, NNZIMAX = NMAX/10 )
      INTEGER * 4      input , out, io_buffer
      PARAMETER      ( input = 55, out = 6, io_buffer = 11 )

C  End of Parameters which may be changed by the user.

      CHARACTER * 64   prbdat
      CHARACTER * 10   pname
      CHARACTER * 10   XNAMES( NMAX )
      CHARACTER * 10   GNAMES( MMAX )
      INTEGER * 4      nvar, ncon, nnzsh, nnzscj, nnza, nnzb, nnzgci
      INTEGER * 4      e_order, l_order, v_order
      INTEGER * 4      EQUATN( MMAX ), LINEAR( MMAX )
      INTEGER * 4      JCOLA ( NMAX + 1 ), JCOLB( NMAX + 1 )
      INTEGER * 4      IRNSH ( NNZMAX ), ICNSH ( NNZMAX ),
     *                 INDVAR( NNZMAX ), INDFUN( NNZMAX ),
     *                 IROWA ( NNZMAX )
      INTEGER * 4      IROWB ( NMAX ), INDVARI( NNZIMAX ),
     *                 ONES( NMAX+1 )
      REAL * 8         f
      REAL * 8         X( NMAX ), BL( NMAX ), BU( NMAX ), G( NMAX ),
     *                 P( NMAX ), Q ( NMAX ), B ( NMAX )
      REAL * 8         V( MMAX ), CL( MMAX ), CU( MMAX ), C( MMAX )
      REAL * 8         CJAC( NMAX*MMAX )
      REAL * 8         DH( NMAX*NMAX ), DHI( NMAX * NMAX )
      REAL * 8         SH   ( NNZMAX ), SCJAC( NNZMAX ), A( NNZMAX ),
     *                 SGCI( NNZIMAX )
      LOGICAL          grad, goth, jtrans, grlagf

C  Common block to store problem dimensions.

      COMMON / SIZE /  nvar, ncon

C  Local variables

      INTEGER * 4      pr, ir, jc, ierr, iptr, n, nn, np1, lc1, lc2,
     *                 i, j, pr2, pr3, m, nnz
      REAL * 8         WORKN( NMAX * 10 ), WORKM( MMAX * 10 )
      INTEGER * 4      IVARTY( NMAX )
      REAL * 8         IVARTY_real( NMAX )
      CHARACTER * 2    msg_status
      CHARACTER * 7    TOOL, TSETUP, TNAMES, TFN, TGR, TOFG, TSGR,
     *                 TCFG, TLFG, TDH, TGRDH, TSH, TSGRSH, TPROD,
     *                 TCFSG, TCIFG, TDIMEN, TDIMSH, TDIMSJ, TVARTY,
     *                 TCIFSG, TIFN, TIGR, TISGR, TIDH, TISH, TSHC,
     *                 TCPROD, TREPRT
      CHARACTER * 150  msg
      REAL * 8         ci, GCI( NMAX ), N_real, M_real, NNZ_real
      REAL * 8         CALLS( 7 ), TIME( 4 )

C  External functions

      CHARACTER * 100 NULLSTR, UPPER
      LOGICAL         EQUAL

C  Check for at least one input argument,
C  which is the name of the tool to be called.

      IF ( nrhs .EQ. 0 ) THEN
         msg = 'CTOOLS requires at least one input argument.'
         CALL MEXERRMSGTXT( msg )
      END IF

C  Get name of unconstrained tool being called.
C  Null-terminate this name and convert it to upper case.

      ierr = MXGETSTRING( PRHS( 1 ), TOOL, 6 )
      IF ( ierr .EQ. 0 ) THEN
         TOOL = NULLSTR( TOOL )
         TOOL = UPPER  ( TOOL )
      ELSE
         msg = 'Error copying tool name from first input argument.'
         CALL MEXERRMSGTXT( msg )
      END IF

C  Set up tool names.

      TSETUP = NULLSTR( 'CUTEST_csetup' )
      TNAMES = NULLSTR( 'CUTEST_cnames' )
      TFN    = NULLSTR( 'CUTEST_cfn' )
      TGR    = NULLSTR( 'CUTEST_cgr' )
      TOFG   = NULLSTR( 'CUTEST_cofg' )
      TOFSG  = NULLSTR( 'CUTEST_cofsg' )
      TSGR   = NULLSTR( 'CUTEST_csgr' )
      TCFG   = NULLSTR( 'CUTEST_ccfg' )
      TCFSG  = NULLSTR( 'CUTEST_ccfsg' )
      TLFG   = NULLSTR( 'CUTEST_clfg' )
      TDH    = NULLSTR( 'CUTEST_cdh' )
      TDHC   = NULLSTR( 'CUTEST_cdhc' )
      TGRDH  = NULLSTR( 'CUTEST_cgrdh' )
      TSH    = NULLSTR( 'CUTEST_csh' )
      TSHC   = NULLSTR( 'CUTEST_cshc' )
      TSGRSH = NULLSTR( 'CUTEST_csgrsh' )
      TPROD  = NULLSTR( 'CUTEST_chprod' )
      TCPROD = NULLSTR( 'CUTEST_chcprod' )
      TIFN   = NULLSTR( 'CUTEST_cifn' )
      TIGR   = NULLSTR( 'CUTEST_cigr' )
      TISGR  = NULLSTR( 'CUTEST_cisgr' )
      TCIFG  = NULLSTR( 'CUTEST_ccifg' )
      TCIFSG = NULLSTR( 'CUTEST_ccifsg' )
      TDIMEN = NULLSTR( 'CUTEST_cdimen' )
      TDIMSH = NULLSTR( 'CUTEST_cdimsh' )
      TDIMSJ = NULLSTR( 'CUTEST_cdimsj' )
      TIDH   = NULLSTR( 'CUTEST_cidh' )
      TISH   = NULLSTR( 'CUTEST_cish' )
      TVARTY = NULLSTR( 'CUTEST_cvarty' )
      TREPRT = NULLSTR( 'CUTEST_creport' )

C  Call the requested tool.

      IF ( EQUAL( TOOL, TSETUP ) ) THEN

C  Check for proper number of arguments.

         IF ( nrhs .NE. 1 .AND. nrhs .NE. 2 ) THEN
            msg = 'CUTEST_csetup requires either zero or one ' //
     *            'input arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 6 .AND. nlhs .NE. 8 ) THEN
            msg = 'CUTEST_csetup returns either six or eight ' //
     *            'output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Set EFIRST, LFIRST, and NVFRST based on input arguments (if any).

         e_order = 0
         l_order = 0
         v_order = 0
         IF ( nrhs .EQ. 2 ) THEN
            iptr = PRHS( 2 )
            CALL GETVEC( iptr, WORKN, 3, nn )
            e_order = NINT( WORKN( 1 ) )
            l_order = NINT( WORKN( 2 ) )
            v_order = NINT( WORKN( 3 ) )
         END IF

C  Build data input file name.

         prbdat = 'OUTSDIF.d'

C  Open the relevant file.

         OPEN ( input, FILE = prbdat, FORM = 'FORMATTED',
     *          STATUS = 'OLD' )
         REWIND input

C  Set up the problem data structures.

         CALL CUTEST_csetup( status, input, out, io_buffer,
     *                nvar, ncon, X , BL, BU,
     *                V , CL, CU, EQUATN, LINEAR,
     *                e_order, l_order, v_order )
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

C  Copy V.

         PLHS( 4 ) = MXCREATEFULL( ncon, 1, 0 )
         pr = MXGETPR( PLHS( 4 ) )
         CALL MXCOPYREAL8TOPTR( V, pr, ncon )

C  Copy CL.

         PLHS( 5 ) = MXCREATEFULL( ncon, 1, 0 )
         pr = MXGETPR( PLHS( 5 ) )
         CALL MXCOPYREAL8TOPTR( CL, pr, ncon )

C  Copy CU.

         PLHS( 6 ) = MXCREATEFULL( ncon, 1, 0 )
         pr = MXGETPR( PLHS( 6 ) )
         CALL MXCOPYREAL8TOPTR( CU, pr, ncon )

C  If eight output arguments are required, copy EQUATN and LINEAR.

         IF ( nlhs .EQ. 8 ) THEN
            DO 100 i = 1, ncon
               WORKM( i ) = DFLOAT( EQUATN( i ) )
  100       CONTINUE
            PLHS( 7 ) = MXCREATEFULL( ncon, 1, 0 )
            pr = MXGETPR( PLHS( 7 ) )
            CALL MXCOPYREAL8TOPTR( WORKM, pr, ncon )
            DO 110 i = 1, ncon
               WORKM( i ) = DFLOAT( LINEAR( i ) )
  110       CONTINUE
            PLHS( 8 ) = MXCREATEFULL( ncon, 1, 0 )
            pr = MXGETPR( PLHS( 8 ) )
            CALL MXCOPYREAL8TOPTR( WORKM, pr, ncon )
         END IF

         CLOSE( input )

      ELSE IF ( EQUAL( TOOL, TNAMES ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 1 .AND. nrhs .NE. 3 ) THEN
            msg = 'CUTEST_cnames requires zero or two input arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 3 ) THEN
            msg = 'CUTEST_cnames returns three output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Get number of variable and constraint names requested.

         n = nvar
         m = ncon
         IF ( nrhs. EQ. 3 ) THEN
            n = NINT( MXGETSCALAR( PRHS( 2 ) ) )
            IF ( n .LT. 1 .OR. n .GT. nvar ) THEN
               msg = 'Invalid number of variable names requested.'
               CALL MEXERRMSGTXT( msg )
            END IF
            m = NINT( MXGETSCALAR( PRHS( 3 ) ) )
            IF ( m .LT. 1 .OR. m .GT. ncon ) THEN
               msg = 'Invalid number of constraint names requested.'
               CALL MEXERRMSGTXT( msg )
            END IF
         END IF
         CALL CUTEST_cnames( status, nvar, ncon, pname, XNAMES, GNAMES )
         IF ( status .NE. 0 ) GO TO 910

C  Copy pname, XNAMES, and GNAMES.
C  First convert XNAMES and GNAMES to REAL*8 arrays.

         PLHS( 1 ) = MXCREATESTRING( pname )
         CALL CNVCHR( XNAMES, n, 10, WORKN )
         CALL CNVCHR( GNAMES, m, 10, WORKM )
         PLHS( 2 ) = MXCREATEFULL( n, 10, 0 )
         nn = n * 10
         pr = MXGETPR( PLHS( 2 ) )
         CALL MXCOPYREAL8TOPTR( WORKN, pr, nn )
         PLHS( 3 ) = MXCREATEFULL( m, 10, 0 )
         nn = m * 10
         pr = MXGETPR( PLHS( 3 ) )
         CALL MXCOPYREAL8TOPTR( WORKM, pr, nn )

      ELSE IF ( EQUAL( TOOL, TFN ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 2 ) THEN
            msg = 'CUTEST_cfn requires x as input.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 2 ) THEN
            msg = 'CUTEST_cfn returns two output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Get the objective function value.

         CALL CUTEST_cfn( status, n, ncon, X, f, C )
         IF ( status .NE. 0 ) GO TO 910

C  Copy F.

         PLHS( 1 ) = MXCREATEFULL( 1, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( f, pr, 1 )

C  Copy C.

         PLHS( 2 ) = MXCREATEFULL( ncon, 1, 0 )
         pr = MXGETPR( PLHS( 2 ) )
         CALL MXCOPYREAL8TOPTR( C, pr, ncon )

      ELSE IF ( EQUAL( TOOL, TGR ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .LT. 2 .OR. nrhs .GT. 4 ) THEN
            msg = 'CUTEST_cgr requires one, two, or three ' //
     *            'input arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 2 ) THEN
            msg = 'CUTEST_cgr returns two output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Set defaults for grlagf and jtrans.

         jtrans = .FALSE.
         grlagf = .FALSE.

C  Copy third input argument (if given) into V.
C  Set default for grlagf to .TRUE. in this case.

         m = ncon
         IF ( nrhs .GE. 3 ) THEN
            iptr = PRHS( 3 )
            CALL GETVEC( iptr, V, ncon, m )
            grlagf = .TRUE.
         END IF

C  Adjust jtrans and grlagf based on fourth input argument, if any.

         IF ( nrhs .EQ. 4 ) THEN
            iptr = PRHS( 4 )
            CALL GETVEC( iptr, WORKN, 2, nn )
            IF ( NINT( WORKN( 1 ) ) .NE. 0 ) jtrans = .TRUE.
            IF ( NINT( WORKN( 2 ) ) .NE. 0 ) grlagf = .TRUE.
         END IF

C  Set dimensions of CJAC based on jtrans.

         IF ( .NOT. jtrans ) THEN
            lc1 = m
            lc2 = n
         ELSE
            lc1 = n
            lc2 = m
         END IF

C  Get the gradient of the objective and constraint functions.

         CALL CUTEST_cgr( status, n, m, X, V, grlagf, G, jtrans,
     *                    lc1, lc2, CJAC )
         IF ( status .NE. 0 ) GO TO 910

C  Copy G.

         PLHS( 1 ) = MXCREATEFULL( n, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( G, pr, n )

C  Copy CJAC.

         nn = lc1 * lc2
         PLHS( 2 ) = MXCREATEFULL( lc1, lc2, 0 )
         pr = MXGETPR( PLHS( 2 ) )
         CALL MXCOPYREAL8TOPTR( CJAC, pr, nn )

      ELSE IF ( EQUAL( TOOL, TSGR ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .LT. 2 .OR. nrhs .GT. 4 ) THEN
            msg = 'CUTEST_csgr requires one, two, or three ' //
     *            'input arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 2 ) THEN
            msg = 'CUTEST_csgr returns two output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Set defaults for grlagf and jtrans.

         jtrans = .FALSE.
         grlagf = .FALSE.

C  Copy third input argument (if given) into V.
C  Set default for grlagf to .TRUE. in this case.

         m = ncon
         IF ( nrhs .GE. 3 ) THEN
            iptr = PRHS( 3 )
            CALL GETVEC( iptr, V, ncon, m )
            grlagf = .TRUE.
         END IF

C  Adjust grlagf based on fourth input argument, if any.
C  Since CSGR combines the objective or Lagrangian gradient
C  with the constraint gradients in a single sparse matrix,
C  jtrans is ignored here and handled in the MATLAB function.

         IF ( nrhs .EQ. 4 ) THEN
            iptr = PRHS( 4 )
            CALL GETVEC( iptr, WORKN, 2, nn )
            IF ( NINT( WORKN( 2 ) ) .NE. 0 ) grlagf = .TRUE.
         END IF

C  Get the gradient of the objective or Lagrangian and constraint functions.

         CALL CUTEST_csgr( status, n, m, X, V, grlagf,
     *                     nnzscj, NNZMAX, SCJAC, INDVAR, INDFUN )
         IF ( status .NE. 0 ) GO TO 910

C  Make sure NNZMAX is big enough.

         IF ( nnzscj .GT. NNZMAX ) THEN
            msg = 'Number of nonzeros in sparse jacobian exceeds ' //
     *            'declared size of sparse Jacobian in MEX-file.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Convert from the CUTE sparse matrix to a MATLAB sparse matrix
C  and vector.  (The CUTE subroutine CSGR combines the gradient of
C  the objective or Lagrangian function and the gradients of the
C  general constraint functions in the single sparse matrix SCJAC.)
C  Put the MATLAB sparse Jacobian matrix in A, IROWA, and JCOLA.
C  Put the MATLAB sparse gradient vector of the objective or
C  Lagrangina function in B, IROWB, and JCOLB.

         CALL CNVSAB( n, nnzscj, SCJAC, INDFUN, INDVAR, nnza, A, IROWA,
     *                JCOLA, nnzb, B, IROWB, JCOLB )

C  Copy the sparse gradient of the objective or Lagrangian function.

         PLHS( 1 ) = MXCREATESPARSE( 1, n, nnzb, 0 )
         iptr = PLHS( 1 )
         pr = MXGETPR( iptr )
         CALL MXCOPYREAL8TOPTR( B, pr, nnzb )
         ir = MXGETIR( iptr )
         CALL MXCOPYINTEGER4TOPTR( IROWB, ir, nnzb )
         np1 = n + 1
         jc = MXGETJC( iptr )
         CALL MXCOPYINTEGER4TOPTR( JCOLB, jc, np1 )

C  Copy the sparse constraint Jacobian.

         PLHS( 2 ) = MXCREATESPARSE( m, n, nnza, 0 )
         iptr = PLHS( 2 )
         pr = MXGETPR( iptr )
         CALL MXCOPYREAL8TOPTR( A, pr, nnza )
         ir = MXGETIR( iptr )
         CALL MXCOPYINTEGER4TOPTR( IROWA, ir, nnza )
         np1 = n + 1
         jc = MXGETJC( iptr )
         CALL MXCOPYINTEGER4TOPTR( JCOLA, jc, np1 )

      ELSE IF ( EQUAL( TOOL, TOFG ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 2 ) THEN
            msg = 'CUTEST_cofg requires x as input.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 .AND. nlhs .NE. 2 ) THEN
            msg = 'CUTEST_cofg returns one or two output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Decide if the gradient is required.

         grad = .FALSE.
         IF ( nlhs .EQ. 2 ) grad = .TRUE.

C  Get the value of the objective function, and possibly its gradient.

         CALL CUTEST_cofg( status, n, X, f, G, grad )
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

      ELSE IF ( EQUAL( TOOL, TOFSG ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 2 ) THEN
            msg = 'CUTEST_cofsg requires x as input.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 .AND. nlhs .NE. 2 ) THEN
            msg = 'CUTEST_cofsg returns one or two output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Decide if the gradient is required.

         grad = .FALSE.
         IF ( nlhs .EQ. 2 ) grad = .TRUE.

C  Get the value of the objective function, and possibly its gradient.

         CALL CUTEST_cofsg( status, nvar, X, f, nnzgci, NNZIMAX, SGCI,
     *                      INDVARI, grad )
         IF ( status .NE. 0 ) GO TO 910

C  Copy F.

         PLHS( 1 ) = MXCREATEFULL( 1, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( f, pr, 1 )

C  Copy G, if it is required.

         IF ( grad ) THEN

C  Make sure NNZIMAX is big enough.

            IF ( nnzgci .GT. NNZIMAX ) THEN
               msg = 'Number of nonzeros in sparse gradient exceeds ' //
     *               'declared size of sparse gradient in MEX-file.'
               CALL MEXERRMSGTXT( msg )
            END IF

C  Substract one to indices because Matlab uses C array storage

            DO 123 j = 1, nnzgci
               INDVARI( j ) = INDVARI( j ) - 1
  123       CONTINUE

C  Set column indices for the vector, still using C arrays

            ONES( 1 ) = 0
            DO 124 j = 2, nvar
               ONES( j ) = -1
  124       CONTINUE
            ONES( nvar + 1 ) = nnzgci

C      Make room for the sparse vector SGCI

            PLHS( 2 ) = MXCREATESPARSE( 1, nvar, nnzgci, 0 )
            pr3 = PLHS( 2 )

C      Copy the values of SGCI

            pr2 = MXGETPR( pr3 )
            CALL MXCOPYREAL8TOPTR( SGCI, pr2, nnzgci )

C      Copy the row indices

            ir = MXGETIR( pr3 )
            CALL MXCOPYINTEGER4TOPTR( INDVARI, ir, nnzgci )

C      Copy the column indices

            jc = MXGETJC( pr3 )
            CALL MXCOPYINTEGER4TOPTR( ONES, jc, nvar+1 )
         END IF

      ELSE IF ( EQUAL( TOOL, TCFG ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 2 .AND. nrhs .NE. 3 ) THEN
            msg = 'CUTEST_ccfg requires x as input. The jtrans ' //
     *            'flag is optional.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 .AND. nlhs .NE. 2 ) THEN
            msg = 'CUTEST_ccfg returns one or two output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Decide if the Jacobian is required.  If yes, decide if its transpose
C  is required.

         grad = .FALSE.
         jtrans = .FALSE.
         IF ( nlhs .EQ. 2 ) grad = .TRUE.
         IF ( grad .AND. nrhs .EQ. 3 ) THEN
            i = NINT( MXGETSCALAR( PRHS( 3 ) ) )
            IF ( i .NE. 0 ) jtrans = .TRUE.
         END IF

C  Set row and column dimensions of CJAC based on jtrans.

         IF ( .NOT. jtrans ) THEN
            lc1 = ncon
            lc2 = n
         ELSE
            lc1 = n
            lc2 = ncon
         END IF

C  Get the values of the constraint functions, and possibly their gradients.

         CALL CUTEST_ccfg( status, n, ncon, X, C, jtrans,
     *                     lc1, lc2, CJAC, grad )
         IF ( status .NE. 0 ) GO TO 910

C  Copy C.

         PLHS( 1 ) = MXCREATEFULL( ncon, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( C, pr, ncon )

C  Copy CJAC, if it is required.

         IF ( grad ) THEN
            nn = lc1 * lc2
            PLHS( 2 ) = MXCREATEFULL( lc1, lc2, 0 )
            pr = MXGETPR( PLHS( 2 ) )
            CALL MXCOPYREAL8TOPTR( CJAC, pr, nn )
         END IF

      ELSE IF ( EQUAL( TOOL, TCFSG ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 2 .AND. nrhs .NE. 3 ) THEN
            msg = TOOL // ' requires x as input. The jtrans flag ' //
     *                  'is optional.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 .AND. nlhs .NE. 2 ) THEN
            msg = TOOL//' returns one or two output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Decide if the Jacobian is required.  If yes, decide if its transpose
C  is required.

         grad = .FALSE.
         jtrans = .FALSE.
         IF ( nlhs .EQ. 2 ) grad = .TRUE.
         IF ( grad .AND. nrhs .EQ. 3 ) THEN
            i = NINT( MXGETSCALAR( PRHS( 3 ) ) )
            IF ( i .NE. 0 ) jtrans = .TRUE.
         END IF

C  Set row and column dimensions of CJAC based on jtrans.

         IF ( .NOT. jtrans ) THEN
            lc1 = ncon
            lc2 = n
         ELSE
            lc1 = n
            lc2 = ncon
         END IF

C  Get the values of the constraint functions, and possibly their gradients.

         CALL CUTEST_ccfsg( status, n, ncon, X, C, nnzscj,
     *                      NNZMAX, SCJAC, INDVAR, INDFUN, grad )
         IF ( status .NE. 0 ) GO TO 910

C  Copy C.

         PLHS( 1 ) = MXCREATEFULL( ncon, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( C, pr, ncon )

C  Copy CJAC, if it is required.

         IF ( grad ) THEN

C  Convert from the CUTE sparse matrix to a MATLAB sparse matrix.
C  Put the MATLAB sparse matrix in A, IROWA, and JCOLA.
C  The roles of INDFUN and INDVAR are reversed if jtrans is .TRUE.

            IF ( .NOT. jtrans ) THEN
               CALL CNVSPR( lc2, nnzscj, SCJAC, INDFUN, INDVAR,
     *                      A, IROWA, JCOLA )
            ELSE
               CALL CNVSPR( lc2, nnzscj, SCJAC, INDVAR, INDFUN,
     *                      A, IROWA, JCOLA )
            END IF

C  Copy the sparse Jacobian.

            PLHS( 2 ) = MXCREATESPARSE( lc1, lc2, nnzscj, 0 )
            iptr = PLHS( 2 )
            pr = MXGETPR( iptr )
            CALL MXCOPYREAL8TOPTR( A, pr, nnzscj )
            ir = MXGETIR( iptr )
            CALL MXCOPYINTEGER4TOPTR( IROWA, ir, nnzscj )
            nn = lc2 + 1
            jc = MXGETJC( iptr )
            CALL MXCOPYINTEGER4TOPTR( JCOLA, jc, nn )
         END IF

      ELSE IF ( EQUAL( TOOL, TLFG ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 3 ) THEN
            msg = 'CUTEST_clfg requires x and y as input.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 .AND. nlhs .NE. 2 ) THEN
            msg = 'CUTEST_clfg returns one or two output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second and third input arguments into X and V, respectively.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )
         IF( ncon .GT. 0 ) THEN
           iptr = PRHS( 3 )
           CALL GETVEC( iptr, V, ncon, m )
         ENDIF

C  Decide if the gradient is required.

         grad = .FALSE.
         IF ( nlhs .EQ. 2 ) grad = .TRUE.

C  Get the value of the objective function, and possibly its gradient.

         CALL CUTEST_clfg( status, n, X, V, f, G, grad )
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

         IF ( nrhs .NE. 3 ) THEN
            msg = 'CUTEST_cdh requires x and Lagrange multipliers ' //
     *            'y as input.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_cdh returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second and third input arguments into X and V, respectively.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )
         IF( ncon .GT. 0 ) THEN
           iptr = PRHS( 3 )
           CALL GETVEC( iptr, V, ncon, m )
         ENDIF

C  Get the dense Hessian of the Lagrangian evaluated at X and V.

         CALL CUTEST_cdh( status, n, m, X, V, n, DH )
         IF ( status .NE. 0 ) GO TO 910

C  Copy H.

         PLHS( 1 ) = MXCREATEFULL( n, n, 0 )
         nn = n * n
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( DH, pr, nn )

      ELSE IF ( EQUAL( TOOL, TDHC ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 3 ) THEN
            msg = 'CUTEST_cdhc requires x and Lagrange multipliers ' //
     *            'y as input.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_cdhc returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second and third input arguments into X and V, respectively.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )
         IF( ncon .GT. 0 ) THEN
           iptr = PRHS( 3 )
           CALL GETVEC( iptr, V, ncon, m )
         ENDIF

C  Get the dense Hessian of the contrained part of the
C  Lagrangian evaluated at X and V.

         CALL CUTEST_cdhc( status, n, m, X, V, n, DH )
         IF ( status .NE. 0 ) GO TO 910

C  Copy H.

         PLHS( 1 ) = MXCREATEFULL( n, n, 0 )
         nn = n * n
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( DH, pr, nn )

      ELSE IF ( EQUAL( TOOL, TGRDH ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 3 .AND. nrhs .NE. 4 ) THEN
            msg = 'CUTEST_cgrdh requires x and Lagrange multipliers' //
     *            ' y as input.  The options vector is optional.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 3 ) THEN
            msg = 'CUTEST_cgrdh returns three output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second and third input arguments into X and V, respectively.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )
         iptr = PRHS( 3 )
         CALL GETVEC( iptr, V, ncon, m )

C  Adjust jtrans and grlagf based on fourth input argument, if any.

         jtrans = .FALSE.
         grlagf = .FALSE.
         IF ( nrhs .EQ. 4 ) THEN
            iptr = PRHS( 4 )
            CALL GETVEC( iptr, WORKN, 2, nn )
            IF ( NINT( WORKN( 1 ) ) .NE. 0 ) jtrans = .TRUE.
            IF ( NINT( WORKN( 2 ) ) .NE. 0 ) grlagf = .TRUE.
         END IF

C  Set dimensions of CJAC based on jtrans.

         IF ( .NOT. jtrans ) THEN
            lc1 = m
            lc2 = n
         ELSE
            lc1 = n
            lc2 = m
         END IF

C  Get the dense Hessian of the Lagrangian evaluated at X and V.

         CALL CUTEST_cgrdh( status, n, m, X, V, grlagf, G, jtrans,
     *                      lc1, lc2, CJAC, n, DH )
         IF ( status .NE. 0 ) GO TO 910

C  Copy G.

         PLHS( 1 ) = MXCREATEFULL( n, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( G, pr, n )

C  Copy CJAC.

         nn = lc1 * lc2
         PLHS( 2 ) = MXCREATEFULL( lc1, lc2, 0 )
         pr = MXGETPR( PLHS( 2 ) )
         CALL MXCOPYREAL8TOPTR( CJAC, pr, nn )

C  Copy H.

         nn = n * n
         PLHS( 3 ) = MXCREATEFULL( n, n, 0 )
         pr = MXGETPR( PLHS( 3 ) )
         CALL MXCOPYREAL8TOPTR( DH, pr, nn )

      ELSE IF ( EQUAL( TOOL, TSH ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 3 ) THEN
            msg = 'CUTEST_csh requires x and Lagrange multipliers ' //
     *            'y as input.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_csh returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second and third input arguments into X and V, respectively.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )
         IF( ncon .GT. 0 ) THEN
            iptr = PRHS( 3 )
            CALL GETVEC( iptr, V, ncon, m )
         ENDIF

C  Get the sparse Hessian of the Lagrangian evaluated at X and V.

         CALL CUTEST_csh( status, n, m, X, V,
     *                    nnzsh, NNZMAX, SH, IRNSH, ICNSH )
         IF ( status .NE. 0 ) GO TO 910

C  Make sure NNZMAX is big enough.

         IF ( nnzsh .GT. NNZMAX ) THEN
            msg = 'Number of nonzeros in sparse Hessian exceeds ' //
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
            msg = 'Number of nonzeros in sparse Hessian exceeds ' //
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
C  Put the MATLAB sparse matrix in A, IROWA, and JCOLA.

         CALL CNVSPR( n, nnzsh, SH, IRNSH, ICNSH, A, IROWA, JCOLA )

C  Copy the sparse Hessian.

         PLHS( 1 ) = MXCREATESPARSE( n, n, nnzsh, 0 )
         iptr = PLHS( 1 )
         pr = MXGETPR( iptr )
         CALL MXCOPYREAL8TOPTR( A, pr, nnzsh )
         ir = MXGETIR( iptr )
         CALL MXCOPYINTEGER4TOPTR( IROWA, ir, nnzsh )
         np1 = n + 1
         jc = MXGETJC( iptr )
         CALL MXCOPYINTEGER4TOPTR( JCOLA, jc, np1 )

      ELSE IF ( EQUAL( TOOL, TSHC ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 3 ) THEN
            msg = 'CUTEST_cshc requires x and Lagrange multipliers ' //
     *            'y as input.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_cshc returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second and third input arguments into X and V, respectively.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )
         IF( ncon .GT. 0 ) THEN
            iptr = PRHS( 3 )
            CALL GETVEC( iptr, V, ncon, m )
         ENDIF

C  Get the sparse Hessian of the contrained part of the
C  Lagrangian evaluated at X and V.

         CALL CUTEST_cshc( status, n, m, X, V,
     *                     nnzsh, NNZMAX, SH, IRNSH, ICNSH )
         IF ( status .NE. 0 ) GO TO 910

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
            msg = 'Number of nonzeros in sparse Hessian exceeds ' //
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

C  Make sure NNZMAX is big enough.

         IF ( nnzsh .GT. NNZMAX ) THEN
            msg = 'Number of nonzeros in sparse Hessian exceeds ' //
     *            'declared size of sparse Hessian in MEX-file.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Convert from the CUTE sparse matrix to a MATLAB sparse matrix.
C  Put the MATLAB sparse matrix in A, IROWA, and JCOLA.

         CALL CNVSPR( n, nnzsh, SH, IRNSH, ICNSH, A, IROWA, JCOLA )

C  Copy the sparse Hessian.

         PLHS( 1 ) = MXCREATESPARSE( n, n, nnzsh, 0 )
         iptr = PLHS( 1 )
         pr = MXGETPR( iptr )
         CALL MXCOPYREAL8TOPTR( A, pr, nnzsh )
         ir = MXGETIR( iptr )
         CALL MXCOPYINTEGER4TOPTR( IROWA, ir, nnzsh )
         np1 = n + 1
         jc = MXGETJC( iptr )
         CALL MXCOPYINTEGER4TOPTR( JCOLA, jc, np1 )

      ELSE IF ( EQUAL( TOOL, TSGRSH ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 3 .AND. nrhs .NE. 4 ) THEN
            msg = 'CUTEST_csgrsh requires x and Lagrange multipliers' //
     *            ' y as input. The options vector is optional.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 3 ) THEN
            msg = 'CUTEST_cgrdh returns three output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second and third input arguments into X and V, respectively.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )
         iptr = PRHS( 3 )
         CALL GETVEC( iptr, V, ncon, m )

C  Adjust grlagf based on fourth input argument, if any.
C  Since CSGR combines the objective or Lagrangian gradient
C  with the constraint gradients in a single sparse matrix,
C  jtrans is ignored here and handled in the MATLAB function.

         grlagf = .FALSE.
         IF ( nrhs .EQ. 4 ) THEN
            iptr = PRHS( 4 )
            CALL GETVEC( iptr, WORKN, 2, nn )
            IF ( NINT( WORKN( 2 ) ) .NE. 0 ) grlagf = .TRUE.
         END IF

C  Get the gradient of the objective or Lagrangian and constraint functions,
C  and get the sparse Hessian of the Lagrangian evaluated at X and V.

         CALL CUTEST_csgrsh( status, n, m, X, V, grlagf,
     *                       nnzscj, NNZMAX, SCJAC, INDVAR, INDFUN,
     *                       nnzsh, NNZMAX, SH, IRNSH, ICNSH )
         IF ( status .NE. 0 ) GO TO 910

C  Make sure NNZMAX is big enough for the sparse Jacobian and Hessian.

         IF ( nnzscj .GT. NNZMAX ) THEN
            msg = 'Number of nonzeros in sparse Jacobian exceeds ' //
     *            'declared size of sparse Jacobian in MEX-file.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nnzsh .GT. NNZMAX ) THEN
            msg = 'Number of nonzeros in sparse Hessian exceeds ' //
     *            'declared size of sparse Hessian in MEX-file.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Remove any zeros and count how many nonzeros there are in the
C  combined lower and upper triangular parts

         j = 0
         nnz = 0
         DO 115 i = 1, nnzsh
           IF ( SH( i ) .NE. 0.0D+0 ) THEN
             j = j + 1
             nnz = nnz + 1
             IF ( IRNSH( i ) .NE. ICNSH( i ) ) nnz = nnz + 1
             IRNSH( j ) = IRNSH( i )
             ICNSH( j ) = ICNSH( i )
             SH( j ) = SH( i )
           END IF
  115    CONTINUE
         nnzsh = J
         IF ( nnz .GT. NNZMAX ) THEN
            msg = 'Number of nonzeros in sparse Hessian exceeds ' //
     *            'declared size of sparse Hessian in MEx-FILE.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Include both lower and upper triangular parts (for Matlab)

         j = nnzsh
         DO 116 i = 1, nnzsh
           IF ( IRNSH( i ) .NE. ICNSH( i ) ) THEN
             j = j + 1
             IRNSH( j ) = ICNSH( i )
             ICNSH( j ) = IRNSH( i )
             SH( j ) = SH( i )
           END IF
  116    CONTINUE
         nnzsh = J

C  Convert from the CUTE sparse matrix to a MATLAB sparse matrix
C  and vector.  (The CUTE subroutine CSGR combines the gradient of
C  the objective or Lagrangian function and the gradients of the
C  general constraint functions in the single sparse matrix SCJAC.)
C  Put the MATLAB sparse Jacobian matrix in A, IROWA, and JCOLA.
C  Put the MATLAB sparse gradient vector of the objective or
C  Lagrangina function in B, IROWB, and JCOLB.

         CALL CNVSAB( n, nnzscj, SCJAC, INDFUN, INDVAR, nnza, A, IROWA,
     *                JCOLA, nnzb, B, IROWB, JCOLB )

C  Copy the sparse gradient of the objective or Lagrangian function.

         PLHS( 1 ) = MXCREATESPARSE( 1, n, nnzb, 0 )
         iptr = PLHS( 1 )
         pr = MXGETPR( iptr )
         CALL MXCOPYREAL8TOPTR( B, pr, nnzb )
         ir = MXGETIR( iptr )
         CALL MXCOPYINTEGER4TOPTR( IROWB, ir, nnzb )
         np1 = n + 1
         jc = MXGETJC( iptr )
         CALL MXCOPYINTEGER4TOPTR( JCOLB, jc, np1 )

C  Copy the sparse constraint Jacobian.

         PLHS( 2 ) = MXCREATESPARSE( m, n, nnza, 0 )
         iptr = PLHS( 2 )
         pr = MXGETPR( iptr )
         CALL MXCOPYREAL8TOPTR( A, pr, nnza )
         ir = MXGETIR( iptr )
         CALL MXCOPYINTEGER4TOPTR( IROWA, ir, nnza )
         np1 = n + 1
         jc = MXGETJC( iptr )
         CALL MXCOPYINTEGER4TOPTR( JCOLA, jc, np1 )

C  Convert from the CUTE sparse Hessian to a MATLAB sparse matrix.
C  Put the MATLAB sparse matrix in A, IROWA, and JCOLA.

         CALL CNVSPR( n, nnzsh, SH, IRNSH, ICNSH, A, IROWA, JCOLA )

C  Copy the sparse Hessian.

         PLHS( 3 ) = MXCREATESPARSE( n, n, nnzsh, 0 )
         iptr = PLHS( 3 )
         pr = MXGETPR( iptr )
         CALL MXCOPYREAL8TOPTR( A, pr, nnzsh )
         ir = MXGETIR( iptr )
         CALL MXCOPYINTEGER4TOPTR( IROWA, ir, nnzsh )
         np1 = n + 1
         jc = MXGETJC( iptr )
         CALL MXCOPYINTEGER4TOPTR( JCOLA, jc, np1 )

      ELSE IF ( EQUAL( TOOL, TPROD ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 4 .AND. nrhs .NE. 5 ) THEN
           msg = 'CUTEST_cprod requires x, Lagrange multipliers y ' //
     *       'and another vector p as input. The goth flag is optional.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_cprod returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second, third, and fourth input arguments into X, V, and P,
C  respectively.  Make sure X and P are the same length.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )
         iptr = PRHS( 3 )
         CALL GETVEC( iptr, V, ncon, m )
         iptr = PRHS( 4 )
         CALL GETVEC( iptr, P, nvar, nn )
         IF ( n .NE. nn ) THEN
            msg = 'x and vector to be multiplied must have same length.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Check for setting of goth flag.

         goth = .FALSE.
         IF ( nrhs .EQ. 4 ) THEN
            i = NINT( MXGETSCALAR( PRHS( 5 ) ) )
            IF ( i .NE. 0 ) goth = .TRUE.
         END IF

C  Get the product of the Hessian of the Lagrangian with P.

         CALL CUTEST_chprod( status, n, m, goth, X, V, P, Q )
         IF ( status .NE. 0 ) GO TO 910

C  Copy Q.

         PLHS( 1 ) = MXCREATEFULL( n, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( Q, pr, n )

      ELSE IF ( EQUAL( TOOL, TCPROD ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 4 .AND. nrhs .NE. 5 ) THEN
          msg = 'CUTEST_chcprod requires x, Lagrange multipliers y ' //
     *      'and another vector p as input. The goth flag is optional.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_chcprod returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy second, third, and fourth input arguments into X, V, and P,
C  respectively.  Make sure X and P are the same length.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )
         iptr = PRHS( 3 )
         CALL GETVEC( iptr, V, ncon, m )
         iptr = PRHS( 4 )
         CALL GETVEC( iptr, P, nvar, nn )
         IF ( n .NE. nn ) THEN
            msg = 'x and vector to be multiplied must have same length.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Check for setting of goth flag.

         goth = .FALSE.
         IF ( nrhs .EQ. 4 ) THEN
            i = NINT( MXGETSCALAR( PRHS( 5 ) ) )
            IF ( i .NE. 0 ) goth = .TRUE.
         END IF

C  Get the product of the Hessian of the contrained part of the
C  Lagrangian with P.

         CALL CUTEST_chcprod( status, n, m, goth, X, V, P, Q )
         IF ( status .NE. 0 ) GO TO 910

C  Copy Q.

         PLHS( 1 ) = MXCREATEFULL( n, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( Q, pr, n )

      ELSE IF ( EQUAL( TOOL, TCIFG ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 3 ) THEN
           msg = 'CUTEST_ccifg requires x and constraint index as input'
           CALL MEXERRMSGTXT( msg )
         END IF
         IF ( (nlhs .NE. 1) .AND. (nlhs .NE. 2) ) THEN
            msg = 'CUTEST_ccifg returns one or two output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy first input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Copy second input argument into I.

         i = NINT( MXGETSCALAR( PRHS( 3 ) ) )

C  Check consistency of constraint index

         IF( i .LT. 1 .OR. i .GT. ncon ) THEN
           msg = 'CUTEST_ccifg: invalid constraint index.'
           CALL MEXERRMSGTXT( msg )
         END IF

C  Decide if we want to evaluate the gradient or not
C  and make room for output.

         IF( nlhs .EQ. 2 ) THEN
            CALL CUTEST_ccifg( status, n, i, X, ci, GCI, .TRUE. )
            IF ( status .NE. 0 ) GO TO 910
            PLHS( 1 ) = MXCREATEFULL( 1, 1, 0 )
            pr2 = MXGETPR( PLHS( 1 ) )
            PLHS( 2 ) = MXCREATEFULL( n, 1, 0 )
            pr3 = MXGETPR( PLHS( 2 ) )
            CALL MXCOPYREAL8TOPTR( ci,  pr2, 1 )
            CALL MXCOPYREAL8TOPTR( GCI, pr3, n )
         ELSE
            CALL CUTEST_ccifg( status, n, i, X, ci, GCI, .FALSE. )
            IF ( status .NE. 0 ) GO TO 910
            PLHS( 1 ) = MXCREATEFULL( 1, 1, 0 )
            pr2 = MXGETPR( PLHS( 1 ) )
            CALL MXCOPYREAL8TOPTR( ci,  pr2, 1 )
         ENDIF

      ELSE IF ( EQUAL( TOOL, TCIFSG ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 3 ) THEN
            msg = 'CUTEST_ccifsg requires x and constraint index ' //
     *            'as input'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( (nlhs .NE. 1) .AND. (nlhs .NE. 2) ) THEN
            msg = 'CUTEST_ccifsg returns one or two output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy first input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Copy second input argument into I.

         i = NINT( MXGETSCALAR( PRHS( 3 ) ) )

C  Check consistency of constraint index

         IF( (I .LT. 1) .OR. (I .GT. ncon) ) THEN
            msg = 'CUTEST_ccifsg: invalid constraint index.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Decide if we want to evaluate the gradient or not
C  and make room for output.

         IF( nlhs .EQ. 2 ) THEN
            CALL CUTEST_ccifsg( status, nvar, i, X, ci,
     *                          nnzgci, NNZIMAX, SGCI, INDVARI, .TRUE. )
           IF ( status .NE. 0 ) GO TO 910

C  Copy ci

            PLHS( 1 ) = MXCREATEFULL( 1, 1, 0 )
            pr = MXGETPR( PLHS( 1 ) )
            CALL MXCOPYREAL8TOPTR( ci, pr, 1 )

C  Make sure NNZIMAX is big enough.

            IF ( nnzgci .GT. NNZIMAX ) THEN
               msg = 'Number of nonzeros in sparse gradient exceeds ' //
     *               'declared size of sparse gradient in MEX-file.'
               CALL MEXERRMSGTXT( msg )
            END IF

C  Substract one to indices because Matlab uses C array storage

            DO 117 j = 1, nnzgci
               INDVARI( j ) = INDVARI( j ) - 1
  117       CONTINUE

C  Set column indices for the vector, still using C arrays

            ONES( 1 ) = 0
            DO 118 j = 2, nvar
               ONES( j ) = -1
  118       CONTINUE
             ONES( nvar + 1 ) = nnzgci

C      Make room for the sparse vector SGCI

            PLHS( 2 ) = MXCREATESPARSE( 1, nvar, nnzgci, 0 )
            pr3 = PLHS( 2 )

C      Copy the values of SGCI

            pr2 = MXGETPR( pr3 )
            CALL MXCOPYREAL8TOPTR( SGCI, pr2, nnzgci )

C      Copy the row indices

            ir = MXGETIR( pr3 )
            CALL MXCOPYINTEGER4TOPTR( INDVARI, ir, nnzgci )

C      Copy the column indices

            jc = MXGETJC( pr3 )
            CALL MXCOPYINTEGER4TOPTR( ONES, jc, nvar+1 )
         ELSE
            CALL CUTEST_ccifsg( status, n, i, X, ci, nnzgci,
     *                          NNZIMAX, SGCI, INDVARI, .FALSE. )
            IF ( status .NE. 0 ) GO TO 910
            PLHS( 1 ) = MXCREATEFULL( 1, 1, 0 )
            pr = MXGETPR( PLHS( 1 ) )
            CALL MXCOPYREAL8TOPTR( ci, pr, 1 )
         ENDIF

      ELSE IF ( EQUAL( TOOL, TIFN ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 3 ) THEN
           msg = 'CUTEST_cifn requires x and problem function index ' //
     *           'as input'
           CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_cifn returns one output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy first input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Copy second input argument into I.

         i = NINT( MXGETSCALAR( PRHS( 3 ) ) )

C  Check consistency of problem index

         IF( i .LT. 0 .OR. i .GT. ncon ) THEN
           msg = 'CUTEST_cifn: invalid problem function index.'
           CALL MEXERRMSGTXT( msg )
         END IF

C  evaluate the problem function

         CALL CUTEST_cifn( status, n, i, X, ci )
         IF ( status .NE. 0 ) GO TO 910
         PLHS( 1 ) = MXCREATEFULL( 1, 1, 0 )
         pr2 = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( ci,  pr2, 1 )

      ELSE IF ( EQUAL( TOOL, TIGR ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 3 ) THEN
           msg = 'CUTEST_cigr requires x and problem function index ' //
     *           'as input'
           CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_cigr returns one output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy first input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Copy second input argument into I.

         i = NINT( MXGETSCALAR( PRHS( 3 ) ) )

C  Check consistency of problem function index

         IF( i .LT. 0 .OR. i .GT. ncon ) THEN
           msg = 'CUTEST_cigr: invalid problem function index.'
           CALL MEXERRMSGTXT( msg )
         END IF

C  evaluate the problem gradient

         CALL CUTEST_cigr( status, n, i, X, GCI )
         IF ( status .NE. 0 ) GO TO 910
         PLHS( 1 ) = MXCREATEFULL( 1, 1, 0 )
         pr2 = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( GCI, pr2, n )

      ELSE IF ( EQUAL( TOOL, TISGR ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 3 ) THEN
           msg = 'CUTEST_cisgr requires x and problem function index ' //
     *           'as input'
           CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_cisgr returns one output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy first input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Copy second input argument into I.

         i = NINT( MXGETSCALAR( PRHS( 3 ) ) )

C  Check consistency of problem function index

         IF ( I .LT. 0 .OR. I .GT. ncon ) THEN
            msg = 'CUTEST_cisgr: invalid problem function index.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Decide if we want to evaluate the gradient or not
C  and make room for output.

         CALL CUTEST_cisgr( status, nvar, i, X,
    *                       nnzgci, NNZIMAX, SGCI, INDVARI )
         IF ( status .NE. 0 ) GO TO 910

C  Make sure NNZIMAX is big enough.

         IF ( nnzgci .GT. NNZIMAX ) THEN
            msg = 'Number of nonzeros in sparse gradient exceeds ' //
     *            'declared size of sparse gradient in MEX-file.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Substract one to indices because Matlab uses C array storage

         DO 119 j = 1, nnzgci
            INDVARI( j ) = INDVARI( j ) - 1
  119    CONTINUE

C  Set column indices for the vector, still using C arrays

         ONES( 1 ) = 0
         DO 120 j = 2, nvar
            ONES( j ) = - 1
  120    CONTINUE
         ONES( nvar + 1 ) = nnzgci

C      Make room for the sparse vector SGCI

         PLHS( 1 ) = MXCREATESPARSE( 1, nvar, nnzgci, 0 )
         pr3 = PLHS( 1 )

C      Copy the values of SGCI

         pr2 = MXGETPR( pr3 )
         CALL MXCOPYREAL8TOPTR( SGCI, pr2, nnzgci )

C      Copy the row indices

         ir = MXGETIR( pr3 )
         CALL MXCOPYINTEGER4TOPTR( INDVARI, ir, nnzgci )

C      Copy the column indices

         jc = MXGETJC( pr3 )
         CALL MXCOPYINTEGER4TOPTR( ONES, jc, nvar+1 )

      ELSE IF ( EQUAL( TOOL, TDIMEN ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 1 ) THEN
            msg = 'CUTEST_cdimen requires no input input arguments'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 2 ) THEN
            msg = 'CUTEST_cdimen returns two output arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Build data input file name.

         prbdat = 'OUTSDIF.d'

C  Open the relevant file.

         OPEN ( input, FILE = prbdat, FORM = 'FORMATTED',
     *          STATUS = 'OLD' )
         REWIND( input )

C  Call CDIMEN

         CALL CUTEST_cdimen( status, input, n, m )
         IF ( status .NE. 0 ) GO TO 910

         N_real = N
         M_real = M

         PLHS( 1 ) = MXCREATEFULL( 1, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( N_real, pr, 1 )
         PLHS( 2 ) = MXCREATEFULL( 1, 1, 0 )
         pr2 = MXGETPR( PLHS( 2 ) )
         CALL MXCOPYREAL8TOPTR( M_real, pr2, 1 )

         CLOSE( input )

      ELSE IF ( EQUAL( TOOL, TDIMSH ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 1 ) THEN
            msg = 'CUTEST_cdimsh does not require an input argument.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_cdimsh returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Call CDIMSH

         CALL CUTEST_cdimsh( status, nnz )

         NNZ_real = NNZ

         PLHS( 1 ) = MXCREATEFULL( 1, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( NNZ_real, pr, 1 )

      ELSE IF ( EQUAL( TOOL, TDIMSJ ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 1 ) THEN
            msg = 'CUTEST_cdimsj does not require an input argument.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_cdimsj returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Call CDIMSJ

         CALL CUTEST_cdimsj( status, nnz )
         IF ( status .NE. 0 ) GO TO 910

         NNZ_real = NNZ

         PLHS( 1 ) = MXCREATEFULL( 1, 1, 0 )
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( NNZ_real, pr, 1 )

      ELSE IF ( EQUAL( TOOL, TIDH ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 3 ) THEN
           msg = 'CUTEST_cidh takes x and constraint index ' //
     *           'as arguments.'
           CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_cidh returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy first input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Copy second input argument into I.

         i = NINT( MXGETSCALAR( PRHS( 3 ) ) )

C  Check consistency of constraint index. I=0 -> objective function.

         IF( (I .LT. 0) .OR. (I .GT. ncon) ) THEN
            msg = 'CUTEST_cidh: invalid constraint index.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Call CIDH

         CALL CUTEST_cidh( status, nvar, X, i, n, DHI )
         IF ( status .NE. 0 ) GO TO 910

C  Copy DHI.

         PLHS( 1 ) = MXCREATEFULL( n, n, 0 )
         nn = n * n
         pr = MXGETPR( PLHS( 1 ) )
         CALL MXCOPYREAL8TOPTR( DHI, pr, nn )

      ELSE IF ( EQUAL( TOOL, TISH ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 3 ) THEN
           msg = 'CUTEST_cish takes x and constraint index ' //
     *           'as arguments.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_cish returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Copy first input argument into X.

         iptr = PRHS( 2 )
         CALL GETVEC( iptr, X, nvar, n )

C  Copy second input argument into I.

         i = NINT( MXGETSCALAR( PRHS( 3 ) ) )

C  Check consistency of constraint index. I=0 -> objective function.

         IF( (I .LT. 0) .OR. (I .GT. ncon) ) THEN
            msg = 'CUTEST_cish: Invalid constraint index.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Call CISH

         CALL CUTEST_cish( status, nvar, X, i,
     *                     nnzsh, NNZMAX, SH, IRNSH, ICNSH )
         IF ( status .NE. 0 ) GO TO 910

C  Make sure NNZMAX is big enough.

         IF ( nnzsh .GT. NNZMAX ) THEN
            msg = 'CUTEST_cish: number of nonzeros in sparse Hessian' //
     *        ' exceeds declared size of sparse Hessian in MEX-file.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Remove any zeros and count how many nonzeros there are in the
C  combined lower and upper triangular parts

         j = 0
         nnz = 0
         DO 121 i = 1, nnzsh
           IF ( SH( i ) .NE. 0.0D+0 ) THEN
             j = j + 1
             nnz = nnz + 1
             IF ( IRNSH( i ) .NE. ICNSH( i ) ) nnz = nnz + 1
             IRNSH( j ) = IRNSH( i )
             ICNSH( j ) = ICNSH( i )
             SH( j ) = SH( i )
           END IF
  121    CONTINUE
         nnzsh = J
         IF ( nnz .GT. NNZMAX ) THEN
           msg = 'Number of nonzeros in sparse Hessian exceeds ' //
     *           'declared size of sparse Hessian in MEX-file.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Include both lower and upper triangular parts (for Matlab)

         j = nnzsh
         DO 122 i = 1, nnzsh
           IF ( IRNSH( i ) .NE. ICNSH( i ) ) THEN
             j = j + 1
             IRNSH( j ) = ICNSH( i )
             ICNSH( j ) = IRNSH( i )
             SH( j ) = SH( i )
           END IF
  122    CONTINUE
         nnzsh = J

C  Convert from the CUTE sparse matrix to a MATLAB sparse matrix.
C  Put the MATLAB sparse matrix in A, IROWA, and JCOLA.

         CALL CNVSPR( n, nnzsh, SH, IRNSH, ICNSH, A, IROWA, JCOLA )

C  Copy the sparse Hessian.

         PLHS( 1 ) = MXCREATESPARSE( n, n, nnzsh, 0 )
         iptr = PLHS( 1 )
         pr = MXGETPR( iptr )
         CALL MXCOPYREAL8TOPTR( A, pr, nnzsh )
         ir = MXGETIR( iptr )
         CALL MXCOPYINTEGER4TOPTR( IROWA, ir, nnzsh )
         np1 = n + 1
         jc = MXGETJC( iptr )
         CALL MXCOPYINTEGER4TOPTR( JCOLA, jc, np1 )

      ELSE IF ( EQUAL( TOOL, TVARTY ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 1 ) THEN
            msg = 'CUTEST_cvarty does not require an input argument.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_cvarty returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Call CVARTY

         CALL CUTEST_cvartype( status, nvar, IVARTY )
         IF ( status .NE. 0 ) GO TO 910

C  Convert to real

         N_real = nvar
         DO 125 i = 1, nvar
           IVARTY_real( i ) = IVARTY( i )
  125      CONTINUE

C  Make room for output

         PLHS( 1 ) = MXCREATEFULL( nvar, 1, 0 )
         pr  = MXGETPR( PLHS( 1 ) )

C  Copy IVARTY

         CALL MXCOPYREAL8TOPTR( IVARTY_real, pr, nvar )

      ELSE IF ( EQUAL( TOOL, TREPRT ) ) THEN

C  Check number of input and output arguments.

         IF ( nrhs .NE. 1 ) THEN
            msg = 'CUTEST_creport does not require an input argument.'
            CALL MEXERRMSGTXT( msg )
         END IF
         IF ( nlhs .NE. 1 ) THEN
            msg = 'CUTEST_creport returns one output argument.'
            CALL MEXERRMSGTXT( msg )
         END IF

C  Call CUTEST_creport

	 CALL CUTEST_creport( status, CALLS, TIME )
         IF ( status .NE. 0 ) GO TO 910

C  Make room for output - only report CALLS

	 PLHS( 1 ) = MXCREATEFULL( 4, 1, 0 )
	 pr  = MXGETPR( PLHS( 1 ) )

C  Copy CALLS

	 CALL MXCOPYREAL8TOPTR( CALLS, pr, 7 )

      ELSE
         msg = 'Requested constrained tool does not exist:  ' // TOOL
         CALL MEXERRMSGTXT( msg )
      END IF
      RETURN

  910 CONTINUE
      WRITE( msg_status, "( I2 )" ) status
      msg = '** CUTEst error, status = ' // msg_status // ', aborting'
      CALL MEXERRMSGTXT( msg )
      END




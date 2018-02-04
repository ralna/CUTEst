C THIS VERSION: CUTEST 1.0 - 16/01/2013 AT 10:20 GMT.

C-*-*-  C U T E S T    G T O O L S   G A T E W A Y    S U B R O U T I N E  -*-*-

C  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
C  Principal authors: Ingrid Bongartz, Nick Gould and Dominique Orban

C  History -
C   originally released 1994
C   fortran 2003 version released in CUTEst, 16th January 2013

      SUBROUTINE GETVEC( iptr, X, nmax, n )
      INTEGER * 4       iptr, nmax, n
      REAL * 8          X( nmax )

C  Copy input argument pointed to by iptr into the target vector X.
C  Make sure the input argument is a real * 8 vector, and that
C  it doesn't exceed the length of the target vector.
C  Put the length of the input vector in N.

C  MEX functions

      INTEGER * 4 MXGETPR, MXGETM, MXGETN, MXISDOUBLE

C  Local variables

      INTEGER * 4       nrows, ncols, m, PR
      CHARACTER ( LEN = 100 ) msg

      nrows = MXGETM( iptr )
      ncols = MXGETN( iptr )
      m = MIN( nrows, ncols )
      n = MAX( nrows, ncols )
      IF ( m .NE. 1 ) THEN
        msg = 'Input argument must be a vector.'
        CALL MEXERRMSGTXT( msg )
      END IF
      IF ( n .GT. nmax ) THEN
        msg = 'Input argument is too long.'
        CALL MEXERRMSGTXT( msg )
      END IF
      IF ( MXISDOUBLE( iptr ) .EQ. 0 ) THEN
        msg = 'Input argument must be REAL * 8.'
        CALL MEXERRMSGTXT( msg )
      END IF
      PR = MXGETPR( iptr )
      CALL MXCOPYPTRTOREAL8( PR, X, n )
      RETURN
      END

      SUBROUTINE CNVSPR( ncol, nnzsh, SH, IRNSH, ICNSH, A, IROW, JCOL )
      INTEGER * 4       ncol, nnzsh
      INTEGER * 4       IRNSH( nnzsh ), ICNSH( nnzsh ), IROW( nnzsh )
      INTEGER * 4       JCOL( ncol + 1 )
      REAL * 8          SH( nnzsh ), A( nnzsh )

C  Convert the CUTE sparse matrix described by SH, IRNSH and ICNSH
C  to the MATLAB sparse matrix described by A, IROW and JCOL.

C  Local variables

      INTEGER * 4 ncolp1, i, j, k, jj

C  Zero out JCOL and then use it to store the number of nonzero
C  entries in column j of the sparse matrix.

      ncolp1 = ncol + 1
      DO 100 j = 1, ncolp1
        JCOL( j ) = 0
  100 CONTINUE
      DO 110 k = 1, nnzsh
        j = ICNSH( k )
        JCOL( j ) = JCOL( j ) + 1
  110 CONTINUE
      JCOL( ncolp1 ) = nnzsh + 1

C  Now go backwards through JCOL to find the starting index for the
C  nonzero entries in column J.

      DO 120 j = ncol, 1, -1
        JCOL( j ) = JCOL( j + 1 ) - JCOL( j )
  120 CONTINUE

C  Copy the entries from SH and IRNSH into the correct entries
C  in A and IROW.  Since MATLAB is written in C, subtract 1 from
C  each of the entries in IROW.  Use JCOL to keep track of what
C  place we're in for each column.

      DO 130 k = 1, nnzsh
        i = IRNSH( k )
        j = ICNSH( k )
        jj = JCOL( j )
        A( jj ) = SH( k )
        IROW( jj ) = i  - 1
        JCOL( j ) = jj + 1
  130 CONTINUE

C  Restore the entries in JCOL to the starting index for the nonzero
C  entries in each column.  Since MATLAB is written in C, subtract 1
C  from each of the entries in JCOL.

      DO 140 j = ncol, 2, -1
        JCOL( j ) = JCOL( j - 1 ) - 1
  140 CONTINUE
      JCOL( 1 ) = 0
      JCOL( ncolp1 ) = JCOL( ncolp1 ) - 1
      RETURN
      END

      SUBROUTINE CNVSAB( ncol, nnzsh, SH, IRNSH, ICNSH, nnza, A, IROWA,
     *                   JCOLA, nnzb, B, IROWB, JCOLB )
      INTEGER * 4 ncol, nnzsh, nnza, nnzb
      INTEGER * 4 IRNSH( nnzsh ), ICNSH( nnzsh ), IROWA( nnzsh )
      INTEGER * 4 IROWB( ncol )
      INTEGER * 4 JCOLA( ncol + 1 ), JCOLB( ncol + 1 )
      REAL * 8 SH( nnzsh ), A( nnzsh ), B( ncol )

C  The CUTE sparse matrix described by SH, IRNSH and ICNSH contains
C  both a sparse matrix and a sparse vector.  The k-th nonzero entry
C  in SH belongs to the sparse vector if IRNSH(k)=0, and belongs
C  to row i of the sparse matrix if IRNSH(k)=i>0.
C  This subroutine returns the MATLAB sparse matrix described by
C  nnza, A, IROWA, JCOLA, and the MATLAB sparse vector described by
C  nnzb, B, IROWB, JCOLB.

C  Local variables

      INTEGER * 4 ncolp1, i, j, k, jj

C  Zero out JCOLA and JCOLB.  Use JCOLA to store the number of nonzero
C  entries in column j of the sparse matrix, and use JCOLB to store
C  the number of nonzero entries in column j of the sparse vector.

      ncolp1 = ncol + 1
      DO 100 j = 1, ncolp1
        JCOLA( j ) = 0
        JCOLB( j ) = 0
  100 CONTINUE
      nnza = 0
      nnzb = 0
      DO 110 k = 1, nnzsh
        i = IRNSH( k )
        j = ICNSH( k )
        IF ( i .GT. 0 ) THEN
          JCOLA( j ) = JCOLA( j ) + 1
          nnza = nnza + 1
        ELSE
          JCOLB( j ) = JCOLB( j ) + 1
          nnzb = nnzb + 1
        END IF
  110 CONTINUE
      JCOLA( ncolp1 ) = nnza + 1
      JCOLB( ncolp1 ) = nnzb + 1

C  Now go backwards through JCOLA and JCOLB to find the starting index
C  for the nonzero entries in column J.

      DO 120 j = ncol, 1, -1
        JCOLA( j ) = JCOLA( j + 1 ) - JCOLA( j )
        JCOLB( j ) = JCOLB( j + 1 ) - JCOLB( j )
  120 CONTINUE

C  Copy the entries from SH and IRNSH into the correct entries
C  in A and IROWA, or B if IRNSH(k)=0.  Since MATLAB is written
C  in C, subtract 1 from each of the entries in IROWA.  Set all
C  of the entries in IROWB=0.  Use JCOLA and JCOLB to keep track
C  of what place we're in for each column.

      DO 130 k = 1, nnzsh
        i = IRNSH( k )
        j = ICNSH( k )
        IF ( i .GT. 0 ) THEN
          jj = JCOLA( j )
          A( jj ) = SH( k )
          IROWA( jj ) = i - 1
          JCOLA( j  ) = jj + 1
        ELSE
          jj = JCOLB( j )
          B( jj ) = SH( k )
          IROWB( jj ) = 0
          JCOLB( j  ) = jj + 1
        END IF
  130 CONTINUE

C  Restore the entries in JCOLA and JCOLB to the starting index for
C  the nonzero entries in each column.  Since MATLAB is written in C,
C  subtract 1 from each of the entries in JCOLA and JCOLB.

      DO 140 j = ncol, 2, - 1
        JCOLA( j ) = JCOLA( j - 1 ) - 1
        JCOLB( j ) = JCOLB( j - 1 ) - 1
  140 CONTINUE
      JCOLA( 1 ) = 0
      JCOLB( 1 ) = 0
      JCOLA( ncolp1 ) = JCOLA( ncolp1 ) - 1
      JCOLB( ncolp1 ) = JCOLB( ncolp1 ) - 1
      RETURN
      END

      SUBROUTINE CNVCHR( CNAMES, NWORD, LWORD, WORK )
      CHARACTER*(*) CNAMES( NWORD )
      INTEGER * 4 nword, lword
      REAL * 8 WORK( nword, lword )

C  Convert individual character entries in character array CNAMES
C  to the floating point values of their position in the collating
C  sequence (i.e., convert to floating point ASCII values).
C  Put these values in the REAL * 8 array WORK.

      INTEGER i, j
      DO 120 i = 1, nword
        DO 100 j = 1, lword
          WORK( i, j ) = DFLOAT( ICHAR( CNAMES( i )( j : j ) ) )
  100   CONTINUE
  120 CONTINUE
      RETURN
      END

      CHARACTER*(*) FUNCTION UPPER( string )
      CHARACTER*(*) string

C     UPPER - convert string to upper case except where enclosed in quotes

C     Source:
C     Fortran tools for VAX/VMS and MS-DOS
C     by Russell K. Jones and Tracy Crabtree,
C     John Wiley & Sons, 1988.

      CHARACTER ( LEN = 1 ) null, quote
      INTEGER i

      null = CHAR( 0 )
      quote = CHAR( 39 )
      i = 1
      DO WHILE ( STRING( i : i ) .NE. null )
        IF ( STRING( i : i ) .EQ. quote ) THEN
          UPPER( i : i ) = quote
          i = i + 1
          DO WHILE ( STRING( i : i ) .NE. quote .AND. 
     &               STRING( i : i ) .NE. null )
            UPPER( i : i ) = STRING( i : i )
            i = i + 1
          END DO
          UPPER( i : i ) = STRING( i : i )
          IF ( STRING( i : i ) .NE. null ) i = i + 1
        ELSE
          IF ( LLT( STRING( i : i ), 'a' ) .OR. 
     &         LGT( STRING( i : i ), 'z' ) ) THEN
            UPPER( i : i ) = STRING( i : i )
          ELSE
            UPPER( i : i ) = CHAR( ICHAR( 'A' ) - ICHAR( 'a' ) 
     &                             + ICHAR( STRING( i : i ) ) )
          END IF
          i = i + 1
        END IF
      END DO
      UPPER( i : i ) = null
      RETURN
      END

      CHARACTER*(*) FUNCTION NULLSTR( STR )
      CHARACTER*(*) STR

C     NULLSTR - null-terminate an unterminated string

C     SOURCE:
C     FORTRAN TOOLS FOR VAX/VMS AND MS-DOS
C     BY RUSSELL K. JONES AND TRACY CRABTREE,
C     JOHN WILEY & SONS, 1988.

      CHARACTER ( LEN = 1 ) null, blank
      INTEGER  i
      null = CHAR( 0 )
      blank = CHAR( 32 )
      i = LEN( STR )
      DO WHILE ( i .GT. 0 .AND. STR( i : i ) .EQ. blank )
        i = i - 1
      END DO
      IF ( i .EQ. 0 ) THEN
        NULLSTR = null
      ELSE
        NULLSTR = STR( 1 : i ) // null
      END IF
      RETURN
      END

      LOGICAL FUNCTION EQUAL( S, T )
      CHARACTER*(*) S, T

C     EQUAL - compare two strings for equality

C     Source:
C     Fortran tools for VAX/VMS and MS-DOS
C     by Russell K. Jones and Tracy Crabtree,
C     John Wiley & Sons, 1988.

      CHARACTER ( LEN = 1 ) null
      INTEGER i
      null = CHAR(0)
      i = 1
      DO WHILE ( S( i : i ) .NE. null )
        IF ( S( i : i ) .NE. T( i : i ) ) THEN
          EQUAL = .FALSE.
          RETURN
        END IF
        i = i + 1
      END DO
      EQUAL = T( i : i ) .EQ. null
      RETURN
      END

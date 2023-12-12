! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      PROGRAM TRNMA

C  TRON test driver for problems derived from SIF files.
C
C  Nick Gould, for CGT Productions.
C  September 2004.
C  Revised for CUTEst, January 2013
C
      USE CUTEST_KINDS_precision
      IMPLICIT NONE
      INTEGER ( KIND = ip_ ) LH, I, out, N, INPUT, J, MAXIT, L, P
      INTEGER ( KIND = ip_ ) IFLAG, INSPEC, NNZH, NNZH2, status
      INTEGER ( KIND = ip_ ) ISAVE( 3 )
      INTEGER ( KIND = ip_ ) :: io_buffer = 11
      REAL ( KIND = rp_ ) F, GTOL  , GNORM , DSAVE( 3 )
      REAL ( KIND = rp_ ) FRTOL, FATOL, FMIN, CGTOL, DELTA, GNORM0
      CHARACTER ( LEN = 60 ) :: TASK
      PARAMETER ( out  = 6 )
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: HPTR, HROW
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: LPTR, LROW
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: BPTR, BROW
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IFREE, IWA
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, XL, XU, G, 
     *                 XC, S, WA, HVAL, HDIAG, LVAL, LDIAG, BVAL, BDIAG
      PARAMETER ( INPUT = 55, INSPEC = 56 )
      CHARACTER ( LEN = 10 ) :: PNAME, SPCDAT
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: XNAMES
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 4 )
      REAL ( KIND = rp_ ) DGPNRM2, DNRM2
      EXTERNAL DTRON, DGPNRM2
C     
C  Open the Spec file for the method.
C
      SPCDAT = 'TRON.SPC'
      OPEN ( INSPEC, FILE = SPCDAT, FORM = 'FORMATTED',
     *      STATUS = 'OLD' )
      REWIND INSPEC
C
C  Read input Spec data.
C
C     P        : the number of vectors to hold factorization fill-in
C     MAXIT    : the maximum number of iterations,
C     FATOL    : the absolute error desired in the function
C     FRTOL    : the relative error desired in the function
C     FMIN     : a lower bound for the function
C     CGTOL    : the relative CG decrease required per iteration
C     GTOL     : the required norm of the projected gradient
C
      READ ( INSPEC, 1000 ) P, MAXIT, FATOL, FRTOL, FMIN, CGTOL, GTOL
C
C  Close input file.
C
      CLOSE ( INSPEC )
C
C  Open the relevant file.
C
      OPEN ( INPUT, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND INPUT
C
C  Check to see if there is sufficient room
C
      CALL CUTEST_udimen_r( status, INPUT, N )
      IF ( status /= 0 ) GO TO 910

      ALLOCATE( HPTR( n + 1 ), LPTR( n + 1 ),
     *          BPTR( n + 1 ), IFREE( n ), IWA( 3 * n ),
     *          X( n ), XL( n ), XU( n ), G( n ), 
     *          XC( n ), S( n ), WA( 7 * n ),
     *          HDIAG( n ), LDIAG( n ), BDIAG( n ),
     *          XNAMES( n ), STAT = status )
      IF ( status /= 0 ) GO TO 990
C
C  Set up SIF data.
C
      CALL CUTEST_usetup_r( status, INPUT, out, io_buffer, N, 
     *                      X, XL, XU )
      IF ( status /= 0 ) GO TO 910
C
C  Check to see if there is sufficient room for the matrices
C
      CALL CUTEST_udimsh_r( status, NNZH )
      IF ( status /= 0 ) GO TO 910

      ALLOCATE( HROW( nnzh ), BROW( nnzh ),
     *          HVAL( nnzh ), BVAL( nnzh ),
     *          LVAL( nnzh + n * p ), LROW( nnzh + n * p ),
     *          STAT = status )
      IF ( status /= 0 ) GO TO 990
      lh = nnzh
C
C  Obtain variable names.
C
      CALL CUTEST_unames_r( status, N, PNAME, XNAMES )
      IF ( status /= 0 ) GO TO 910
C
C  Set up algorithmic input data.
C
      IFLAG  = 0
C
C  Optimization loo
C
      TASK = 'START'
   30 CONTINUE
C
C  Evaluate the function, F

C
         IF (TASK( 1: 1 ) .EQ. 'F' .OR. 
     *       TASK( 1: 5 ) .EQ. 'START' ) THEN
            CALL CUTEST_ufn_r( status, N, X, F )
            IF ( status /= 0 ) GO TO 910
         END IF
C
C  Evaluate the gradient, G, and Hessian, H. 
C  NB. The lower triangular part of H is required, 
C  and the row indices are stored in BROW
C
         IF (TASK( 1: 2 ) .EQ. 'GH' .OR. 
     *       TASK( 1: 5 ) .EQ. 'START' ) THEN
            CALL CUTEST_ugrsh_r( status, N, X, G, NNZH, LH, HVAL, 
     *                         BROW, HROW  )
            IF ( status /= 0 ) GO TO 910
C
C  Separate the diagonal of the Hessian from its off diagonal
C
            DO 40 I = 1, N
              HDIAG( I ) = 0.0_rp_
   40       CONTINUE
            NNZH2 = NNZH
            NNZH = 0
            DO 50 L = 1, NNZH2
              I = HROW( L ) 
              J = BROW( L )
              IF ( I .EQ. J ) THEN
                 HDIAG( I ) = HVAL( L ) 
              ELSE
                 NNZH = NNZH + 1
                 HROW( NNZH ) = I
                 BROW( NNZH ) = J
                 HVAL( NNZH ) = HVAL( L )
              END IF
   50       CONTINUE
C
C  Move the Hessian from co-ordinate to sparse row format
C
            CALL REORDA( N, NNZH, HROW, BROW, HVAL, HPTR, BPTR )
         END IF
C
C           Initialize the trust region bound.
C
         IF ( TASK( 1: 5 ) .EQ. 'START' ) THEN
            GNORM0 = MAX( DNRM2( N, G, 1 ), 1.0_rp_ )
            DELTA  = GNORM0
         END IF
C
C  Call the optimizer
C
         CALL DTRON( N, X, XL, XU, F, G, HVAL, HDIAG, HPTR, HROW,
     *               FRTOL, FATOL, FMIN, CGTOL, MAXIT, DELTA, TASK,
     *               BVAL, BDIAG, BPTR, BROW,
     *               LVAL, LDIAG, LPTR, LROW,
     *               XC, S, IFREE, ISAVE, DSAVE, WA, IWA )
C
C  Test for convergence
C 
        IF ( TASK( 1: 4 ) .EQ. 'CONV' ) THEN
           IFLAG = 0
         ELSE IF (  TASK( 1: 4 ) .EQ. 'WARN' ) THEN
           IFLAG = 1
         ELSE IF (  TASK( 1: 4 ) .EQ. 'NEWX' ) THEN
           GNORM = DGPNRM2( N, X, XL, XU, G )
           IF ( GNORM .LE. GTOL ) THEN
C          IF ( GNORM .LE. GTOL * GNORM0 ) THEN
              IFLAG = 0
           ELSE
             GO TO 30
           END IF
         ELSE
            GO TO 30
         END IF
C
C  Terminal exit.
C
      CALL CUTEST_ureport_r( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910
      GNORM = DGPNRM2( N, X, XL, XU, G )
      WRITE ( out, 2010 ) F, GNORM
      DO 120 I = 1, N
         WRITE( out, 2020 ) XNAMES( I ), XL( I ), X( I ), XU( I ), 
     *                       G( I )
  120 CONTINUE
      WRITE ( out, 2000 ) PNAME, N, INT( CALLS(1) ), INT( CALLS(2) ),
     *                     IFLAG, F, CPU(1), CPU(2) 
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
C
C  Non-executable statements.
C
 1000 FORMAT( 2( I10, / ), 4( D10.3, / ), D10.3 )
 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *    ,' Package used            :  TRON',     /
     *    ,' Problem                 :  ', A10,    /
     *    ,' # variables             =      ', I10 /
     *    ,' # objective functions   =      ', I10 /
     *    ,' # objective gradients   =      ', I10 / 
     *     ' Exit code               =      ', I10 /
     *    ,' Final f                 = ', E15.7 /
     *    ,' Set up time             =      ', 0P, F10.2, ' seconds' /
     *     ' Solve time              =      ', 0P, F10.2, ' seconds' //
     *     66('*') / )
 2010 FORMAT( ' Final objective function value   = ', 1P, D12.4, 
     *        /, ' Final norm of projected gradient = ', 1P, D12.4,
     *        //, '                XL           X        XU', 
     *           '           G ' )
 2020 FORMAT(  1X, A10, 1P, 4D12.4 )
      END

      SUBROUTINE REORDA( NC, NNZ, IRN, JCN, A, IP, IW )
      USE CUTEST_KINDS_precision
      INTEGER NC, NNZ
      INTEGER IRN( NNZ  ), JCN( NNZ )
      INTEGER IW( NC + 1 ), IP( NC + 1 )
      REAL ( KIND = rp_ )  A( NNZ )

C  Sort a sparse matrix from arbitrary order to column order

C  Nick Gould
C  7th November, 1990

      INTEGER I, J, K, L, IC, NCP1, ITEMP, JTEMP,  LOCAT
      REAL ( KIND = rp_ ) ANEXT , ATEMP

C  Initialize the workspace as zero

      NCP1       = NC + 1
      DO 10 J    = 1, NCP1
         IW( J ) = 0
   10 CONTINUE

C  Pass 1. Count the number of elements in each column

      DO 20 K   = 1, NNZ
        J       = JCN( K )
        IW( J ) = IW( J ) + 1
   20 CONTINUE

C  Put the positions where each column begins in
C  a compressed collection into IP and IW

      IP( 1 )       = 1
      DO 30 J       = 2, NCP1
        IP( J )     = IW( J - 1 ) + IP( J - 1 )
        IW( J - 1 ) = IP( J - 1 )
   30 CONTINUE

C  Pass 2. Reorder the elements into column order. 
C          Fill in each column in turn

      DO 70 IC = 1, NC

C  Consider the next unfilled position in column IC

        DO 60 K = IW( IC ), IP( IC + 1 ) - 1

C  The entry should be placed in column J

          I       = IRN( K )
          J       = JCN( K )
          ANEXT   = A( K )
          DO 40 L = 1, NNZ

C  See if the entry is already in place

             IF ( J .EQ. IC ) GO TO 50
             LOCAT = IW( J )

C  As a new entry is placed in column J, increase the pointer 
C  IW( J ) by one

             IW( J  ) = LOCAT + 1

C  Record details of the entry which currently occupies location LOCAT

             ITEMP = IRN( LOCAT )
             JTEMP = JCN( LOCAT )
             ATEMP = A( LOCAT )

C  Move the new entry to its correct place

             IRN( LOCAT ) = I 
             JCN( LOCAT ) = J  
             A( LOCAT )   = ANEXT

C  Make the displaced entry the new entry

             I          = ITEMP
             J          = JTEMP
             ANEXT      = ATEMP
   40     CONTINUE

C  Move the new entry to its correct place 

   50     CONTINUE
          JCN( K ) = J
          IRN( K ) = I
          A( K )   = ANEXT
   60   CONTINUE
   70 CONTINUE

      RETURN

C  End of REORDA

      END

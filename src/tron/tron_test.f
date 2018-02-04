C     ( Last modified on 4 Jan 2013 at 16:10:00 )

C  Dummy DTRON for testing tron_main interface to CUTEst
C  Nick Gould,  4th January 2013

      SUBROUTINE DTRON( n, X, XL, XU, f, G, HVAL, HDIAG, HPTR, HROW,
     *               frtol, fatol, fmin, cgtol, maxit, delta, TASK,
     *               BVAL, BDIAG, BPTR, BROW,
     *               LVAL, LDIAG, LPTR, LROW,
     *               XC, S, IFREE, ISAVE, DSAVE, WA, IWA )
      INTEGER n, maxit, ISAVE( 3 )
      DOUBLE PRECISION f, frtol, fatol, fmin, cgtol, delta, DSAVE( 3 )
      CHARACTER ( LEN = 60 ) :: TASK

      INTEGER HPTR( n + 1 ), LPTR( n + 1 ),
     *          BPTR( n + 1 ), IFREE( n ), IWA( 3 * n ),
     *          HROW( * ), BROW( * ), LROW( * )
      DOUBLE PRECISION X( n ), XL( n ), XU( n ), G( n ),
     *          XC( n ), S( n ), WA( 7 * n ),
     *          HDIAG( n ), LVAL( * ), HVAL( * ), BVAL( * ),
     *          LDIAG( n ), BDIAG( n )

      IF ( TASK( 1: 5 ) .EQ. 'START' ) THEN
        TASK( 1: 5 ) = 'GH   '
      ELSE
        TASK( 1: 5 ) = 'CONV '
      END IF
      RETURN
      END

      DOUBLE PRECISION FUNCTION DGPNRM2(n,x,xl,xu,g)
      INTEGER n
      DOUBLE PRECISION x(n), xl(n), xu(n), g(n)
      INTEGER i
      dgpnrm2 = 0.0D+0
      DO i = 1, n
        IF (xl(i) .ne. xu(i)) THEN
          IF (x(i) .eq. xl(i)) THEN
             dgpnrm2 = dgpnrm2 + min(g(i),0.0D+0)**2
          ELSE IF (x(i) .eq. xu(i)) THEN
             dgpnrm2 = dgpnrm2 + max(g(i),0.0D+0)**2
          ELSE
             dgpnrm2 = dgpnrm2 + g(i)**2
          END IF
        END IF
      END DO
      dgpnrm2 = sqrt(dgpnrm2)
      RETURN
      END

      DOUBLE PRECISION FUNCTION dnrm2(n,x,lx)
      INTEGER n, lx
      DOUBLE PRECISION x(n)
      INTEGER i
      dnrm2 = 0.0D+0
      DO i = 1, n
        dnrm2 = dnrm2 + x(i)**2
      END DO
      dnrm2 = sqrt(dnrm2)
      return
      END

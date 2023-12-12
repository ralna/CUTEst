! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"

C  Dummy DTRON for testing tron_main interface to CUTEst

C  Nick Gould,  4th January 2013

      SUBROUTINE DTRON( n, X, XL, XU, f, G, HVAL, HDIAG, HPTR, HROW,
     *                  frtol, fatol, fmin, cgtol, maxit, delta, TASK,
     *                  BVAL, BDIAG, BPTR, BROW,
     *                  LVAL, LDIAG, LPTR, LROW,
     *                  XC, S, IFREE, ISAVE, DSAVE, WA, IWA )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) n, maxit, ISAVE( 3 )
      REAL ( KIND = rp_ ) f, frtol, fatol, fmin, cgtol, delta
      REAL ( KIND = rp_ ) DSAVE( 3 )
      CHARACTER ( LEN = 60 ) :: TASK

      INTEGER ( KIND = ip_ ) HPTR( n + 1 ), LPTR( n + 1 )
      INTEGER ( KIND = ip_ ) BPTR( n + 1 ), IFREE( n ), IWA( 3 * n )
      INTEGER ( KIND = ip_ ) HROW( * ), BROW( * ), LROW( * )
      REAL ( KIND = rp_ ) X( n ), XL( n ), XU( n ), G( n )
      REAL ( KIND = rp_ ) XC( n ), S( n ), WA( 7 * n )
      REAL ( KIND = rp_ ) HDIAG( n ), LVAL( * ), HVAL( * ), BVAL( * )
      REAL ( KIND = rp_ ) LDIAG( n ), BDIAG( n )

      IF ( TASK( 1: 5 ) .EQ. 'START' ) THEN
        TASK( 1: 5 ) = 'GH   '
      ELSE
        TASK( 1: 5 ) = 'CONV '
      END IF
      RETURN
      END

      REAL ( KIND = rp_ ) FUNCTION DGPNRM2(n,x,xl,xu,g)
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) n
      REAL ( KIND = rp_ ) x(n), xl(n), xu(n), g(n)
      INTEGER ( KIND = ip_ ) i
      dgpnrm2 = 0.0_rp_
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

      REAL ( KIND = rp_ ) FUNCTION dnrm2(n,x,lx)
      USE CUTEST_KINDS_precision
      INTEGER  ( KIND = ip_ ) n, lx
      REAL ( KIND = rp_ ) x(n)
      INTEGER ( KIND = ip_ ) i
      dnrm2 = 0.0_rp_
      DO i = 1, n
        dnrm2 = dnrm2 + x(i)**2
      END DO
      dnrm2 = sqrt(dnrm2)
      return
      END

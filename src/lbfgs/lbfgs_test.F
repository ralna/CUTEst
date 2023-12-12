C  THIS VERSION: CUTEST 2.2 - 2023-11-29 AT 15:00 GMT.

#include "cutest_modules.h"

C  Dummy LBFGS for testing lbfgs_main interface to CUTEst
C  Nick Gould,  4th January 2013

      SUBROUTINE LBFGS( n, m, X, f, G, DIAGCO, DIAG, IPRINT, eps, xtol,
     *                  W, iflag )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) n, m, iflag, iprint( 2 )
      REAL ( KIND = rp_ ) f, eps, xtol
      LOGICAL diagco
      REAL ( KIND = rp_ ) X( n ), G( n ), DIAG( n ), 
     *                    W( n * ( 2 * m + 1 ) + 2 * m )
      IF ( iflag == 0 ) THEN
        iflag = 1
      ELSE
        iflag = 0
      END IF
      RETURN
      END

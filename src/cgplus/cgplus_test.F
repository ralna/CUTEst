C  THIS VERSION: CUTEST 2.2 - 2023-11-29 AT 14:40 GMT.

#include "cutest_modules.h"

C  Dummy CGFAM for testing cgplus_main interface to CUTEst
C  Nick Gould,  4th January 2013

      SUBROUTINE CGFAM( n, X, f, G, D, GOLD, IPRINT, eps, W,
     *               iflag, irest, method, finish )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) n, iflag, irest, method, IPRINT( 2 )
      REAL ( KIND = rp_ ) f, eps
      LOGICAL finish
      REAL ( KIND = rp_ ) X( n ), G( n ), D( n ), GOLD( n ), W( n )

      iflag = iflag + 1
      RETURN
      END

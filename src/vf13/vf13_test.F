! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"

C  Dummy VF13AD for testing vf13_main interface to CUTEst

C  Nick Gould,  4th January 2013

      SUBROUTINE VF13AD( n, m, meq, X, f, G, C, CN, lcn, maxfun,
     *                  acc, iprint, inf, W, lw, IW )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n, m, meq, lcn, maxfun, iprint, inf, lw
      REAL ( KIND = rp_ ) :: f, acc
      INTEGER ( KIND = ip_ ) :: IW( n + 1 )
      REAL ( KIND = rp_ ) :: X( n ), G( n ), C( m ), CN( lcn, m )
      REAL ( KIND = rp_ ) :: W( lw )

      IF ( inf .EQ. - 1 ) THEN
        inf = 0
      ELSE
        inf = 1
      END IF
      RETURN
      END


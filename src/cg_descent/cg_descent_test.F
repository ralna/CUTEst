! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"

C  Dummy CG_DECSCENT for testing cg_descent_main interface to CUTEst

C  Nick Gould,  4th January 2013

      SUBROUTINE CG_DESCENT( tol, X, n, EVALF, EVALG, stat, gnorm, f,
     *                 iter, nf, ng, D, G, XTEMP, GTEMP )
      USE CUTEST_KINDS_precision
      INTEGER :: n, iter, nf, ng, stat
      REAL ( KIND = rp_ ) :: f, tol, gnorm 
      EXTERNAL EVALF, EVALG
      REAL ( KIND = rp_ ) :: X( n ), G( n ), D( n )
      REAL ( KIND = rp_ ) :: XTEMP( n ), GTEMP( n )

      WRITE( 6, "( ' Calling CG_DESCENT_dummy with n = ', I0 )" ) n
      CALL EVALF( f, X, n )
      CALL EVALG( G, X, n )
      WRITE( 6, "( ' Leaving CG_DESCENT_dummy ' )" )
      RETURN
      END


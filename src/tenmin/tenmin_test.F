C THIS VERSION: CUTEST 2.2 - 2023-11-26 AT 15:10 GMT.

#include "cutest_modules.h"

C  Dummy TENSOR for testing tenmin_main interface to CUTEst
C  Nick Gould, 5th January 2013

      SUBROUTINE TENSOR( nr, n, X, EVALF, EVALG, EVALH, TYPSIZ,
     *             fscale, gradtl, steptl, ilim, stepmx, out,
     *             method, iagflg, iahflg, ndigit, msg, XPLS,
     *             fpls, GPLS, H, itnno, WRK, IWRK  )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: nr, n, method, msg, ndigit, ilim, itnno
      INTEGER ( KIND = ip_ ) :: iagflg, iahflg, out
      REAL ( KIND = rp_ ) :: gradtl, steptl, fscale, fpls, stepmx
      INTEGER ( KIND = ip_ ) :: IWRK( n )
      REAL ( KIND = rp_ ) :: X( n ), TYPSIZ( n ), XPLS( n ), GPLS( n )
      REAL ( KIND = rp_ ) :: H( nr, n ), WRK( nr, 8 )
      EXTERNAL :: EVALF, EVALG, EVALH
      INTEGER ( KIND = ip_ ) :: i
      DO 10 i = 1, n
        XPLS( i ) = X( i )
   10 CONTINUE
      CALL EVALF( n, XPLS, fpls )
      CALL EVALG( n, X, GPLS )
      CALL EVALH( nr, n, XPLS, H )
      itnno = 4
      RETURN
      END

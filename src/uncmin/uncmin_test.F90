! THIS VERSION: CUTEST 2.2 - 2023-11-07 AT 11:20 GMT.

#include "cutest_modules.h"

!  Dummy OPTIF9 for testing uncmin_main interface to CUTEst
!  Nick Gould, 5th January 2013

      SUBROUTINE OPTIF9( nr, n, X, EVALF, EVALG, EVALH, TYPSIZ,                &
                         fscale, method, iexp, msg, ndigit, ilim,              &
                         iagflg, iahflg, out, dlt, gradtl, stp,                &
                         steptl, XPLS, fpls, GPLS, itrmcd, A, WRK )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: nr, n, method, iexp, msg, ndigit, ilim, itrmcd
      INTEGER ( KIND = ip_ ) :: out, iagflg, iahflg
      REAL ( KIND = rp_ ) :: dlt, gradtl, stp, steptl, fscale, fpls
      REAL ( KIND = rp_ ) :: X( n ), TYPSIZ( n ), XPLS( n ), GPLS( n )
      REAL ( KIND = rp_ ) :: A( nr, n ), WRK( nr, 8 )
      EXTERNAL :: EVALF, EVALG, EVALH
      INTEGER ( KIND = ip_ ) :: i
      DO i = 1, n
        XPLS( i ) = X( i )
      END DO
      CALL EVALF( n, XPLS, fpls )
      CALL EVALG( n, X, GPLS )
      CALL EVALH( nr, n, XPLS, A )
      itrmcd = 4
      RETURN
      END SUBROUTINE OPTIF9

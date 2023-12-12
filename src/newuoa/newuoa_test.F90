! THIS VERSION: CUTEST 2.2 - 2023-11-16 AT 13:25 GMT.

#include "cutest_modules.h"

!  Dummy NEWUOA for testing newuoa_main interface to CUTEst
!  Nick Gould, 29th January 2013

      SUBROUTINE NEWUOA( n, npt, X, rhobeg, rhoend, iprint, maxfun, W )
      USE CUTEST_KINDS_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ) :: n, npt, iprint, maxfun
      REAL( KIND = rp_ ) :: rhobeg, rhoend
      REAL( KIND = rp_ ) :: X( * ), W( * )

      REAL( KIND = rp_ ) :: f
      CALL CALFUN( n, X, f )

      RETURN
      END

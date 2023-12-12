! THIS VERSION: CUTEST 2.2 - 2023-11-16 AT 11:45 GMT.

#include "cutest_modules.h"

!  Dummy BOBYQA for testing bobyqa_main interface to CUTEst
!  Nick Gould, 27th January 2013

      SUBROUTINE BOBYQA( n, npt, X, X_l, X_u,                                  &
                         rhobeg, rhoend, iprint, maxfun, W )
      USE CUTEST_KINDS_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ) :: n, npt, iprint, maxfun
      REAL( KIND = rp_ ) :: rhobeg, rhoend
      REAL( KIND = rp_ ) :: X( * ), X_l( * ), X_u( * ), W( * )

      REAL( KIND = rp_ ) :: f
      CALL CALFUN( n, X, f )

      RETURN
      END

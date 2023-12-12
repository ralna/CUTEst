! THIS VERSION: CUTEST 2.2 - 2023-11-16 AT 13:15 GMT.

#include "cutest_modules.h"

!  Dummy LINCOA for testing lincoa_main interface to CUTEst
!  Nick Gould, 29th January 2013

      SUBROUTINE LINCOA( n, npt, m, A, ia, B, X, rhobeg, rhoend, iprint,       &
                         maxfun, W )
      USE CUTEST_KINDS_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ) :: n, npt, m, ia, iprint, maxfun
      REAL( KIND = rp_ ) :: rhobeg, rhoend
      REAL( KIND = rp_ ) :: A( ia, * ), B( * ), X( * ), W( * )

      REAL( KIND = rp_ ) :: f
      CALL CALFUN( n, X, f )

      RETURN
      END

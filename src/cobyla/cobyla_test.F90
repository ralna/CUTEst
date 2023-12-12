! THIS VERSION: CUTEST 2.2 - 2023-11-16 AT 12:25 GMT.

#include "cutest_modules.h"

!  Dummy COBYLA for testing cobyla_main interface to CUTEst
!  Nick Gould, 7th January 2013

      SUBROUTINE COBYLA( n, m, X, rhobeg, rhoend, iprint, maxfun, W, IW )
      USE CUTEST_KINDS_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ) :: n, m, iprint, maxfun
      REAL( KIND = rp_ ) :: rhobeg, rhoend
      INTEGER ( KIND = ip_ ) :: IW( * )
      REAL( KIND = rp_ ) :: X( * ), W( * )

      REAL( KIND = rp_ ) :: f
      REAL( KIND = rp_ ) :: C( m )
      CALL CALCFC( n, m, X, f, C )

      RETURN
      END

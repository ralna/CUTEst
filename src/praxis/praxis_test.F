! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"

C  Dummy PRAXIS etc testing praxis_main interface to CUTEst

C  Nick Gould, 6th January 2013

      SUBROUTINE PRAXIS( EVALF )
      USE CUTEST_KINDS_precision
      EXTERNAL :: EVALF
      COMMON / CPRAX / V, X, D, Q0, Q1, dmin, epsmch, fx, h, qd0, qd1,
     *                 qf1, small, t, xldt, xm2, xm4, dseed, scbd, n,
     *                 nl, nf, lp, jprint, nmx, illcin, ktm, nfmax,
     *                 jranch
      INTEGER ( KIND = ip_ ), PARAMETER :: nmax = 1000
      INTEGER ( KIND = ip_ ) :: n, nl, nf, lp, jprint, illcin, ktm, i
      INTEGER ( KIND = ip_ ) :: nfmax, jranch, nmx, status
      REAL ( KIND = rp_ ) :: dmin, epsmch, fx, h, qd0, qd1, qf1
      REAL ( KIND = rp_ ) :: small, t, xldt, xm2, xm4, dseed, scbd
      REAL ( KIND = rp_ ), DIMENSION( nmax ) :: X, D, Q0, Q1
      REAL ( KIND = rp_ ), DIMENSION( nmax, nmax ) :: V
      REAL ( KIND = rp_ ) :: PRAXIS_evalf
      EXTERNAL :: PRAXIS_evalf
      fx = PRAXIS_evalf( X( : n ), n )
      RETURN
      END

      SUBROUTINE RANINI( rvalue )
      USE CUTEST_KINDS_precision
      REAL ( KIND = rp_ ) :: rvalue
      RETURN
      END

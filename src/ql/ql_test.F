! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"

!  Dummy QL for testing ql_main interface to CUTEst

!  Nick Gould, 20th February 2013

      SUBROUTINE QL( m, me, mmax, n, nmax, mnn, C, D, A, B,     
     &               XL, XU, X, U, eps, mode, iout, ifail, iprint,
     &               WAR, lwar, IWAR, liwar )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: m, me, iout, mode, ifail, iprint
      INTEGER ( KIND = ip_ ) :: nmax, mmax, n, mnn, lwar, liwar
      INTEGER ( KIND = ip_ ) :: IWAR( liwar )
      REAL ( KIND = rp_ ) :: eps
      REAL ( KIND = rp_ ) :: C( nmax, n ), D( n ), B( mmax ), U( mnn )
      REAL ( KIND = rp_ ) :: XL( n ), XU( n ), X( n ), WAR( lwar )
      REAL ( KIND = rp_ ) :: A( mmax, n )
      ifail = 1
      RETURN
      END

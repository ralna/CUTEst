! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"

!  Dummy NLPQLP for testing ql_main interface to CUTEst

!  Nick Gould, 22nd February 2013

      SUBROUTINE NLPQLP ( l, m, me, mmax, n, nmax, mnn2, X, F, G,
     &                    DF, DG, U, XL, XU, C, D, acc, accqp, stpmin,
     &                    maxfun, maxit, maxnm, rho, iprint, mode, iout,
     &                    ifail, WA, lwa, KWA, lkwa, ACTIVE, lactiv,
     &                    lql, QL )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: m, me, l, n, mmax, nmax, mnn2
      INTEGER ( KIND = ip_ ) :: lwa, lkwa, lactiv, iout, ifail
      INTEGER ( KIND = ip_ ) :: maxfun, maxit, maxnm, iprint, mode
      REAL ( KIND = rp_ ) :: acc, accqp, stpmin, rho
      LOGICAL :: lql
      INTEGER ( KIND = ip_ ) :: KWA( lkwa )
      REAL ( KIND = rp_ ) :: X( NMAX, L ),F( L ), G( MMAX, L )
      REAL ( KIND = rp_ ) :: DF( NMAX ), DG( mmax, nmax ), U( mnn2 )
      REAL ( KIND = rp_ ) :: XL( n ), XU( n )
      REAL ( KIND = rp_ ) :: C( nmax, nmax ), D( nmax ), WA( lwa )
      LOGICAL ACTIVE( lactiv )
      EXTERNAL :: QL
      IF ( ifail == 0 ) THEN
        ifail = - 1
      ELSE IF ( ifail == - 1 ) THEN
        ifail = - 2
      ELSE
        KWA( 1 ) = 2
        KWA( 2 ) = 2
        KWA( 3 ) = 1
        KWA( 4 ) = 0
        ifail = 1
      END IF
      RETURN
      END

      SUBROUTINE QL( m, me, mmax, n, nmax, mnn, C, D, A, B,
     &               XL, XU, X, U, eps, mode, iout, ifail, iprint,
     &               WAR, lwar, IWAR, liwar )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: m, me, iout, mode, ifail, iprint
      INTEGER ( KIND = ip_ ) :: nmax, mmax, n, mnn, lwar, liwar
      INTEGER ( KIND = ip_ ) :: IWAR( liwar )
      REAL ( KIND = rp_ ) :: eps
      REAL ( KIND = rp_ ) :: C( nmax, n ), D( n )
      REAL ( KIND = rp_ ) :: A( mmax, n ), B( mmax ), WAR( lwar )
      REAL ( KIND = rp_ ) :: XL( n ), XU( n ),X( n ), U( mnn )
      ifail = 1
      RETURN
      END

!     ( Last modified on 22 Feb 2013 at 10:00:00 )

!  Dummy NLPQLP for testing ql_main interface to CUTEst
!  Nick Gould, 22nd February 2013

      SUBROUTINE NLPQLP ( l, m, me, mmax, n, nmax, mnn2, X, F, G,
     &                    DF, DG, U, XL, XU, C, D, acc, accqp, stpmin,
     &                    maxfun, maxit, maxnm, rho, iprint, mode, iout,
     &                    ifail, WA, lwa, KWA, lkwa, ACTIVE, lactiv,
     &                    lql, QL )
      INTEGER :: m, me, l, n, mmax, nmax, mnn2, lwa, lkwa, lactiv
      INTEGER :: maxfun, maxit, maxnm, iprint, mode, iout, ifail
      DOUBLE PRECISION ::  acc, accqp, stpmin, rho
      LOGICAL :: lql
      INTEGER :: KWA( lkwa )
      DOUBLE PRECISION :: X( NMAX, L ),F( L ),G( MMAX, L ), DF( NMAX )
      DOUBLE PRECISION :: DG( mmax, nmax ), U( mnn2 ), XL( n ),XU( n )
      DOUBLE PRECISION :: C( nmax, nmax ), D( nmax ), WA( lwa )
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
      INTEGER :: m, me, iout, mode, ifail, iprint
      INTEGER :: nmax, mmax, n, mnn, lwar, liwar
      INTEGER :: IWAR( liwar )
      DOUBLE PRECISION :: eps
      DOUBLE PRECISION :: C( nmax, n ), D( n ), A( mmax, n ), B( mmax )
      DOUBLE PRECISION :: XL( n ), XU( n ),X( n ), U( mnn ), WAR( lwar )
      ifail = 1
      RETURN
      END

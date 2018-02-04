!     ( Last modified on 20 Feb 2013 at 15:50:00 )

!  Dummy QL for testing ql_main interface to CUTEst
!  Nick Gould, 20th February 2013

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

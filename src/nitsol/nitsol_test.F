! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"

C  Dummy NITSOL for testing nitsol_main interface to CUTEst

C  Nick Gould,  7th January 2013

      SUBROUTINE NITSOL( n, X, evalf, evalj, ftol, stptol, 
     *     INPUT, INFO, RWORK, RPAR, IPAR, iterm, dinpr, dnorm )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) n, INPUT( 10 ), INFO( 6 ), IPAR( * ), iterm 
      REAL ( KIND = rp_ ) X( n ), ftol, stptol, RWORK( * ), RPAR( * ) 
      REAL ( KIND = rp_ ) C( n ), V( n ), Z( n )
      REAL ( KIND = rp_ ) dinpr, dnorm
      EXTERNAL evalf, evalj, dinpr, dnorm
      INTEGER ( KIND = ip_ ) :: ijob, itrmf, itrmjv, i
      CALL evalf( n, X, C, RPAR, IPAR, itrmf )
      ijob = 0
      DO 10 i = 1, n
        V( i ) = 1.0_rp_
   10 CONTINUE
      CALL evalj( n, X, C, ijob, V, Z, RPAR, IPAR, itrmjv )
      iterm = 1
      INFO( 1 ) = 1
      INFO( 2 ) = 1
      INFO( 3 ) = 0
      INFO( 4 ) = 1
      INFO( 5 ) = 1
      INFO( 6 ) = 0
      RETURN
      END

      REAL ( KIND = rp_ ) FUNCTION NITSOL_dot( n, X, incx, Y, incy )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: incx, incy, n
      REAL ( KIND = rp_ ) X( * ), Y( * )
      INTEGER ( KIND = ip_ ) :: i
      NITSOL_dot = 0.0_rp_
      DO 10 i = 1, n
        NITSOL_dot = NITSOL_dot + X( i ) * Y( i )
   10 CONTINUE
      RETURN
      END

      REAL ( KIND = rp_ ) FUNCTION NITSOL_norm2( n, X, incx )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: incx, n
      REAL ( KIND = rp_ ) X( * )
      REAL ( KIND = rp_ ) NITSOL_dot
      NITSOL_norm2 = SQRT( NITSOL_dot( n, X, incx, X, incx ) )
      RETURN
      END

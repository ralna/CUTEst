C     ( Last modified on 7 Jan 2013 at 07:25:00 )

C  Dummy NITSOL for testing nitsol_main interface to CUTEst
C  Nick Gould,  7th January 2013

      SUBROUTINE NITSOL( n, X, evalf, evalj, ftol, stptol, 
     *     INPUT, INFO, RWORK, RPAR, IPAR, iterm, dinpr, dnorm )
      integer n, INPUT( 10 ), INFO( 6 ), IPAR( * ), iterm 
      DOUBLE PRECISION X( n ), ftol, stptol, RWORK( * ), RPAR( * ) 
      DOUBLE PRECISION C( n ), V( n ), Z( n )
      DOUBLE PRECISION dinpr, dnorm
      EXTERNAL evalf, evalj, dinpr, dnorm
      INTEGER :: ijob, itrmf, itrmjv, i
      CALL evalf( n, X, C, RPAR, IPAR, itrmf )
      ijob = 0
      DO 10 i = 1, n
        V( i ) = 1.0D+0
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

      DOUBLE PRECISION FUNCTION NITSOL_dot( n, X, incx, Y, incy )
      INTEGER incx, incy, n
      DOUBLE PRECISION X( * ), Y( * )
      INTEGER :: i
      NITSOL_dot = 0.0D+0
      DO 10 i = 1, n
        NITSOL_dot = NITSOL_dot + X( i ) * Y( i )
   10 CONTINUE
      RETURN
      END

      DOUBLE PRECISION FUNCTION NITSOL_norm2( n, X, incx )
      INTEGER incx, n
      DOUBLE PRECISION X( * )
      DOUBLE PRECISION NITSOL_dot
      NITSOL_norm2 = SQRT( NITSOL_dot( n, X, incx, X, incx ) )
      RETURN
      END

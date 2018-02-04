      SUBROUTINE STUMCD( n, X, npairs, IRN, lirn, ICN, licn, EVALF,
     *                   EVALG, EVALSH, TYPX, fscale, gradtl, steptl,
     *                   ilim, stepmx, ipr, method, grdflg, hsnflg,
     *                   ndigit, msg, XPLS, fpls, GPLS, HESS, WRK,
     *                   lwrk, IWRK, liwrk, termcd, VECTOR, inform )
      INTEGER :: n, npairs, lirn, licn, ilim, ipr, method, inform
      INTEGER :: grdflg, hsnflg, ndigit, msg, lwrk, liwrk, termcd
      DOUBLE PRECISION :: fscale, gradtl,steptl, stepmx, fpls
      INTEGER :: IRN( lirn ), ICN( licn ), IWRK( liwrk )
      DOUBLE PRECISION :: X( N ), TYPX( N )
      DOUBLE PRECISION :: XPLS( n ), GPLS( n ), HESS( licn )
      DOUBLE PRECISION :: WRK( lwrk ), VECTOR( n )
      EXTERNAL :: EVALF, EVALG, EVALSH
      INTEGER :: i
      DO 10 i = 1, n
        XPLS( i ) = X( i )
   10 CONTINUE
      CALL EVALF( n, XPLS, fpls )
      CALL EVALG( n, X, GPLS )
      CALL EVALSH( n, XPLS, npairs, lirn, HESS, IRN, ICN )
      termcd = 0
      RETURN
      END

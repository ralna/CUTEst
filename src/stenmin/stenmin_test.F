C THIS VERSION: CUTEST 2.2 - 2023-11-26 AT 15:30 GMT.

#include "cutest_modules.h"

      SUBROUTINE STUMCD( n, X, npairs, IRN, lirn, ICN, licn, EVALF,
     *                   EVALG, EVALSH, TYPX, fscale, gradtl, steptl,
     *                   ilim, stepmx, ipr, method, grdflg, hsnflg,
     *                   ndigit, msg, XPLS, fpls, GPLS, HESS, WRK,
     *                   lwrk, IWRK, liwrk, termcd, VECTOR, inform )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n, npairs, lirn, licn, ilim, ipr, method
      INTEGER ( KIND = ip_ ) :: grdflg, hsnflg, ndigit, msg, lwrk, liwrk
      INTEGER ( KIND = ip_ ) :: inform, termcd
      REAL ( KIND = rp_ ) :: fscale, gradtl,steptl, stepmx, fpls
      INTEGER ( KIND = ip_ ) :: IRN( lirn ), ICN( licn ), IWRK( liwrk )
      REAL ( KIND = rp_ ) :: X( N ), TYPX( N )
      REAL ( KIND = rp_ ) :: XPLS( n ), GPLS( n ), HESS( licn )
      REAL ( KIND = rp_ ) :: WRK( lwrk ), VECTOR( n )
      EXTERNAL :: EVALF, EVALG, EVALSH
      INTEGER ( KIND = ip_ ) :: i
      DO 10 i = 1, n
        XPLS( i ) = X( i )
   10 CONTINUE
      CALL EVALF( n, XPLS, fpls )
      CALL EVALG( n, X, GPLS )
      CALL EVALSH( n, XPLS, npairs, lirn, HESS, IRN, ICN )
      termcd = 0
      RETURN
      END

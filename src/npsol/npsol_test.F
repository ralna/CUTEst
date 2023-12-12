! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"

C  Dummy NPSOL for testing npsol_main interface to CUTEst

C  Nick Gould,  11th January 2013

      subroutine npsol ( n, nclin, ncnln, ldA, ldJu, ldR,
     $                   A, bl, bu,
     $                   funcon, funobj,
     $                   inform, iter, istate,
     $                   c, cJacu, clamda, objf, gradu, R, x,
     $                   iw, leniw, w, lenw )
      use CUTEST_KINDS_precision
      integer ( kind = ip_ ) n, nclin, ncnln, leniw, lenw
      integer ( kind = ip_ ) inform, iter, ldA, ldJu, ldR
      real ( kind = rp_ ) objf
      external funcon, funobj
      integer ( kind = ip_ ) istate(n+nclin+ncnln)
      integer ( kind = ip_ ) iw(leniw)
      real ( kind = rp_ ) A(ldA,*), bl(n+nclin+ncnln), bu(n+nclin+ncnln)
      real ( kind = rp_ ) c(*), cJacu(ldJu,*), clamda(n+nclin+ncnln)
      real ( kind = rp_ ) gradu(n), R(ldR,*), x(n)
      real ( kind = rp_ ) w(lenw)
      INTEGER ( KIND = ip_ ) :: mode, nstate
      INTEGER ( KIND = ip_ ) :: needc( 1 )

      mode = 1
      CALL FUNOBJ( mode, n, X, objf, w, nstate )
      CALL FUNCON( mode, ncnln, n, ldJu, needc, x, c, cJacu, nstate )
      RETURN
      END

      subroutine npfile( ioptns, inform )
      use CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: ioptns, inform
      INTEGER ( KIND = ip_ ), PARAMETER :: mxparm = 30
      INTEGER ( KIND = ip_ ) :: idbgnp, itmxnp, ldbgnp, nlnj, nlnx
      INTEGER ( KIND = ip_ ) :: jvrfy1, jvrfy2, jvrfy3, jvrfy4
      INTEGER ( KIND = ip_ ) :: lformh, lvlder, lverfy, msgnp, nlnf
      INTEGER ( KIND = ip_ ) :: nncnln, nsave, nload, ksave
      INTEGER ( KIND = ip_ ) :: ipadnp( 12 ), IPSVNP( mxparm )
      COMMON / NPPAR1/ IPSVNP, idbgnp, itmxnp, jvrfy1, jvrfy2, jvrfy3,
     *                 jvrfy4, ldbgnp, lformh, lvlder, lverfy, msgnp,
     *                 nlnf, nlnj, nlnx, nncnln, nsave, nload, ksave,
     *                 IPADNP
      lvlder = 2
      inform = 0
      RETURN
      END

      subroutine npoptn( string )
      character*(*)      string
      RETURN
      END


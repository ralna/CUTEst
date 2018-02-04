C     ( Last modified on 11 Jan 2013 at 14:00:00 )

C  Dummy NPSOL for testing npsol_main interface to CUTEst
C  Nick Gould,  11th January 2013

      subroutine npsol ( n, nclin, ncnln, ldA, ldJu, ldR,
     $                   A, bl, bu,
     $                   funcon, funobj,
     $                   inform, iter, istate,
     $                   c, cJacu, clamda, objf, gradu, R, x,
     $                   iw, leniw, w, lenw )
      integer n, nclin, ncnln, leniw, lenw, ldA, ldJu, ldR
      integer inform, iter
      double precision objf
      external           funcon, funobj
      integer            istate(n+nclin+ncnln)
      integer            iw(leniw)
      double precision   A(ldA,*), bl(n+nclin+ncnln), bu(n+nclin+ncnln)
      double precision   c(*), cJacu(ldJu,*), clamda(n+nclin+ncnln)
      double precision   gradu(n), R(ldR,*), x(n)
      double precision   w(lenw)
      INTEGER :: mode, nstate
      INTEGER :: needc( 1 )

      mode = 1
      CALL FUNOBJ( mode, n, X, objf, w, nstate )
      CALL FUNCON( mode, ncnln, n, ldJu, needc, x, c, cJacu, nstate )
      RETURN
      END

      subroutine npfile( ioptns, inform )
      integer            ioptns, inform
      INTEGER, PARAMETER :: mxparm = 30
      INTEGER :: idbgnp, itmxnp, jvrfy1, jvrfy2, jvrfy3, jvrfy4, ldbgnp
      INTEGER :: lformh, lvlder, lverfy, msgnp , nlnf, nlnj, nlnx
      INTEGER :: nncnln, nsave, nload, ksave
      INTEGER :: ipadnp( 12 ), IPSVNP( mxparm )
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


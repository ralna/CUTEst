! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"

C  Dummy SNOPT for testing snopt_main interface to CUTEst

C  Nick Gould,  14th January 2013

      subroutine snoptb( Start, m, n, ne, nName,
     &                   nnCon, nnObj, nnJac,
     &                   iObj, ObjAdd, Prob,
     &                   fgcon, fgobj,
     &                   Jcol, indJ, locJ, bl, bu, Names,
     &                   hs, x, pi, rc, 
     &                   inform, mincw, miniw, minrw,
     &                   nS, nInf, sInf, Obj,
     &                   cu, lencu, iu, leniu, ru, lenru, 
     &                   cw, lencw, iw, leniw, rw, lenrw )
      use CUTEST_KINDS_precision
      implicit none
      external  fgcon, fgobj
      integer ( kind = ip_ ) inform, iObj, lencu, leniu, lenru, lencw
      integer ( kind = ip_ ) leniw, lenrw, mincw, miniw, minrw, m, n
      integer ( kind = ip_ ) ne, nName, nS, nInf, nnCon, nnObj, nnJac
      integer ( kind = ip_ ) indJ(ne), hs(n+m), locJ(n+1), iu(leniu)
      integer ( kind = ip_ ) iw(leniw)
      real ( kind = rp_ ) sInf, Obj, ObjAdd, Jcol(ne), bl(n+m), bu(n+m)
      real ( kind = rp_ ) x(n+m), pi(m), rc(n+m), ru(lenru), rw(lenrw)
      character ( len = 8 ) Start
      character ( len = 8 ) Prob, Names(nName), cu(lencu), cw(lencw)
      integer ( kind = ip_ ) mode, nstate
      mode = 1
      call fgobj( mode, n, x, obj, rc, nstate, cu, lencu, 
     *            iu, leniu, ru, lenru )
      call fgcon( mode, m, n, ne, x, rc, Jcol, nstate,
     *            cu, lencu, iu, leniu, ru, lenru )
      inform = 1
      return
      end

      subroutine snInit( iPrint, iSumm, cw, lencw, iw, leniw, 
     &                   rw, lenrw )
      use CUTEST_KINDS_precision
      implicit none
      integer ( kind = ip_ ) iPrint, iSumm, lencw, leniw, lenrw
      integer ( kind = ip_ ) iw(leniw)
      real ( kind = rp_ ) rw(lenrw)
      character ( len = 8 ) cw(lencw)
      integer ( kind = ip_ ), parameter :: ldenj = 105
C  dense derivatives
C     iw( ldenj ) = 1
C  sparse derivatives
      iw( ldenj ) = 0
      return
      end

      subroutine snSpec( iSpecs, inform, cw, lencw, iw, leniw, 
     &                   rw, lenrw )
      use CUTEST_KINDS_precision
      implicit none
      integer ( kind = ip_ ) iSpecs, inform, lencw, leniw, lenrw
      integer ( kind = ip_ ) iw(leniw)
      real ( kind = rp_ ) rw(lenrw)
      character ( len = 8 ) cw(lencw)
      inform = 101
      return
      end

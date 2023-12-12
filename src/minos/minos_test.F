! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"

C  Dummy MINOSS for testing minos_main interface to CUTEst

C  Nick Gould,  14th January 2013

      subroutine minoss( start, m, n, nb, ne, nname,
     $                   nncon, nnobj, nnjac,
     $                   iobj, objadd, names,
     $                   a, ha, ka, bl, bu, name1, name2,
     $                   hs, xn, pi, rc,
     $                   inform, mincor, ns, ninf, sinf, obj,
     $                   z, nwcore )
      use CUTEST_KINDS_precision
C     implicit           real ( kind = rp_ ) (a-h,o-z)
      integer ( kind = ip_ ) nwcore, nprob, nstate
      integer ( kind = ip_ ) m, n, nb, ne, nname, nncon, nnobj, nnjac
      integer ( kind = ip_ ) iobj, inform, mincor, ns, ninf
      character*8 start
      character*8 names(5)
      integer*4 ha(ne), hs(nb)
      integer ( kind = ip_ ) ka(n+1), name1(nname), name2(nname)
      real ( kind = rp_ )   objadd, sinf, obj
      real ( kind = rp_ )   a(ne), bl(nb), bu(nb)
      real ( kind = rp_ )   xn(nb), pi(m), rc(nb), z(nwcore)
      integer ( kind = ip_ ) mode
      mode = 1
      call funobj( mode, n, xn, obj, Z, nstate, nprob, Z, nwcore )
      call funcon( mode, m, n, ne, xn, Z, a, nstate, nprob,
     *             Z, nwcore )
      return
      end

      subroutine m1open( lun, index, state )
      use CUTEST_KINDS_precision
      integer ( kind = ip_ ) lun, index
      character*3 state
      integer ( kind = ip_ ) ncom, nden, nlag, nmajor, nminor
      real ( kind = rp_ ) penpar, rowtol
      common / m8al1 / penpar, rowtol, ncom, nden, nlag, nmajor, nminor
C  dense derivatives
C     nden = 1
C  sparse derivatives
      nden = 0
      return
      end

      subroutine mispec( ispecx, iprinx, isummx, nwcore, inform )
      use CUTEST_KINDS_precision
      integer ( kind = ip_ ) ispecx, iprinx, isummx, nwcore, inform
      inform = 0
      return
      end

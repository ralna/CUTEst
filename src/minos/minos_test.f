C     ( Last modified on 14 Jan 2013 at 10:00:00 )

C  Dummy MINOSS for testing minos_main interface to CUTEst
C  Nick Gould,  14th January 2013

      subroutine minoss( start, m, n, nb, ne, nname,
     $                   nncon, nnobj, nnjac,
     $                   iobj, objadd, names,
     $                   a, ha, ka, bl, bu, name1, name2,
     $                   hs, xn, pi, rc,
     $                   inform, mincor, ns, ninf, sinf, obj,
     $                   z, nwcore )
C     implicit           double precision (a-h,o-z)
      integer            nwcore, nprob, nstate
      integer            m, n, nb, ne, nname, nncon, nnobj, nnjac
      integer            iobj, inform, mincor, ns, ninf
      character*8        start
      character*8        names(5)
      integer*4          ha(ne), hs(nb)
      integer            ka(n+1), name1(nname), name2(nname)
      double precision   objadd, sinf, obj
      double precision   a(ne), bl(nb), bu(nb)
      double precision   xn(nb), pi(m), rc(nb), z(nwcore)
      integer mode
      mode = 1
      call funobj( mode, n, xn, obj, Z, nstate, nprob, Z, nwcore )
      call funcon( mode, m, n, ne, xn, Z, a, nstate, nprob,
     *             Z, nwcore )
      return
      end

      subroutine m1open( lun, index, state )
      integer          lun, index
      character*3      state
      integer          ncom, nden, nlag, nmajor, nminor
      double precision penpar, rowtol
      common / m8al1 / penpar, rowtol, ncom, nden, nlag, nmajor, nminor
C  dense derivatives
C     nden = 1
C  sparse derivatives
      nden = 0
      return
      end

      subroutine mispec( ispecx, iprinx, isummx, nwcore, inform )
      integer            ispecx, iprinx, isummx, nwcore, inform
      inform = 0
      return
      end

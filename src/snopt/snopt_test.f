C     ( Last modified on 14 Jan 2013 at 16:00:00 )

C  Dummy SNOPT for testing snopt_main interface to CUTEst
C  Nick Gould,  14th January 2013

      subroutine snoptb( Start, m, n, ne, nName,
     &     nnCon, nnObj, nnJac,
     &     iObj, ObjAdd, Prob,
     &     fgcon, fgobj,
     &     Jcol, indJ, locJ, bl, bu, Names,
     &     hs, x, pi, rc, 
     &     inform, mincw, miniw, minrw,
     &     nS, nInf, sInf, Obj,
     &     cu, lencu, iu, leniu, ru, lenru, 
     &     cw, lencw, iw, leniw, rw, lenrw )
      implicit none
      external  fgcon, fgobj
      integer inform, iObj, lencu, leniu, lenru, lencw, leniw, lenrw,
     &     mincw, miniw, minrw, m, n, ne, nName, nS, nInf, nnCon,
     &     nnObj, nnJac, indJ(ne), hs(n+m), locJ(n+1), iu(leniu),
     &     iw(leniw)
      double precision sInf, Obj, ObjAdd, Jcol(ne), bl(n+m), bu(n+m), 
     &     x(n+m), pi(m), rc(n+m), ru(lenru), rw(lenrw)
      character ( len = 8 ) Start
      character ( len = 8 ) Prob, Names(nName), cu(lencu), cw(lencw)
      integer mode, nstate
      mode = 1
      call fgobj( mode, n, x, obj, rc, nstate, cu, lencu, 
     *            iu, leniu, ru, lenru )
      call fgcon( mode, m, n, ne, x, rc, Jcol, nstate,
     *            cu, lencu, iu, leniu, ru, lenru )
      return
      end

      subroutine s1open( lun, index, state )
      implicit none
      integer lun, index
      character ( len = 3 ) state
      return
      end

      subroutine snInit( iPrint, iSumm, cw, lencw, iw, leniw, 
     &                   rw, lenrw )
      implicit none
      integer iPrint, iSumm, lencw, leniw, lenrw, iw(leniw)
      double precision rw(lenrw)
      character ( len = 8 ) cw(lencw)
      integer, parameter :: ldenj = 105
C  dense derivatives
C     iw( ldenj ) = 1
C  sparse derivatives
      iw( ldenj ) = 0
      return
      end

      subroutine snSpec( iSpecs, inform, cw, lencw, iw, leniw, 
     &                   rw, lenrw )
      implicit none
      integer iSpecs, inform, lencw, leniw, lenrw, iw(leniw)
      double precision rw(lenrw)
      character ( len = 8 ) cw(lencw)
      inform = 0
      return
      end

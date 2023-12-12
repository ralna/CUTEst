! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      program SNOPT_main

      use CUTEST_KINDS_precision
      implicit none

      character*8  :: start, probnm
      character*10 :: pName
      integer ( kind = ip_ ) :: lenrw, leniw, lencw, minrw, miniw, mincw
      integer ( kind = ip_ ) :: lenru, leniu, lencu
      integer ( kind = ip_ ) :: lG, liGfun, ljGvar, llocG, nlocG
      integer ( kind = ip_ ) :: m, n, ne, nb, nnCon, nnJac, nnObj, iObj
      integer ( kind = ip_ ) :: neG, inform, nS, nInf, nNames
      integer ( kind = ip_ ) :: j, jslack, neq, nlc, status
      real ( kind = rp_ )  :: ObjAdd, sInf, Obj, cpu(4), calls(7)
      integer ( kind = ip_ ), allocatable :: iu(:), iw(:)
      real ( kind = rp_ ), allocatable :: ru(:), rw(:)
      character*8, allocatable :: cu(:), cw(:)
      integer ( kind = ip_ ), allocatable :: hs(:), indA(:), locA(:)
      real ( kind = rp_ ), allocatable :: x(:), bl(:), bu(:), rc(:)
      real ( kind = rp_ ), allocatable :: y(:), c(:), Aval(:)
      character*8,  allocatable :: Names(:)
      character*10, allocatable :: vname(:), cname(:)
      logical, allocatable :: equatn(:), linear(:)
      integer ( kind = ip_ ), parameter   :: iPrint = 15, iSpecs = 4
      integer ( kind = ip_ ), parameter   :: iSumm = 6,  nout = 6
      integer ( kind = ip_ ), parameter   :: input = 55, io_buffer = 11
      real ( kind = rp_ ), parameter :: big = 1.0e+20_rp_
      external :: SNOPT_evalcj, SNOPT_evalfg

      status = 0
      inform = 0

*     -----------------------
*     Open problem data file.
*     -----------------------
      open ( input, file = 'OUTSDIF.d', form = 'formatted',
     &       status = 'old' )
      rewind ( input )

*     ---------------------------
*     Compute problem dimensions.
*     ---------------------------
      call CUTEST_cdimen_r( status, input, n, m )
      if ( status /= 0 ) go to 920

*     ---------------
*     Allocate space.
*     ---------------
      nb    = n+m+1
      lencw = 500
      leniw = 18000000
      lenrw = 50000000
      allocate( hs(nb), x(nb), bl(nb), bu(nb),
     &          y(m + 1), c(m + 1), rc(nb), equatn(m + 1),
     &          linear(m + 1), vname(n), cname(m + 1),
     &          names(nb), cw(lencw), iw(leniw), rw(lenrw),
     &          STAT=status )
      if ( status /= 0 ) go to 990

*     -----------------------------
*     Start setting up the problem.
*     -----------------------------
      call CUTEST_csetup_r( status, input, nout, io_buffer, n, m,
     &                      x, bl, bu, y, bl(n+1), bu(n+1), equatn,
     &                      linear, 0, 2, 1 )
      close (input)
      if ( status /= 0 ) go to 910

*     Compute number of nonlinear variables, and linear/equality constraints.
      call CUTEST_cstats_r( status, nnObj, nnJac, neq, nlc )
      if ( status /= 0 ) go to 910

*     Compute the objective and constraints at x = 0.
      rc(1:n) = 0.0_rp_
      call CUTEST_cfn_r( status, n, m, rc, Obj, c )
      if ( status /= 0 ) go to 910

*     Compute the number of nonlinear constraints.
      nnCon = m - nlc

      do j = 1, m
         if ( equatn(j) ) then
            bl(n+j) = 0.0_rp_
            bu(n+j) = 0.0_rp_
         end if
      end do

*     Set linear objective row
      if ( nnObj < n .or. m == 0 ) then
         m       = m + 1
         bl(n+m) = -big
         bu(n+m) =  big
         c(m)    = 0.0_rp_
      end if

      if ( nnObj < n ) then
         iObj    = m
      else
         iObj    = 0
      end if

*     Construct the structures for Jacobian.
      call CUTEST_cdimsj_r( status, ne )
      if ( status /= 0 ) go to 910

      nlocG  = nnJac + 1

      llocG  = 4
      liGfun = llocG  + nlocG
      ljGvar = liGfun + ne
      leniu  = ljGvar + ne

      lG     = 0
      lenru  = ne

      lencu  = 1

      allocate ( Aval(ne), locA(n+1), indA(ne) )
      allocate ( iu(leniu), ru(lenru), cu(1) )

      iu(1) = llocG
      iu(2) = liGfun
      iu(3) = ljGvar
      iu(4) = lG

      call buildJac ( status, n, m, nnObj, nnCon, nnJac, ne,
     &                iObj, locA, indA, Aval, nlocG, iu(llocG+1),
     &                iu(liGfun+1), iu(ljGvar+1), ru, x, y, neG )
      if ( status /= 0 ) go to 910

*     Set bounds on linear constraints.
      do j = 1, m
         jslack = n + j
         if ( j > nnCon ) then
            bu( jslack ) = bu( jslack ) - c( j )
            bl( jslack ) = bl( jslack ) - c( j )
         end if

*     If possible, set slack variables to be nonbasic at zero.
         x(jslack) = max( 0.0_rp_, bl(jslack) )
         x(jslack) = min( x( jslack), bu(jslack) )
      end do


*     Set names, initialize some vectors.
      call CUTEST_cnames_r( status, n, m, pname, vname, cname )
      if ( status /= 0 ) go to 910

      probnm     = pname(1:8)
      Names(1:n) = vname(1:n)(1:8)
      if ( iObj == 0 ) then
         Names(n+1:n+m) = cname(1:m)(1:8)
      else
         Names(n+1:n+m-1) = cname(1:m-1)(1:8)
         Names(n+m)       = probnm
      end if
      nNames = n + m

      hs(1:nb)   = 0
      y(1:nnCon) = 0.0_rp_
      ObjAdd     = 0.0_rp_
      if ( nnObj == 0 ) ObjAdd = Objadd - Obj

*     ------------------------------
*     Open SNOPT input/output files.
*     Initialize SNOPT
*     Read a Specs file.
*     Solve the problem.
*     ------------------------------
      open ( iSpecs, file = 'snopt.spc', form = 'formatted',
     &       status = 'old' )
      open ( iPrint, file = trim(probnm)//'.out', status = 'unknown' )

      call snInit( iPrint, iSumm, cw, lencw, iw, leniw, rw, lenrw )

      call snSpec( iSpecs, inform, cw, lencw, iw, leniw, rw, lenrw )
      if ( inform .ne. 101 .and. inform .ne. 107 ) then
         if (inform .eq. 131 .and. nout .gt. 0) write( nout, 2010 )
         stop
      end if

      start = 'cold'
      call snoptb ( start , m , n , ne    , nNames,
     &              nnCon , nnObj , nnJac ,
     &              iObj  , ObjAdd, probnm,
     &              SNOPT_evalcj, SNOPT_evalfg,
     &              Aval  , indA  , locA  , bl    , bu    , names ,
     &              hs    , x     , y     , rc    ,
     &              inform, mincw , miniw , minrw ,
     &              nS    , nInf  , sInf  , Obj   ,
     &              cu    , lencu , iu    , leniu , ru    , lenru ,
     &              cw    , lencw , iw    , leniw , rw    , lenrw )

*     Try to handle abnormal SNOPT inform codes gracefully.
      if ( inform .ge. 50 .and.
     &     ( iPrint .gt. 0 .or. iSumm .gt. 0 ) ) then

         if ( iPrint .gt. 0 ) write ( iPrint, 3000 ) inform
         if ( iSumm  .gt. 0 ) write ( iSumm , 3000 ) inform

         if ( inform .eq. 83  .or. inform .eq. 84 ) then
            if ( iPrint .gt. 0 ) write ( iPrint, 3010 )
            if ( iSumm  .gt. 0 ) write ( iSumm , 3010 )
         end if

      end if

      call CUTEST_creport_r( status, calls, cpu )
      if ( status /= 0 ) go to 910

      write(nout,2000) pname, n, m, calls(1), calls(2), calls(5),
     &                 calls(6), inform, Obj, cpu(1), cpu(2)

 910  continue
      if ( allocated(iu) )     deallocate ( iu )
      if ( allocated(ru) )     deallocate ( ru )
      if ( allocated(cu) )     deallocate ( cu )

      if ( allocated(Aval) )   deallocate ( Aval )
      if ( allocated(locA) )   deallocate ( locA )
      if ( allocated(indA) )   deallocate ( indA )

      if ( allocated(hs) )     deallocate ( hs )
      if ( allocated(locA) )   deallocate ( locA )
      if ( allocated(x) )      deallocate ( x )
      if ( allocated(bl) )     deallocate ( bl )
      if ( allocated(bu) )     deallocate ( bu )
      if ( allocated(y) )      deallocate ( y )
      if ( allocated(c) )      deallocate ( c )
      if ( allocated(rc) )     deallocate ( rc )
      if ( allocated(equatn) ) deallocate ( equatn )
      if ( allocated(linear) ) deallocate ( linear )
      if ( allocated(vname) )  deallocate ( vname )
      if ( allocated(cname) )  deallocate ( cname )
      if ( allocated(names) )  deallocate ( names )
      if ( allocated(cw) )     deallocate ( cw )
      if ( allocated(iw) )     deallocate ( iw )
      if ( allocated(rw) )     deallocate ( rw )

      close( iSpecs )
      close( iPrint )

      if ( status /= 0 ) go to 920

      call CUTEST_cterminate_r( status )
      stop

 920  continue
      write(nout, "( ' CUTEst error, status = ', i0, ', stopping' )")
     &     status
      stop

 990  continue
      write(nout, "( ' Allocation error, status = ', I0 )" ) status
      stop

*     Non-executable statements.
 2010 format( /, ' ** PROGRAM SNPMA: No Specs file found.' )
 3000 format( /, ' WARNING!  Abnormal SNOPT termination code:',
     &           ' INFORM = ', I2 )
 3010 format(    ' Not enough storage to solve the problem.',
     &        /, ' Reduce parameters in SNOPT.SPC file or increase',
     &           ' LENIW and LENRW in SNPMA.' )
 2000 format( /, 24('*'), ' CUTEr statistics ', 24('*') //
     &    ,' Code used               :  SNOPT',    /
     &    ,' Problem                 :  ', A10,    /
     &    ,' # variables             =      ', I10 /
     &    ,' # constraints           =      ', I10 /
     &    ,' # objective functions   =        ', F8.2 /
     &    ,' # objective gradients   =        ', F8.2 /
     &    ,' # constraints functions =        ', F8.2 /
     &    ,' # constraints gradients =        ', F8.2 /
     &     ' Exit code               =      ', I10 /
     &    ,' Final f                 = ', E15.7 /
     &    ,' Set up time             =      ', 0P, F10.2, ' seconds' /
     &     ' Solve time              =      ', 0P, F10.2, ' seconds' //
     &     66('*') / )
      end


*     ******************************************************************

      subroutine buildJac ( status, n, m, nnObj, nnCon, nnJac, ne,
     &                      iObj, locA, indA, A, nlocG, locG,
     &                      iGfun, jGvar, G, x, y, neG )

      use CUTEST_KINDS_precision
      implicit none
      integer ( kind = ip_ ) :: status
      integer ( kind = ip_ ) :: n, m, nnObj, nnCon, nnJac, ne, neG
      integer ( kind = ip_ ) :: iObj, locA(n+1), indA(ne)
      integer ( kind = ip_ ) :: nlocG, locG(nlocG), iGfun(ne), jGvar(ne)
      real ( kind = rp_ ) :: x(n), y(m), A(ne), G(ne)

*     On entry,
*       nlocG = nnJac + 1
*     On exit,
*       neG

      integer ( kind = ip_ )          :: i, j, k, l, neA, lj

      lj = ne
      call CUTEST_csgr_r( status, n, m, x, y, .false., ne, lj,
     &                    G, jGvar, iGfun )
      if ( status /= 0 ) return

*     Initialize vectors.
      do j = 1, nnJac
         locA(j) = 0
         locG(j) = 0
      end do

      do j = nnJac+1, n
         locA(j) = 0
      end do

*     Count the Jacobian entries in column j.
*     Initialize locA.  Store the column counts in locA(1:n).
*     Don't include nonlinear objective function entries.
      neA = ne
      do l = 1, ne
         j = jGvar(l)
         i = iGfun(l)
         if (i .eq. 0 .and. j .le. nnObj) then
            neA = neA - 1
         else
            locA(j) = locA(j) + 1
         end if
      end do
      locA(n+1) = neA + 1

*     Set locA(j) to point to start of column j.
      do j = n, 1, -1
         locA(j) = locA(j+1) - locA(j)
      end do

*     Load nonlinear Jacobian entries into A and indA.
*     Use locA to keep track of position for each variable b.
*     Also count nonlinear Jacobian entries in each column and
*     store count in locG(j).
      neG = 0
      do k = 1, ne
         j = jGvar(k)
         i = iGfun(k)
         if ( i .gt. 0 .and. i .le. nnCon .and. j .le. nnJac ) then
            l       = locA(j)
            A(l)    = G(k)
            indA(l) = i
            locA(j) = l   + 1
            locG(j) = locG(j) + 1
            neG     = neG + 1
         end if
      end do

*     Load the constant Jacobian entries, including linear objective
*     function entries,  in A and indA.
*     locA(j) points to the next available position in column j.
      do k = 1, ne
         j = jGvar(k)
         i = iGfun(k)
         if (i .eq. 0  .and.  j .gt. nnObj) then ! linear obj
            l       = locA(j)
            A(l)    = G(k)
            indA(l) = iObj
            locA(j) = l + 1
         else if (i .gt. nnCon .or. (i .gt. 0 .and. j .gt. nnJac)) then
            l       = locA(j)
            A(l)    = G(k)
            indA(l) = i
            locA(j) = l + 1
         end if
      end do

*     Reset locA  and set locG.
*     locG(j)  points  to the start of column j in Gcon,
*     the nonlinear Jacobian.  These pointers are used by usrfgh.
      locG(nlocG) = neG + 1

      do  j = n, 2, -1
         locA(j) = locA(j-1)
      end do
      locA(1) = 1

      do  j = nnJac, 2, -1
         locG(j) = locG(j+1) - locG(j)
      end do
      locG(1) = 1

      ne      = neA

      if (ne .eq. 0) then
         ne        = 1
         A(ne)     = 0.0_rp_
         indA(ne)  = 1
         locA(n+1) = ne + 1
      end if

      end

*     ******************************************************************

      subroutine SNOPT_evalfg ( mode, nnObj, x, fObj, gObj, nState,
     &                          cu, lencu, iu, leniu, ru, lenru )
      use CUTEST_KINDS_precision
      integer ( kind = ip_ ) :: mode, nnObj, nState, lencu, leniu, lenru
      integer ( kind = ip_ ) :: iu(leniu)
      real ( kind = rp_ ) :: fObj
      real ( kind = rp_ ) :: x(nnObj), gObj(nnObj), ru(lenru)
      character*8      :: cu(lencu)

      logical          :: needG
      integer ( kind = ip_ )          :: status

      if (mode .eq. 0) then
         needG = .false.
      else
         needG = .true.
      end if

      call CUTEST_cofg_r( status, nnObj, x, fObj, gObj, needG )

      if ( status .ne. 0 ) THEN
         write( 6, "( ' CUTEst error, status = ', i0, ', stopping' )")
     &   status
         stop
      end if

      end

*     ******************************************************************

      subroutine SNOPT_evalcj ( mode, nnCon, nnJac, neG, x,
     &                          fCon, gCon, nState,
     &                          cu, lencu, iu, leniu, ru, lenru )

      use CUTEST_KINDS_precision
      integer ( kind = ip_ ) :: mode, nnCon, nnJac, neG, nState
      integer ( kind = ip_ ) :: lencu, leniu, lenru
      integer ( kind = ip_ ) :: iu(leniu)
      real ( kind = rp_ ) :: x(nnJac), fCon(nnCon), gCon(neG), ru(lenru)
      character*8      :: cu(lencu)

      integer ( kind = ip_ ) :: liGfun, ljGvar, llocG, lG, nlocG

      llocG   = iu(1)
      liGfun  = iu(2)
      ljGvar  = iu(3)
      lG      = iu(4)

      nlocG   = nnJac + 1

      call funcon0 ( mode, nnCon, nnJac, neG, x, fCon, gCon, nState,
     &               nlocG, iu(llocG+1), iu(liGfun+1), iu(ljGvar+1),
     &               ru(lG+1) )

      end

*     ******************************************************************

      subroutine funcon0 ( mode, nnCon, nnJac, neG,
     &                     x, fCon, gCon, nState,
     &                     nlocG, locG, iGfun, jGvar, G )

      use CUTEST_KINDS_precision
      integer ( kind = ip_ ) :: mode, nlocG, nnCon, neG, nnJac, nState
      integer ( kind = ip_ ) :: iGfun(neG), jGvar(neG), locG(nlocG)
      real ( kind = rp_ ) :: x(nnJac), fCon(nnCon), gCon(neG)
      real ( kind = rp_ ) :: G(neG)

      integer ( kind = ip_ ) :: j, k, l, nnzJ
      integer ( kind = ip_ ) :: status
      logical          :: needG

      needG  = mode .gt. 0

*     Evaluate the problem constraints. On input, nnCon > 0.
*     The Jacobian is stored in sparse format.
      call CUTEST_ccfsg_r( status, nnJac, nnCon, x, fCon, nnzj, neG,
     &                     G, jGvar, iGfun, needG )
      if ( status .ne. 0 ) go to 910

      if ( needG ) then
*        Copy the Jacobian from CSCFG, stored in (G,iGfun,jGvar)
*        into the SNOPT Jacobian stored by columns in gCon.
*        locG(j) points to the next available position in column j.
         do l       = 1, nnzJ
            j       = jGvar(l)
            k       = locG(j)
            gCon(k) = G(l)
            locG(j) = k + 1
         end do

*        Reset locG.
         do j = nnJac, 2, -1
            locG(J) = locG(J-1)
         end do
         locG(1) = 1
      end if

      return

 910  continue
      write( 6, "( ' CUTEst error, status = ', i0, ', stopping' )")
     &     status
      stop

      end

*     ******************************************************************

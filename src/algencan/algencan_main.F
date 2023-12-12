! THIS VERSION: CUTEST 2.2 - 2023-11-29 AT 11:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      PROGRAM ALGENCAN_main

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     Driver for running ALGENCAN on CUTEst problems.
C
C     Derived from ALGENCAN algencanma.f and cuterwrapper.f from the 
C     TANGO home page:
C       www.ime.usp.br/~egbirgin/tango/
C     CUTEst evolution February 2013, Nick Gould
C     
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C  Set up parameters, variables and arrays required by constrained tools

      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ), PARAMETER :: input = 10, out = 6
      INTEGER ( KIND = ip_ ), PARAMETER :: io_buffer = 11
      INTEGER ( KIND = ip_ ) :: inform, iprint, m, n, ncomp, status
      REAL ( KIND = rp_ ) :: cnorm, efacc, eoacc, epsfeas, epsopt, f
      REAL ( KIND = rp_ ) :: nlpsupn, snorm
      LOGICAL :: checkder
      CHARACTER * 10 :: pname
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 7 )
      LOGICAL :: CODED( 11 )
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, X_l, X_u
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: LAMBDA
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATN, LINEAR

C  Open the relevant file

      OPEN ( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND( input )

C  compute problem dimensions

      CALL CUTEST_cdimen_r( status, input, n, m )
      IF ( status /= 0 ) GO TO 910
C  close input so that inip can open it again!
      CLOSE( input )

C  allocate space 

      ALLOCATE( X( n ), X_l( n ), X_u( n ), LAMBDA( m ), 
     *          EQUATN( m ), LINEAR( m ), STAT = status )
      IF ( status /= 0 ) GO TO 990

C  set up parameters

      CALL param( epsfeas, epsopt, efacc, eoacc, iprint, ncomp )

C  initialize data

      CALL inip( n, X, X_l, X_u, m, LAMBDA, EQUATN, LINEAR, CODED, 
     *           checkder )

C  call the optimizer

      CALL algencan( epsfeas, epsopt, efacc, eoacc, iprint, ncomp, n, 
     *               X, X_l, X_u, m, LAMBDA, EQUATN, LINEAR, CODED, 
     *               checkder, f, cnorm, snorm, nlpsupn, inform )

C  output information

      CALL CUTEST_creport_r( status, CALLS, CPU )
      CALL CUTEST_probname_r( status, pname )
      IF ( out .GT. 0 )
     *  WRITE ( out, 2000 ) pname, n, m, CALLS( 1 ), CALLS( 2 ), 
     *   CALLS( 5 ), CALLS( 6 ), inform, f, CPU( 1 ), CPU( 2 )

C  terminate runs

      CALL endp( n, X, X_l, X_u, m, LAMBDA, EQUATN, LINEAR )
      DEALLOCATE( X, X_l, X_u, LAMBDA, EQUATN, LINEAR, 
     *            STAT = status )
      CALL CUTEST_cterminate_r( status )
      STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP

C  Non-executable statements

 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *    ,' Package used            :  ALGENCAN',    /
     *    ,' Problem                 :  ', A10,    /
     *    ,' # variables             =      ', I10 /
     *    ,' # constraints           =      ', I10 /
     *    ,' # objective functions   =        ', F8.2 /
     *    ,' # objective gradients   =        ', F8.2 / 
     *    ,' # constraints functions =        ', F8.2 /
     *    ,' # constraints gradients =        ', F8.2 /
     *    ,' Exit code               =      ', I10 /
     *    ,' Final f                 = ', E15.7 /
     *    ,' Set up time             =      ', 0P, F10.2, ' seconds' /
     *    ,' Solve time              =      ', 0P, F10.2, ' seconds' //
     *     66('*') / )
      END

C     ******************************************************************
C     auxiliary routines
C     ******************************************************************

      subroutine param(epsfeas,epsopt,efacc,eoacc,iprint,ncomp)

      use CUTEST_KINDS_precision

C     SCALAR ARGUMENTS
      integer ( kind = ip_ ) iprint,ncomp
      real ( kind = rp_ ) efacc,eoacc,epsfeas,epsopt

      epsfeas  = ( 10.0_rp_ ) ** ( - 8 )
      epsopt   = ( 10.0_rp_ ) ** ( - 8 )

      efacc    = sqrt( epsfeas )
      eoacc    = sqrt( epsopt )

      iprint   = 10
      ncomp    = 6

      end

C  =================================================================
C  ******************************************************************

      subroutine inip(n,x,l,u,m,lambda,equatn,linear,coded,checkder)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      logical checkder
      integer ( kind = ip_ ) m,n

C     ARRAY ARGUMENTS
      logical coded(11),equatn(m),linear(m)
      real ( kind = rp_ ) l(n),lambda(m),u(n),x(n)

C#include "../../algencan/dim.par"
      include "dim.par"
      include "cutest.com"

C     LOCAL SCALARS
      integer ( kind = ip_ ) cind,i,sind, status
      character * 10 pname
      real ( kind = rp_ ) dum, big
      parameter ( big = ( 10.0_rp_ ) ** 20 )

C     LOCAL ARRAYS
C     character * 10 gnames(mmax),xnames(nmax)
C     real ( kind = rp_ ) c(mmax),cl(mmax),cu(mmax)
      character * 10 gnames(m),xnames(n)
      real ( kind = rp_ ) c(m),cl(m),cu(m)

C     EXTERNAL SUBROUTINES
      external CUTEST_csetup_r, CUTEST_cnames_r

C     Set problem data

      open(10,file='OUTSDIF.d',form='formatted',status='old')
      rewind( 10 )

      call CUTEST_csetup_r(status,10,20,11,n,m,x,l,u,lambda,cl,cu,
     + equatn,linear,1,0,0)
      if ( status .ne. 0 ) go to 910
      call CUTEST_cnames_r(status,n,m,pname,xnames,gnames)
      if ( status .ne. 0 ) go to 910

      close(10)

      write(*,*) 'Solving problem: ',pname

C     There are two options for two-sides inequality constraints:
C
C     (a) Add a slack variable and transform it into an equality
C         constraints plus a bound constraint for the slack.
C
C     (b) Split it into two one-side inequality constraints.
C
C     There is a local variable of this subroutine called useslacks.
C     If useslacks is TRUE then we implement (a) else, we implement (b).

      useslacks = .false.

C     Compute constraints to initialize slacks

      if ( useslacks ) then
          call CUTEST_cfn_r(status,n,m,x,dum,c)
          if ( status .ne. 0 ) go to 910
      end if

      nranges = 0

      do i = 1,m

C         Equality constraint.

          if ( equatn(i) ) then

              slaind(i) =     - 1
              ccor(i)   =       0
              cmap(i)   =       i
              ca(i)     =   1.0_rp_
              cb(i)     = - cu(i)

C         Ranged inequality constraint: add slack or split it.

          else if ( cl(i) .gt. - big .and. cu(i) .lt. big ) then

              nranges = nranges + 1

C             Replace by c(x) - s = 0 and l <= s <= u.

              if ( useslacks ) then

                  sind = n + nranges

                  l(sind) = cl(i)
                  u(sind) = cu(i)
                  x(sind) = max( cl(i), min( c(i), cu(i) ) )

                  slaind(i) =    sind
                  ccor(i)   =       0
                  cmap(i)   =       i
                  ca(i)     =   1.0_rp_
                  cb(i)     =   0.0_rp_

                  equatn(i) = .true.

C             Split into c(x) - u <= 0 and l - c(x) <= 0.

              else
                  cind = m + nranges

                  equatn(cind) = equatn(i)
                  linear(cind) = linear(i)

                  slaind(cind) =     - 1
                  cmap(cind)   =       i
                  ca(cind)     =   1.0_rp_
                  cb(cind)     = - cu(i)

                  slaind(i)    =     - 1
                  ccor(i)      =    cind
                  cmap(i)      =       i
                  ca(i)        = - 1.0_rp_
                  cb(i)        =   cl(i)
              end if

C         Inequality constraint of type c(x) <= u.

          else if ( cu(i) .lt. big ) then

              slaind(i) =     - 1
              ccor(i)   =       0
              cmap(i)   =       i
              ca(i)     =   1.0_rp_
              cb(i)     = - cu(i)

C         Inequality constraint of type l <= c(x).

          else if ( cl(i) .gt. - big ) then

              slaind(i) =     - 1
              ccor(i)   =       0
              cmap(i)   =       i
              ca(i)     = - 1.0_rp_
              cb(i)     =   cl(i)

          end if

      end do

      mcutest = m
      ncutest = n

      if ( useslacks ) then
          n = n + nranges
      else
          m = m + nranges
      end if

C     Lagrange multipliers approximation

      do i = 1,m
          lambda(i) =  0.0_rp_
      end do

C     In this CUTEst interface evalfc, evalgjac, evalhl and evalhlp are
C     present. evalf, evalg, evalh, evalc, evaljac and evalhc are not.

      coded( 1) = .false. ! evalf
      coded( 2) = .false. ! evalg
      coded( 3) = .false. ! evalh
      coded( 4) = .false. ! evalc
      coded( 5) = .false. ! evaljac
      coded( 6) = .false. ! evalhc
      coded( 7) = .true.  ! evalfc
      coded( 8) = .true.  ! evalgjac
      coded( 9) = .false. ! evalgjacp
      coded(10) = .true.  ! evalhl
      coded(11) = .false. ! evalhlp (coded but not being used)
                          ! (In fact, the case sf=0.0 is incomplete,
                          ! September 11th, 2009)

      checkder = .false.
      return

  910 continue
      write( 6, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      stop
      end

C     ******************************************************************
C     ******************************************************************

      subroutine endp(n,x,l,u,m,lambda,equatn,linear)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      integer ( kind = ip_ ) m,n

C     ARRAY ARGUMENTS
      logical equatn(m),linear(m)
      real ( kind = rp_ ) l(n),lambda(m),u(n),x(n)

      end

C     *****************************************************************
C     *****************************************************************

      subroutine evalf(n,x,f,flag)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      integer ( kind = ip_ ) flag,n
      real ( kind = rp_ ) f

C     ARRAY ARGUMENTS
      real ( kind = rp_ ) x(n)

      flag = - 1

      end

C     *****************************************************************
C     *****************************************************************

      subroutine evalg(n,x,g,flag)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      integer ( kind = ip_ ) flag,n

C     ARRAY ARGUMENTS
      real ( kind = rp_ ) g(n),x(n)

      flag = - 1

      end

C     ******************************************************************
C     ******************************************************************

      subroutine evalh(n,x,hlin,hcol,hval,hnnz,flag)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      integer ( kind = ip_ ) flag,hnnz,n

C     ARRAY ARGUMENTS
      integer ( kind = ip_ ) hcol(*),hlin(*)
      real ( kind = rp_ ) hval(*),x(n)

      flag = - 1

      end

C     ******************************************************************
C     ******************************************************************

      subroutine evalc(n,x,ind,c,flag)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      integer ( kind = ip_ ) flag,ind,n
      real ( kind = rp_ ) c

C     ARRAY ARGUMENTS
      real ( kind = rp_ ) x(n)

      flag = - 1

      end

C     ******************************************************************
C     ******************************************************************

      subroutine evaljac(n,x,ind,jcvar,jcval,jcnnz,flag)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      integer ( kind = ip_ ) flag,ind,jcnnz,n

C     ARRAY ARGUMENTS
      integer ( kind = ip_ ) jcvar(*)
      real ( kind = rp_ ) jcval(*),x(n)

      flag = - 1

      end

C     ******************************************************************
C     ******************************************************************

      subroutine evalhc(n,x,hclin,hccol,hcval,hcnnz,flag)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      integer ( kind = ip_ ) flag,hcnnz,n

C     ARRAY ARGUMENTS
      integer ( kind = ip_ ) hccol(*),hclin(*)
      real ( kind = rp_ ) hcval(*),x(n)

      flag = - 1

      end

C     ******************************************************************
C     ******************************************************************

      subroutine evalhl(n,x,m,lambda,sf,sc,hlin,hcol,hval,hnnz,flag)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      integer ( kind = ip_ ) flag,hnnz,m,n, status
      real ( kind = rp_ ) sf

C     ARRAY ARGUMENTS
      integer ( kind = ip_ ) hlin(*),hcol(*)
      real ( kind = rp_ ) hval(*),lambda(m),sc(m),x(n)

C#include "../../algencan/dim.par"
      include "dim.par"
      include "cutest.com"

C     LOCAL SCALARS
      integer ( kind = ip_ ) i

C     LOCAL ARRAYS
!     real ( kind = rp_ ) v(mmax)
      real ( kind = rp_ ) v(m)

      flag = 0

      if ( sf .eq. 0.0_rp_ ) then
C         flag = - 1
C         return

C         Special case for sf=0
C         It is too inneficient !!!
C         call ievalhl(n,x,m,lambda,sf,sc,hlin,hcol,hval,hnnz,flag)
C         return

          do i = 1,mcutest
              v(i) = 0.0_rp_
          end do

          do i = 1,m
              v(cmap(i)) = v(cmap(i)) + ca(i) * lambda(i) * sc(i)
          end do

          call CUTEST_cshc_r(status,ncutest,mcutest,x,v,
     *                       hnnz,hnnzmax,hval,hlin,hcol)
          if ( status .ne. 0 ) go to 910

C         Interchange row and column indices

          do i = 1,hnnz
              call intswap(hlin(i),hcol(i))
          end do

          return
      end if

      do i = 1,mcutest
          v(i) = 0.0_rp_
      end do

      do i = 1,m
          v(cmap(i)) = v(cmap(i)) + ca(i) * lambda(i) * sc(i) / sf
      end do

      call CUTEST_csh_r(status,ncutest,mcutest,x,v,
     *                  hnnz,hnnzmax,hval,hlin,hcol)
      if ( status .ne. 0 ) go to 910

C     Interchange row and column indices

      do i = 1,hnnz
          call intswap(hlin(i),hcol(i))
          hval(i) = hval(i) * sf
      end do

      return

  910 continue
      write( 6, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      stop
      end

C     ******************************************************************
C     ******************************************************************

      subroutine ievalhl(n,x,m,lambda,sf,sc,hlin,hcol,hval,hnnz,flag)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      integer ( kind = ip_ ) flag,hnnz,m,n
      real ( kind = rp_ ) sf

C     ARRAY ARGUMENTS
      integer ( kind = ip_ ) hlin(*),hcol(*)
      real ( kind = rp_ ) hval(*),lambda(m),sc(m),x(n)

C#include "../../algencan/dim.par"
      include "dim.par"
      include "cutest.com"

C     LOCAL SCALARS
      integer ( kind = ip_ ) col,con,hnnztmp,hnnzmaxtmp,i,ind,itmp,j
      integer ( kind = ip_ ) lin,nextj,rnnz
      integer ( kind = ip_ ) status
      real ( kind = rp_ ) val

C     LOCAL ARRAYS
C     integer ( kind = ip_ ) hcon(hnnzmax),pos(nmax),rind(nmax)
C     integer ( kind = ip_ ) stlin(nmax)
C     real ( kind = rp_ ) rval(nmax),v(mmax)
      integer ( kind = ip_ ), allocatable, dimension( : ) :: hcon
      integer ( kind = ip_ ) pos(n),rind(n)
      integer ( kind = ip_ ) stlin(n)
      real ( kind = rp_ ) rval(n),v(m)

C     EXTERNAL SUBROUTINES
      external CUTEST_cish_r

      flag = 0

C     ==================================================================
C     COMPUTE MULTIPLIERS FOR CUTEST-STYLE FUNCTIONS
C     ==================================================================

      do i = 1,mcutest
          v(i) = 0.0_rp_
      end do

      do i = 1,m
          v(cmap(i)) = v(cmap(i)) + ca(i) * lambda(i) * sc(i)
      end do

C     ==================================================================
C     COMPUTE HESSIANS
C     ==================================================================

C     COMPUTE HESSIAN OF THE OBJECTIVE FUNCTION

      call CUTEST_cish_r(status,ncutest,x,0,hnnz,hnnzmax,hval,hlin,hcol)
      if ( status .ne. 0 ) go to 910

      allocate( hcon( hnnz ), stat = i )

C     For each element of the Hessian of the objective function,
C     set constraint index as zero
      do i = 1,hnnz
          hval(i) = hval(i) * sf
          hcon(i) = 0
      end do

C     COMPUTE HESSIANS OF THE CONSTRAINTS

      ind = 0
      hnnzmaxtmp = hnnzmax - hnnz

      do j = 1,mcutest
C         Compute Hessian of constraint j
          call CUTEST_cish_r(status,ncutest,x,j,hnnztmp,hnnzmaxtmp,
     +      hval(hnnz+ind+1),hlin(hnnz+ind+1),hcol(hnnz+ind+1))
          if ( status .ne. 0 ) go to 910

C         For each element of the Hessian, set constraint as j
          do i = hnnz + ind + 1,hnnz + ind + hnnztmp
              hcon(i) = j
          end do

          ind = ind + hnnztmp
          hnnzmaxtmp = hnnzmaxtmp - hnnztmp
      end do

      hnnz = hnnz + ind

C     INTERCHANGE ROW AND COLUMN INDICES

      do i = 1,hnnz
          call intswap(hlin(i),hcol(i))
      end do

      if ( ind .eq. 0 ) return

C     ==================================================================
C     SET ROW LINKED LISTS
C     ==================================================================

C     Initialize pointers to the first element of each row
      do i = 1,n
         stlin(i) = 0
      end do

C     Set row linked lists
      do i = 1,hnnz
         lin        = hlin(i)
         itmp       = stlin(lin)
         stlin(lin) = i
         hlin(i)    = itmp
      end do

C     ==================================================================
C     BUILD HESSIAN OF THE LAGRANGIAN ROW BY ROW
C     ==================================================================

C     Initialize array pos
      do i = 1,n
          pos(i) = 0
      end do

      do i = 1,n
C         Initialize the i-th row of the Hessian of the Lagrangian
          rnnz = 0

C         Process the i-th row of all the Hessians
          j = stlin(i)

 10       if ( j .ne. 0 ) then

C             Process element (i,hcol(j)) of the Hessian of constraint
C             hcon(j) (Hessian of the objective function if hcon(j)=0)

              col = hcol(j)
              con = hcon(j)
              if ( con .eq. 0 ) then
                  val = hval(j)
              else
                  val = hval(j) * v(con)
              end if

              if ( pos(col) .ne. 0 ) then
                  rval(pos(col)) = rval(pos(col)) + val

              else
                  rnnz           = rnnz + 1
                  pos(col)       = rnnz
                  rind(pos(col)) = col
                  rval(pos(col)) = val
              end if

C             Get next element in the i-th row linked list
              j = hlin(j)
              go to 10
          end if

C         Clean array pos
          do j = 1,rnnz
              pos(rind(j)) = 0
          end do

C         Set i-th row of hl (over the i-th rows of the Hessians)
C         and mark remaining elements to be deleted
          j = stlin(i)

 20       if ( j .ne. 0 ) then
              nextj = hlin(j)

              if ( rnnz .ne. 0 ) then
                  hlin(j) = i
                  hcol(j) = rind(rnnz)
                  hval(j) = rval(rnnz)
                  rnnz    = rnnz - 1
              else
                  hlin(j) = 0
              end if

              j = nextj
              go to 20
          end if

      end do

C     Eliminate remaining elements (marked with hlin(j)=0)
      j = 1

 30   if ( j .le. hnnz ) then
          if ( hlin(j) .eq. 0 ) then
              if ( j .ne. hnnz ) then
                  hlin(j) = hlin(hnnz)
                  hcol(j) = hcol(hnnz)
                  hval(j) = hval(hnnz)
              end if
              hnnz = hnnz - 1
          else
              j = j + 1
          end if

          go to 30
      end if
      deallocate( hcon, stat = i )
      return

  910 continue
      write( 6, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      stop
      end

C     ******************************************************************
C     ******************************************************************

      subroutine evalhlp(n,x,m,lambda,sf,sc,p,hp,goth,flag)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      logical goth
      integer ( kind = ip_ ) flag,m,n
      real ( kind = rp_ ) sf

C     ARRAY ARGUMENTS
      real ( kind = rp_ ) hp(n),lambda(m),p(n),sc(m),x(n)

C#include "../../algencan/dim.par"
      include "dim.par"
      include "cutest.com"

C     LOCAL SCALARS
      integer ( kind = ip_ ) i, status

C     LOCAL ARRAYS
C     real ( kind = rp_ ) v(mmax)
      real ( kind = rp_ ) v(m)

C     EXTERNAL SUBROUTINES
      external CUTEST_cprod_r

      flag = 0

      if ( sf .eq. 0.0_rp_ ) then
          flag = - 1
          return

C         Special case for sf=0
C         It is too inneficient !!!
C         call ievalhlp(n,x,m,lambda,sf,sc,p,hp,goth,flag)
C         return
      end if

      do i = 1,mcutest
          v(i) = 0.0_rp_
      end do

      do i = 1,m
          v(cmap(i)) = v(cmap(i)) + ca(i) * lambda(i) * sc(i) / sf
      end do

      call CUTEST_chprod_r(status,ncutest,mcutest,goth,x,v,p,hp)
      if ( status .ne. 0 ) go to 910

      do i = 1,ncutest
          hp(i) = hp(i) * sf
      end do

      do i = ncutest + 1,n
          hp(i) = 0.0_rp_
      end do

      goth = .true.
      return

  910 continue
      write( 6, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      stop
      end

C     ******************************************************************
C     ******************************************************************

      subroutine ievalhlp(n,x,m,lambda,sf,sc,p,hp,goth,flag)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      logical goth
      integer ( kind = ip_ ) flag,m,n
      real ( kind = rp_ ) sf

C     ARRAY ARGUMENTS
      real ( kind = rp_ ) hp(n),lambda(m),p(n),sc(m),x(n)

C#include "../../algencan/dim.par"
      include "dim.par"
C#include "../../algencan/hessdat.com"
      include "hessdat.com"
      include "cutest.com"

C     LOCAL SCALARS
      integer ( kind = ip_ ) col,i,lin
      real ( kind = rp_ ) val

      flag = 0

      if ( .not. goth ) then
          goth = .true.
          call ievalhl(n,x,m,lambda,sf,sc,hlin,hcol,hval,hnnz,flag)
      end if

      do i = 1,ncutest
          hp(i) = 0.0_rp_
      end do

      do i = 1,hnnz
          lin = hlin(i)
          col = hcol(i)
          val = hval(i)

          hp(lin) = hp(lin) + p(col) * val

          if ( lin .ne. col ) then
              hp(col) = hp(col) + p(lin) * val
          end if
      end do

      do i = ncutest + 1,n
          hp(i) = 0.0_rp_
      end do

      end

C     *****************************************************************
C     *****************************************************************

      subroutine evalfc(n,x,f,m,c,flag)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      integer ( kind = ip_ ) flag,m,n
      real ( kind = rp_ ) f

C     ARRAY ARGUMENTS
      real ( kind = rp_ ) c(m),x(n)

C#include "../../algencan/dim.par"
      include "dim.par"
      include "cutest.com"

C     LOCAL SCALARS
      integer ( kind = ip_ ) i,sind, status

C     EXTERNAL SUBROUTINES
      external CUTEST_cfn_r

      flag = 0

      call CUTEST_cfn_r(status,ncutest,mcutest,x,f,c)
      if ( status .ne. 0 ) go to 910

      do i = m,1,-1
          c(i) = ca(i) * c(cmap(i)) + cb(i)
      end do

      do i = 1,m
          sind = slaind(i)
          if ( sind .ne. - 1 ) then
              c(i) = c(i) - x(sind)
          end if
      end do
      return

  910 continue
      write( 6, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      stop
      end

C     ******************************************************************
C     ******************************************************************

      subroutine evalgjac(n,x,g,m,jcfun,jcvar,jcval,jcnnz,flag)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      integer ( kind = ip_ ) flag,jcnnz,m,n

C     ARRAY ARGUMENTS
      integer ( kind = ip_ ) jcfun(*),jcvar(*)
      real ( kind = rp_ ) g(n),jcval(*),x(n)

C#include "../../algencan/dim.par"
      include "dim.par"
      include "cutest.com"

C     LOCAL SCALARS
      integer ( kind = ip_ ) fu2,fun,i,jcnnztmp,sind,var, status

C     LOCAL ARRAYS
      real ( kind = rp_ ) dum3(1)

C     EXTERNAL SUBROUTINES
      external CUTEST_csgr_r

      flag = 0

      call CUTEST_csgr_r(status,ncutest,mcutest,x,dum3,.false.,
     +                   jcnnz,jcnnzmax,jcval,jcvar,jcfun)
      if ( status .ne. 0 ) go to 910

C     Remove gradient from the sparse structure

      do i = 1,n
          g(i) = 0.0_rp_
      end do

      i = 1

 100  if ( i .le. jcnnz ) then

          fun = jcfun(i)
          var = jcvar(i)

          if ( fun .eq. 0 ) then
              g(var) = g(var) + jcval(i)

              if ( i .ne. jcnnz ) then
                  call intswap(jcfun(jcnnz),jcfun(i))
                  call intswap(jcvar(jcnnz),jcvar(i))
                  call dblswap(jcval(jcnnz),jcval(i))
              end if

              jcnnz = jcnnz - 1

          else
              i = i + 1
          end if

          go to 100
      end if

C     Duplicate entrances correspoding to splitted constraints

      jcnnztmp = jcnnz

      do i = 1,jcnnztmp

          fun = jcfun(i)
          fu2 = ccor(fun)

          if ( fu2 .ne. 0 ) then
              jcnnz = jcnnz + 1
              jcfun(jcnnz) = fu2
              jcvar(jcnnz) = jcvar(i)
              jcval(jcnnz) = ca(fu2) * jcval(i)
          end if

          jcval(i) = ca(fun) * jcval(i)

      end do

C     Add effect of slack variables

      do i = 1,m
          sind = slaind(i)
          if ( sind .ne. - 1 ) then
              jcnnz = jcnnz + 1
              jcfun(jcnnz) = i
              jcvar(jcnnz) = sind
              jcval(jcnnz) = - 1.0_rp_
          end if
      end do
      return

  910 continue
      write( 6, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      stop
      end

C     ******************************************************************
C     ******************************************************************

      subroutine evalgjacp(n,x,g,m,p,q,work,gotj,flag)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      logical gotj
      integer ( kind = ip_ ) flag,m,n
      character work

C     ARRAY ARGUMENTS
      real ( kind = rp_ ) g(n),p(m),q(n),x(n)

C     The usage of this subroutine for solving problems in the CUTEst
C     collection (in replacement of evalgjac)  was never evaluated.

      flag = - 1

      end

C     ******************************************************************
C     ******************************************************************

      subroutine intswap(i,j)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      integer ( kind = ip_ ) i,j

C     LOCAL SCALARS
      integer ( kind = ip_ ) tmp

      tmp = i
      i   = j
      j   = tmp

      end

C     ******************************************************************
C     ******************************************************************

      subroutine dblswap(a,b)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      real ( kind = rp_ ) a,b

C     LOCAL SCALARS
      real ( kind = rp_ ) tmp

      tmp = a
      a   = b
      b   = tmp

      end


! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

c  this is an amalgam of CUTEdriver.f and CUTEuser.f from the 
c  filterSQP source distribution

c  part from CUTEdriver.f

      program filter_driver
      
c     --------------------------------------------------------------
c     Solve NLP problems of the form
c     
c          minimize    f(x)
c          subject to  l_j <= c_j(x) <= u_j  ,    j = 1 , ... , m
c                      l_i <=   x_i  <= u_i  ,    i = 1 , ... , n
c
c     The problems solved are defined using CUTE.
c     --------------------------------------------------------------

c  CUTEr interface by Roger Fletcher and Sven Leyffer (U. Dundee)
c  Revised for CUTEst, Nick Gould, January 2013

      use CUTEST_KINDS_precision
      implicit none

      integer ( kind = ip_ ) kmax, maxa, maxf, mxwk, mxiwk, nnzh, nnzj
      integer ( kind = ip_ ) len_lws,len_ws, luser, liuser, status, mlp
c  CUTEst input unit
      integer ( kind = ip_ ), parameter :: input = 7

c     ... internal variables -- scalars
      integer ( kind = ip_ ) n, m, iprint, i, j, idummy, m_nln, ifail
      integer ( kind = ip_ ) nout, max_iter
      real ( kind = rp_ )    rho, f, fmin, CPU_start, CPU_end, CPU_total
      integer ( kind = ip_ ) istat(14)
      real ( kind = rp_ )  rstat(7)

c     ... internal variables -- allocatable arrays

      integer ( kind = ip_ ), allocatable, dimension( : ) :: la
      integer ( kind = ip_ ), allocatable, dimension( : ) :: iuser, lws
      real ( kind = rp_ ), allocatable, dimension( : ) :: a, x, blo, bup
      real ( kind = rp_ ), allocatable, dimension( : ) :: s, c, ws
      real ( kind = rp_ ), allocatable, dimension( : ) :: user, lam
      character ( len = 1 ), allocatable, dimension( : )  :: cstype
      character ( len = 10 ), allocatable, dimension( : )  :: xnames
      character ( len = 10 ), allocatable, dimension( : )  :: gnames
      logical, allocatable, dimension( : ) :: equatn, linear

c     ... common statements
      real ( kind = rp_ )                 infty, eps
      common /NLP_eps_inf/ infty, eps

c     ... common to indicate initial penalty parameter & updating or not
      real ( kind = rp_ )               given_mu
      logical                      update_mu
      common /penalty_c/ given_mu, update_mu

c     ... upper bound on filter
      real ( kind = rp_ )          ubd, tt
      common /ubdc/ ubd, tt

c     ... problem name (& length)
      integer ( kind = ip_ )         char_l
      character*10            pname
      common /cpname/ char_l, pname

c     ... trap IEEE exceptions
c     external abort,ieee_handler
c     integer ( kind = ip_ )  ieee_handler, abort

c     ... default options set here
c     data iprint, max_iter, nout, rho, idummy /1, 1000, 6, 10.0_rp_, 0/
      iprint = 1
      max_iter = 1000
      nout = 6
      rho = 10.0_rp_
      idummy = 0

c     ... trap IEEE exceptions
c     i=ieee_handler('set','invalid',abort)
c     i=ieee_handler('set','division',abort)
c     i=ieee_handler('set','overflow',abort)

c     ========================  procedure body  ======================

c     ... open SIF output file

C  open the relevant data input file

      OPEN( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *      STATUS = 'OLD' )
      REWIND( input )

c  compute problem dimensions

      call CUTEST_cdimen_r( status, input, n, m )
      IF ( status /= 0 ) GO TO 910

c  allocate space 

      allocate( x(n), blo(n+m), bup(n+m), c(m), lam(n+m),
     .         s(n+m), equatn(m), linear(m), xnames(n),
     .         gnames(m), cstype(m), stat = status )
      if ( status /= 0 ) GO TO 990

c     ... read spec.par parameter file (or use defaults)
      call readpar(iprint,kmax,maxf,max_iter,mlp,len_lws,len_ws,
     *             idummy,nout,rho,idummy)

c     ... initialization
      call initialize_NLP(n,m,blo,bup,x,lam,equatn,
     .                    linear,cstype,xnames,gnames)
      
c  compute the numbers of nonzeros in the constraint Jacobian and Hessian

      call CUTEST_cdimsj_r( status, nnzj )
      IF ( status /= 0 ) GO TO 910
      call CUTEST_cdimsh_r( status, nnzh )
      IF ( status /= 0 ) GO TO 910

c  allocate further space 

      maxa = nnzj + n
      luser = n
      liuser = maxa
      mxwk = 8 * ( n + m ) + 5 * n + m + 8 * maxf + nnzh + len_ws
      mxiwk = 2 * ( m + n )+ n + mlp + 101 + 2 * nnzh + len_lws
      allocate( la(0:maxa+m+2), iuser(liuser), lws(mxiwk),
     .          a(maxa), ws(mxwk),user(luser), stat = status )
      if ( status /= 0 ) GO TO 990

c     ... variable/constraint scale factors
      call readscale (n,m,xnames,gnames,pname,char_l,s,ifail)

c     ... initialize penalty parameter (for monotoring only)
      update_mu = .true.
      given_mu  = 1.D0

c     ... set fmin, kmax
      fmin  = - infty
      ifail = 0

c     ... call the main SQP routine
      CALL CPU_time( CPU_start )
      call filterSQP(n,m,MIN(n,kmax),maxa,maxf,mlp,mxwk,mxiwk,iprint,
     .               nout,ifail,rho,x,c,f,fmin,blo,bup,s,a,la,ws,lws,
     .               lam,cstype,user,iuser,max_iter,istat,rstat)
      CALL CPU_time( CPU_end )
      CPU_total = CPU_end - CPU_start
      if (iprint.ge.1) then
         write(nout,"( ' CPU time for this solve.............', 
     .     F6.2 )" ) CPU_total
      endif

c     ... count number of nonlinear c/s (for output)
      m_nln = 0
      do j=1,m
         if (cstype(j).eq.'N') m_nln = m_nln + 1
      enddo
c     ... save result in a compact format
      open (unit=12,file='00temp.txt')
      write(12,9001) pname, ifail, n, m, m_nln, istat(1)
      write(12,9001) pname, (istat(j),j=2,7)
      write(12,9001) pname, (istat(j),j=8,14)
      write(12,9002) pname, f,(rstat(j),j=5,7)
      write(12,9002) pname, rho,(rstat(j),j=1,4),CPU_total
      close(12)

c     ... write solution onto a files
      open(unit=1, file=pname(1:char_l)//'.solution')
      write(1,*) 'Problemname.........',pname
      write(1,*) 'No. of variables....',n
      write(1,*) 'No. of constraints..',m
      write(1,*) 'Solution:'
      write(1,*) '========='
      write(1,*) '  f* = ', f
      write(1,*)
      write(1,'(2A)')' Name    |    lower bd   |       x*      |',
     .               '  upper bd    | multiplier   | scales'
      write(1,'(2A)')'---------+---------------+---------------+',
     .               '--------------+--------------+---------'
      write(1,7002) (xnames(i),blo(i),x(i),bup(i),lam(i),s(i),i=1,n)
      write(1,*)
      write(1,'(2A)')' Name    |    lower bd   |      c(x*)    |',
     .               '  upper bd    | multiplier   | scales'
      write(1,'(2A)')'---------+---------------+---------------+',
     .               '--------------+--------------+---------'
      write(1,7003)(gnames(i),blo(n+i),c(i),bup(n+i),lam(n+i),
     .              s(n+i),cstype(i),i=1,m)
      write(1,*)
      write(1,*) '------------------------------------------------',
     .     '-----------------------------'
      close(1)

 7002 format(A,5G15.7)
 7003 format(A,5G15.7,A)
 9001 format(A,7(1X,I6))
 9002 format(A,6(G10.4,1X))

      stop

  910 continue
      write( 6, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      stop

  990 continue
      write( 6, "( ' Allocation error, status = ', I0 )" ) status
      stop

      end

c  part from CUTEuser.f

c     *******************************************************************
c     User supplied problem specific routines
c     =======================================
c     * initialize_NLP  - initialize the NLP, call CUTE, set up problem
c     * confun          - evaluate the constraints c(x)
c     * objfun          - evaluate the objective function f(x)
c     * gradient        - evaluate the gradients of the constraints
c     * hessian         - evaluate the Hessian of the Lagrangian
c     * gdotx           - forms v = G.x for Hessian G called by bqpd    
c     
c     *******************************************************************

      subroutine initialize_NLP (n, m, blo, bup, x, lam, equatn, 
     .                           linear, cstype, xnames, gnames )

      use CUTEST_KINDS_precision
      implicit none

      integer ( kind = ip_ ), parameter :: io_buffer = 11

c     ... decalartion of passed parameters -- scalars
      integer ( kind = ip_ ) n, m

c     ... decalartion of passed parameters -- arrays
      real ( kind = rp_ )    blo(n+m), bup(n+m), x(n), lam(n+m)
      logical equatn(m), linear(m)
      character*10 xnames(n), gnames(m)
      character    cstype(m)

c     ... declaration of common blocks
      integer ( kind = ip_ )         char_l
      character*10            pname
      common /cpname/ char_l, pname

c     ... declaration of internal parameters
      integer ( kind = ip_ )    input
      parameter (input=7)

c     ... declaration of internal variables -- scalars
      integer ( kind = ip_ ) i, status

c     =======================  procedure body  =========================

c     ... open SIF output file
c     open (file='OUTSDIF.d', unit=input)
c     ... read SIF output file and set up the constraints
      call CUTEST_csetup_r(status,input, 6, io_buffer, n, m, x, 
     .                     blo, bup, lam(n+1), blo(n+1), bup(n+1),
     .                     equatn, linear, 0, 1, 0)
      if ( status /= 0 ) GO TO 910

c     ... shift bounds to be consecutive & find indices of nonlin. c/s
      do i=1,m
         if (linear(i)) then
            cstype(i) = 'L'
         else
            cstype(i) = 'N'
         endif
      enddo
c      print *, 'WARNING: ALL CONSTAINTS TREATED AS NONLINEAR'

c     ... obtain names of problem, variables and constraints
      call CUTEST_cnames_r(status, n, m, pname, xnames, gnames)
      if ( status /= 0 ) GO TO 910

c     ... truncate pname if necessary and open main output file
      char_l = 10
      do i=10,1,-1
         if (pname(i:i).eq.' ') then
            char_l = i-1
         endif
      enddo

      return

  910 continue
      write( 6, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      stop

      end

c     ******************************************************************


      subroutine confun(x, n, m, c, a, la, user, iuser, flag)

      use CUTEST_KINDS_precision
      implicit none

c     ... declaration of passed parameters -- scalars
      integer ( kind = ip_ ) n, m, flag

c     ... declaration of passed parameters -- arrays
      real ( kind = rp_ )    x(n), c(m), a(*), user(*)
      integer ( kind = ip_ ) la(0:*), iuser(*)

c     ... declaration of internal variables
      logical jtrans, grad
      integer ( kind = ip_ ) lcjac1, lcjac2, status

c     ... IEEE floatig point exception handling
c     external  ieee_handler, abort
c     integer ( kind = ip_ )   ieee_flags, ieeer, ieee_handler
c     external  ieee_flags
      character out*16

c     =======================  procedure body  =========================

      lcjac1 = n
      lcjac2 = m
      jtrans = .true.
      grad   = .false.

c     ... clear all previous occurrences of IEEE exception
      out = ''
c     ieeer = ieee_flags ( 'clearall','all','',out )

c     ... switch off IEEE trapping (for this routine)
c     ieeer=ieee_handler('clear','invalid',abort)
c     ieeer=ieee_handler('clear','division',abort)
c     ieeer=ieee_handler('clear','overflow',abort)

c     ... call CUTE's constraint function
      call CUTEST_ccfg_r(status, n, m, x, c, jtrans, lcjac1, lcjac2,
     .                   a, grad)
      if ( status /= 0 ) GO TO 910

c     ... switch IEEE trapping back on
c     ieeer=ieee_handler('set','invalid',abort)
c     ieeer=ieee_handler('set','division',abort)
c     ieeer=ieee_handler('set','overflow',abort)

c     ... check for  IEEE exception
c     ieeer = ieee_flags ( 'get','exception','',out )
c     if ((out.eq.'division').or.(out.eq.'overflow')
c    .     .or.(out.eq.'invalid')) then
c        print *,'WARNING: IEEE exception ',out,' occurred in confun'
c        flag = 1
c     else
         flag = 0
c     endif
      return

  910 continue
      write( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      stop

      end


c     ******************************************************************


      subroutine objfun(x, n, f, user, iuser, flag)

      use CUTEST_KINDS_precision
      implicit none

c     ... declaration of passed parameters -- scalars
      integer ( kind = ip_ ) n, flag
      real ( kind = rp_ )    f

c     ... declaration of passed parameters -- arrays
      real ( kind = rp_ )    x(n), user(*)
      integer ( kind = ip_ ) iuser(*)

c     ... declaration of internal variables
      integer ( kind = ip_ ) status
      logical grad

c     ... IEEE floatig point exception handling
c     external  ieee_handler, abort
c     integer ( kind = ip_ )   ieee_flags, ieeer, ieee_handler
c     external  ieee_flags
      character out*16

c     =======================  procedure body  =========================

      grad=.false.

c     ... clear all previous occurrences of IEEE exception
      out = ''
c     ieeer = ieee_flags ( 'clearall','all','',out )

c     ... switch off IEEE trapping (for this routine)
c     ieeer=ieee_handler('clear','invalid',abort)
c     ieeer=ieee_handler('clear','division',abort)
c     ieeer=ieee_handler('clear','overflow',abort)

c     ... call CUTE's objective function
      call CUTEST_cofg_r( status, n, x, f, user, grad)
      if ( status /= 0 ) GO TO 910

c     ... switch IEEE trapping back on
c     ieeer=ieee_handler('set','invalid',abort)
c     ieeer=ieee_handler('set','division',abort)
c     ieeer=ieee_handler('set','overflow',abort)

c     ... check for  IEEE exception
c     ieeer = ieee_flags ( 'get','exception','',out )
c     if ((out.eq.'division').or.(out.eq.'overflow')
c    .     .or.(out.eq.'invalid')) then
c        print *,'WARNING: IEEE exception ',out,' occurred in objfun'
c        flag = 1
c     else
         flag = 0
c   endif
      return

  910 continue
      write( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      stop

      end

c     ******************************************************************

      subroutine gradient(n,m,mxa,x,a,la,maxa,user,iuser,flag)

      use CUTEST_KINDS_precision
      implicit none

c     ... declaration of passed parameters -- scalars
      integer ( kind = ip_ ) n, m, mxa, maxa, flag

c     ... declaration of passed parameters -- arrays
      integer ( kind = ip_ ) la(0:*), iuser(*)
      real ( kind = rp_ )    x(n), a(maxa), user(*)

c     ... declaration of internal variables
      logical grlagf
      integer ( kind = ip_ ) i, ii, j, jj, pjp, status

c     ... IEEE floatig point exception handling
c     external  ieee_handler, abort
c     integer ( kind = ip_ )   ieee_flags, ieeer, ieee_handler
c     external  ieee_flags
      character out*16

c     =======================  procedure body  =========================

      grlagf = .false.

c     ... clear all previous occurrences of IEEE exception
      out = ' '
c     ieeer = ieee_flags ( 'clearall','all','',out )

c     ... switch off IEEE trapping (for this routine)
c     ieeer=ieee_handler('clear','invalid',abort)
c     ieeer=ieee_handler('clear','division',abort)
c     ieeer=ieee_handler('clear','overflow',abort)

c     ... call CUTE's sparse Jacobian evaluation (shift to allow for objective)
      call CUTEST_csgr_r(status,n,m,x,user,grlagf,mxa,maxa-n,a(n+1),
     .                 la(n+1),iuser)
      if ( status /= 0 ) GO TO 910

cGRD      print *,'--------------------------------------------------------'
cGRD      print *,'Jacobian from csgr: i, ip(i), jp(i), a(i)'
cGRD      do i=1,mxa
cGRD         print *, i, la(n+i), iuser(i), a(n+i)
cGRD      enddo
cGRD      print *,'--------------------------------------------------------'

c     ... switch IEEE trapping back on
c     ieeer=ieee_handler('set','invalid',abort)
c     ieeer=ieee_handler('set','division',abort)
c     ieeer=ieee_handler('set','overflow',abort)

c     ... check for  IEEE exception
c     ieeer = ieee_flags ( 'get','exception','',out )
c     if ((out.eq.'division').or.(out.eq.'overflow')
c    .     .or.(out.eq.'invalid')) then
c        print *,'WARNING: IEEE exception ',out,' occurred in gradient'
c        flag = 1
c        goto 999
c     else
         flag = 0
c     endif

      if (mxa+n.gt.maxa) then
         print *, 'STOP: Too many Jacobian entries:', mxa+n, maxa
         stop
      endif

c     ... copy CUTE's coordinate format into sparse vector format
      pjp       = mxa+n+1
      la(0)     = pjp
      la(pjp+0) = 1
      la(pjp+1) = 1+n
      ii        = n

c     ... columns are in ascending order; construct pointers to start
      j = 1
      do i=1,mxa
         if (iuser(i).eq.j) then
            ii = ii+1
         elseif (iuser(i).ge.j+1) then
            do jj=j,iuser(i)-1
               la(pjp+jj+1) = ii+1
            enddo
            j  = iuser(i)
            ii = ii+1
         elseif ((iuser(i).lt.j).and.(iuser(i).ne.0)) then
            print *, 'ERROR in CUTE Jacobian',j,iuser(i),i
            do ii=1,i
               print 1001, a(n+ii), la(n+ii), iuser(ii), ii
            enddo
            stop
         endif
      enddo
      do jj=j,m
         la(pjp+jj+1) = ii+1
      enddo

c     ... find objective columns
      do i=1,n
         user(i) = 0.0_rp_
      enddo
      ii = max( 1 , ii-n )
      do i=ii,mxa
         if (iuser(i).eq.0) then
            user(la(n+i)) = a(n+i)
         endif
      enddo
      
c     ... copy objective gradient back
      do i=1,n
         la(i) = i
         a (i) = user(i)
      enddo
      mxa = mxa + n

cGRD      print *,'pjp ',la(0)
cGRD      print *,'la  ',(la(i),i=1,la(0)-1)
cGRD      print *,'a   ',(a(i),i=1,la(0)-1)
cGRD      print *,'jp  ',(la(i),i=la(0),la(0)+m+1)

c999  continue
 1001 format(G12.4,3I5)
      
      return

  910 continue
      write( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      stop

      end


c     ******************************************************************


      subroutine hessian (x,n,m,phase,lam,ws,lws,user,iuser,l_hess,
     .                    li_hess,flag)

      use CUTEST_KINDS_precision
      implicit none

c     ... declaration of passed parameters -- scalars
      integer ( kind = ip_ ) n, m, phase, flag, l_hess, li_hess

c     ... declaration of passed parameters -- arrays
      real ( kind = rp_ )    x(n), lam(n+m), ws(*), user(*)
      integer ( kind = ip_ ) lws(*), iuser(*)

c     ... declaration of internal variables
      integer ( kind = ip_ ) hess_length, i, maxhess, n1, status

c     ... user's common with Hessian storage map, used in Wdotd
      integer ( kind = ip_ )        phl, phr, phc
      common /hessc/ phl, phr, phc

c     ... IEEE floatig point exception handling
c     external  ieee_handler, abort
c     integer ( kind = ip_ )   ieee_flags, ieeer, ieee_handler
c     external  ieee_flags
      character out*16

c     =======================  procedure body  =========================

c     ... max storage available for Hessian is ...
      maxhess = min ( l_hess , (li_hess-1) / 2 )

c     ... set storage map for Hessian entries
      phl = 1
      phr = 1

c     ... clear all previous occurrences of IEEE exception
      out   = ''
c     ieeer = ieee_flags   ( 'clearall','all','',out )

c     ... switch off IEEE trapping (for this routine)
c     ieeer=ieee_handler('clear','invalid',abort)
c     ieeer=ieee_handler('clear','division',abort)
c     ieeer=ieee_handler('clear','overflow',abort)
      
c     ... set multipliers for CUTE's Hessian evaluation *** UNDO AT END ***
      do i=1,m
         lam(n+i) = -lam(n+i)
      enddo

c     ... evaluate the Hessian of the Lagrangian and store in ws, lws
      if (m.gt.0) then
         n1 = n+1
      else
         n1 = n
      endif
      if (phase.eq.1) then
c        ... call CUTE's Hessian WITHOUT f(x) contribution
         call CUTEST_cshc_r( status, n, m, x, lam(n1), hess_length, 
     .                     maxhess, ws, lws(2), lws(2+maxhess))
      else
c        ... call CUTE's normal Hessian routine
         call CUTEST_csh_r(status, n, m, x, lam(n1), hess_length, 
     .                     maxhess, ws, lws(2), lws(2+maxhess))
      endif
      if ( status /= 0 ) GO TO 910

c     ... copy multipliers back to normal
      do i=1,m
         lam(n+i) = -lam(n+i)
      enddo

c     ... switch IEEE trapping back on
c     ieeer=ieee_handler('set','invalid',abort)
c     ieeer=ieee_handler('set','division',abort)
c     ieeer=ieee_handler('set','overflow',abort)

c     ... check for  IEEE exception
c     ieeer = ieee_flags ( 'get','exception','',out )
c     if ((out.eq.'division').or.(out.eq.'overflow')
c    .     .or.(out.eq.'invalid')) then
c        print *,'WARNING: IEEE exception ',out,' occurred in hessian'
c        flag = 1  
c        lws(phl) = 0
c        l_hess   = 0
c        li_hess  = 0
c        goto 999
c     else
         flag = 0
c     endif

      if (hess_length.gt.maxhess) then
         print *, 'STOP, not enough Hessian workspace'
         print *, 'hess_length, maxhess=',hess_length,maxhess
         STOP
      endif

c     ... shift column indices into right order; set length of Hessian ws
      do i=1,hess_length
         lws(1+hess_length+i)=lws(1+maxhess+i)
      enddo

c     ... set storage map for Hessian entries
      phc = phr + hess_length

c     ... save number of Hessian entries
      lws(phl) = hess_length

c     ... set length of Hessian storage used here 
      l_hess  = hess_length 
      li_hess = 1 + 2*hess_length

c999  continue
      return

  910 continue
      write( out, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      stop

      end
 

c     ******************************************************************


      subroutine Wdotd (n, d, ws, lws, v)

      use CUTEST_KINDS_precision
      implicit none

c     ... declaration of passed parameters
      integer ( kind = ip_ ) n, lws(*)
      real ( kind = rp_ )    d(n), v(n), ws(*)

c     ... declaration of internal variables
      integer ( kind = ip_ ) i, hl, row, col

c     ... storage map for hessian 
      integer ( kind = ip_ )        phl, phr, phc
      common /hessc/ phl, phr, phc

c     ========================  procedure body  =========================

c     ... form v = W.d from sparse, upper triangular Hessian
      if (phl.gt.0) then 
         hl = lws(phl)
         do i=1,hl
            row = lws(phr+i)
            col = lws(phc+i)
            v(row) = v(row) + ws(i)*d(col)
            if (row.ne.col) then
               v(col) = v(col) + ws(i)*d(row)
            endif
         enddo
      endif

      return
      end


c     *******************************************************************


      subroutine ident_Hessian (n,lws,ws,a)

c     ========================================================================
c     Set initial Hessian = I and gradient = 0, to get l_2 closest feas. point 
c     ========================================================================

      use CUTEST_KINDS_precision
      implicit none

c     ... declaration of passed parameters
      integer ( kind = ip_ ) n, lws(*)
      real ( kind = rp_ )    ws(*), a(*)

c     ... storage map for hessian 
      integer ( kind = ip_ )        phl, phr, phc
      common /hessc/ phl, phr, phc

c     ... declaration of internal variables
      integer ( kind = ip_ ) i

c     ========================  procedure body  =========================

c     ... set storage map for Hessian entries
      phl = 1
      phr = 1
      phc = phr + n
      lws(phl) = n

c     ... unit Hessian matrix for initial step towards constraints
      do i=1,n
         lws(phr+i) = i
         lws(phc+i) = i
         ws (    i) = 1.0_rp_
         a  (    i) = 0.0_rp_
      enddo

      return
      end

c     **************************** E N D *********************************



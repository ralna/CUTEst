! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"

C  Dummy FILTERSQP for testing filtersqp_main interface to CUTEst

C  Nick Gould,  9th January 2013

      subroutine readpar (iprint, kmax, maxf, maxiter, mlp, mxiwk,
     .                    mxwk, nobj, nout, rho, stackmax)
      use CUTEST_KINDS_precision
      implicit none
c     ... declaration of passed parameters
      integer ( kind = ip_ ) iprint, kmax, maxf, maxiter, mlp
      integer ( kind = ip_ ) mxiwk, mxwk, nobj, nout,
     .        stackmax
      real ( kind = rp_ ) rho
      real ( kind = rp_ ) infty, eps
      common /NLP_eps_inf/ infty, eps
c print flag
      iprint = 1
c max. number of iterations allowed to SQP solver
      maxiter = 1
c output channel
      nout = 6
c max dimension of null space
      kmax = 2000
c max length of filter
      maxf = 50
c level for resolving degeneracy in QP
      mlp = 200
c length of non-cutest real workspace required by filterSQP
      mxwk = 10000000
c length of non-cutest integer ( kind = ip_ ) workspace required by filterSQP
      mxiwk = 500000
c initial trust region radius
      rho = 10.0

      infty = HUGE( 1.0_rp_ )
      eps = EPSILON( 1.0_rp_ )
      return
      end

      subroutine readscale (n,m,vname,cname,fname,flen,scale,ifail)
      use CUTEST_KINDS_precision
      integer ( kind = ip_ ) n, m, ifail, flen
      real ( kind = rp_ ) scale(n+m)
      character*10 vname(n), cname(m)
      character*10 fname
      return
      end

      subroutine filterSQP (n,m,kmax,maxa,maxf,mlp,mxwk,mxiwk,iprint,
     .                     nout,ifail,rho,x,c,f,fmin,blo,bup,s,a,la,ws,
     .                     lws,lam,cstype,user,iuser,max_iter,istat,
     .                     rstat)
      use CUTEST_KINDS_precision
      integer ( kind = ip_ ) n, m, kmax, maxa, maxf, mlp, mxwk, mxiwk,
     .        iprint, nout, ifail, max_iter
      real ( kind = rp_ ) rho, f, fmin
      integer ( kind = ip_ ) la(0:maxa+m+2), lws(mxiwk)
      integer ( kind = ip_ ) iuser(*), istat(14)
      real ( kind = rp_ ) a(maxa), blo(n+m), bup(n+m), x(n), c(m),
     .        lam(n+m), ws(mxwk), user(*), rstat(7), s(n+m)
      character cstype(m)
      integer ( kind = ip_ ) flag, l_hess, li_hess, i, mxa

      l_hess  = mxwk
      li_hess = mxiwk

      call objfun(x, n, f, user, iuser, flag)
      call confun(x, n, m, c, a, la, user, iuser, flag)
      call gradient(n,m,mxa,x,a,la,maxa,user,iuser,flag)
      call hessian (x,n,m,1,lam,ws,lws,user,iuser,l_hess,
     .              li_hess,flag)
      call hessian (x,n,m,2,lam,ws,lws,user,iuser,l_hess,
     .              li_hess,flag)

      istat(1)  = kmax
      istat(2)  = 1
      istat(3)  = 0
      istat(4)  = 1
      istat(5)  = 1
      istat(6)  = 1
      istat(7)  = 2
      istat(8)  = 0
      istat(9)  = 0
      istat(10) = 0
      istat(11) = 0
      istat(12) = 0
      istat(13) = 0
      istat(14) = 0
      rstat(1) = 0.0_rp_
      rstat(2) = 0.0_rp_
      rstat(3) = 0.0_rp_
      rstat(4) = 0.0_rp_
      rstat(5) = 0.0_rp_
      rstat(6) = 0.0_rp_
      rstat(7) = 0.0_rp_
      DO 10 i = 1, m + n
        lam( i ) = 0.0_rp_
        s( i ) = 1.0_rp_
   10 Continue
      ifail = 6
      return
      end

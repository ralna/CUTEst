C     ( Last modified on 9 Jan 2013 at 12:40:00 )

C  Dummy FILTERSQP for testing filtersqp_main interface to CUTEst
C  Nick Gould,  9th January 2013

      subroutine readpar (iprint, kmax, maxf, maxiter, mlp, mxiwk,
     .                    mxwk, nobj, nout, rho, stackmax)
      implicit none
c     ... declaration of passed parameters
      integer iprint, kmax, maxf, maxiter, mlp, mxiwk, mxwk, nobj, nout,
     .        stackmax
      double precision rho
      double precision infty, eps
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
c length of non-cutest integer workspace required by filterSQP
      mxiwk = 500000
c initial trust region radius
      rho = 10.0

      infty = HUGE( 1.0D+0 )
      eps = EPSILON( 1.0D+0 )
      return
      end

      subroutine readscale (n,m,vname,cname,fname,flen,scale,ifail)
      integer n, m, ifail, flen
      double precision         scale(n+m)
      character*10 vname(n), cname(m)
      character*10 fname
      return
      end

      subroutine filterSQP (n,m,kmax,maxa,maxf,mlp,mxwk,mxiwk,iprint,
     .                     nout,ifail,rho,x,c,f,fmin,blo,bup,s,a,la,ws,
     .                     lws,lam,cstype,user,iuser,max_iter,istat,
     .                     rstat)
      integer n, m, kmax, maxa, maxf, mlp, mxwk, mxiwk,
     .        iprint, nout, ifail, max_iter
      double precision    rho, f, fmin
      integer la(0:maxa+m+2), lws(mxiwk), iuser(*), istat(14)
      double precision    a(maxa), blo(n+m), bup(n+m), x(n), c(m),
     .        lam(n+m), ws(mxwk), user(*), rstat(7), s(n+m)
      character cstype(m)
      integer flag, l_hess, li_hess, i, mxa

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
      rstat(1) = 0.0
      rstat(2) = 0.0
      rstat(3) = 0.0
      rstat(4) = 0.0
      rstat(5) = 0.0
      rstat(6) = 0.0
      rstat(7) = 0.0
      DO 10 i = 1, m + n
        lam( i ) = 0.0D0
        s( i ) = 1.0D0
   10 Continue
      ifail = 6
      return
      end

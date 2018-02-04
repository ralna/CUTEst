!     ( Last modified on 18 Feb 2013 at 13:50:00 )

!  Dummy SPG for testing spg_main interface to CUTEst
!  Nick Gould, 18th February 2013

      subroutine spg(n,x,epsopt,maxit,maxfc,iprint,f,gpsupn,iter,fcnt,
     +               spginfo,inform)

      implicit none

C     SCALAR ARGUMENTS
      double precision gpsupn,epsopt,f
      integer fcnt,inform,iprint,iter,maxfc,maxit,n,spginfo

C     ARRAY ARGUMENTS
      double precision x(n)
      double precision g(n)
      call evalf(n,x,f,inform)
      if ( inform .ne. 0 ) call evalg(n,x,g,inform)
      end

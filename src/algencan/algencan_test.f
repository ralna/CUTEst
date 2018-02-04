!     ( Last modified on 18 Feb 2013 at 13:50:00 )

!  Dummy ALGENCAN for testing algencan_main interface to CUTEst
!  Nick Gould, 18th February 2013

      subroutine algencan(epsfeas,epsopt,efacc,eoacc,iprint,ncomp,n,x,l,
     +u,m,lambda,equatn,linear,coded,checkder,fu,cnormu,snorm,nlpsupn,
     +inform)

      implicit none

C     SCALAR ARGUMENTS
      logical checkder
      integer inform,iprint,m,n,ncomp
      double precision cnormu,efacc,eoacc,epsfeas,epsopt,fu,nlpsupn,
     +        snorm

C     ARRAY ARGUMENTS
      logical coded(11),equatn(m),linear(m)
      double precision l(n),lambda(m),u(n),x(n)

      integer flag
      double precision c(m)
      call evalfc(n,x,fu,m,c,flag)
      inform=0
      end

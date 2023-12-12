! THIS VERSION: CUTEST 2.2 - 2023-11-29 AT 12:30 GMT.

#include "cutest_modules.h"

!  Dummy ALGENCAN for testing algencan_main interface to CUTEst
!  Nick Gould, 18th February 2013

      subroutine algencan(epsfeas,epsopt,efacc,eoacc,iprint,ncomp,n,x,l,
     +u,m,lambda,equatn,linear,coded,checkder,fu,cnormu,snorm,nlpsupn,
     +inform)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      logical checkder
      integer ( kind = ip_ ) inform,iprint,m,n,ncomp
      real ( kind = rp_ ) cnormu,efacc,eoacc,epsfeas,epsopt,fu,nlpsupn,
     +        snorm

C     ARRAY ARGUMENTS
      logical coded(11),equatn(m),linear(m)
      real ( kind = rp_ ) l(n),lambda(m),u(n),x(n)

      integer ( kind = ip_ ) flag
      real ( kind = rp_ ) c(m)
      call evalfc(n,x,fu,m,c,flag)
      inform=0
      end

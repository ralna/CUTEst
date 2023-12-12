! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"

!  Dummy SPG for testing spg_main interface to CUTEst

!  Nick Gould, 18th February 2013

      subroutine spg(n, x, epsopt, maxit, maxfc, iprint, f, gpsupn,
     +               iter, fcnt, spginfo, inform)

      use CUTEST_KINDS_precision
      implicit none

C     SCALAR ARGUMENTS
      real ( kind = rp_ ) gpsupn, epsopt, f
      integer ( kind = ip_ ) fcnt, inform, iprint, iter
      integer ( kind = ip_ ) maxfc, maxit, n, spginfo

C     ARRAY ARGUMENTS
      real ( kind = rp_ ) x( n )
      real ( kind = rp_ ) g( n )
      call evalf( n, x, f, inform )
      if ( inform .ne. 0 ) call evalg( n, x, g, inform )
      end

! THIS VERSION: CUTEST 2.2 - 2023-11-24 AT 10:40 GMT.

#include "cutest_modules.h"

!  Dummy DFO for testing dfo_main interface to CUTEst
!  Nick Gould, 7th January 2013

      SUBROUTINE DFO( n, nx, X, ldx, FX, CONX, ifiniv, m, C, nclin , ncnln,    &
                      LB, UB, A, lda, XNAMES, pname, CNAMES, it, nf, info,     &
                      maxit,  maxnf, stpcrtr, delmin, stpthr, cnstolp, delta,  &
                      pp, scale, ioutp, iprintp )

      USE CUTEST_KINDS_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ) :: n, m, nx, ldx, nclin, ncnln, lda, it, nf, info
      INTEGER ( KIND = ip_ ) :: maxnf, maxit, stpcrtr, scale, ioutp, iprintp
      LOGICAL ( KIND = ip_ ) :: ifiniv
      CHARACTER ( LEN = 256 ) :: pname
      REAL ( KIND = rp_ ) :: delmin, stpthr, cnstolp, delta, pp
      REAL ( KIND = rp_ ) :: X( ldx * nx ), FX( nx ) , LB( * ), UB( * ) 
      REAL ( KIND = rp_ ) :: C( * ), CONX( * ), A( lda * n )
      CHARACTER ( LEN = 256 ) :: XNAMES( N ), CNAMES( * )

!  local variables

      LOGICAL :: iferr

      CALL FUN( n, m, X, FX( 1 ), C, iferr )
      it = 1
      nf = 1
      info = 2
      RETURN
      END SUBROUTINE DFO

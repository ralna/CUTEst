! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"

C  Dummy FILTERSD for testing filtersd_main interface to CUTEst

C  Nick Gould,  30th January 2013

      SUBROUTINE filterSD(n, m, x, al, f, fmin, cstype, bl, bu, ws, lws,
     *  v, nv, maxa, maxla, maxu, maxiu, kmax, maxg, rho, htol, rgtol,
     *  maxit, iprint, nout, ifail)
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n, m, nv ,maxa, maxla, maxu, maxiu, kmax
      INTEGER ( KIND = ip_ ) :: maxg, maxit, iprint, nout, ifail
      REAL ( KIND = rp_ ) :: f, fmin, rho, htol, rgtol
      INTEGER ( KIND = ip_ ) :: lws( * )
      REAL ( KIND = rp_ ) :: x( * ), al( * ), bl( * ), bu( * )
      REAL ( KIND = rp_ ) :: ws( * ), v( * )
      CHARACTER :: cstype( * )

      INTEGER ( KIND = ip_ ) :: last1, ncx1, npv, ngr, ninf
      INTEGER ( KIND = ip_ ) :: mlp, mxf, ipeq, k, itn, nft, ngt, iter
      REAL ( KIND = rp_ ) :: ainfty, ubd, dnorm, h, hJt, hJ
      REAL ( KIND = rp_ ) :: rgnorm, vstep

      COMMON / defaultc / ainfty, ubd, mlp, mxf
      COMMON / statsc / dnorm, h, hJt, hJ, ipeq, k, itn, nft, ngt
      COMMON / infoc / rgnorm, vstep, iter, npv, ngr, ninf

      last1 = maxu + 1
      ncx1 = last1 + 2 * maxa
      CALL FUNCTIONS( n, m, x, f, ws( ncx1 ),ws, lws )
      CALL GRADIENTS( n, m, x, ws( last1 ), ws, lws )
      h = 1.0_rp_
      ubd = 1.0_rp_
      rgnorm = 1.0_rp_
      k = n
      itn = 0
      nft = 1
      ngt = 2
      ifail = 5
      RETURN
      END

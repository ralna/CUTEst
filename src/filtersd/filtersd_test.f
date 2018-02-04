C     ( Last modified on 35 Jan 2016 at 14:40:00 )

C  Dummy FILTERSD for testing filtersd_main interface to CUTEst
C  Nick Gould,  30th January 2013

      SUBROUTINE filterSD(n, m, x, al, f, fmin, cstype, bl, bu, ws, lws,
     *  v, nv, maxa, maxla, maxu, maxiu, kmax, maxg, rho, htol, rgtol,
     *  maxit, iprint, nout, ifail)
      INTEGER :: n, m, nv ,maxa, maxla, maxu, maxiu, kmax
      INTEGER :: maxg, maxit, iprint, nout, ifail
      DOUBLE PRECISION :: f, fmin, rho, htol, rgtol
      INTEGER :: lws( * )
      DOUBLE PRECISION :: x( * ), al( * ), bl( * ), bu( * )
      DOUBLE PRECISION :: ws( * ), v( * )
      CHARACTER :: cstype( * )

      INTEGER :: last1, ncx1
      INTEGER :: mlp, mxf, ipeq, k, itn, nft, ngt, iter, npv, ngr, ninf
      DOUBLE PRECISION :: ainfty, ubd, dnorm, h, hJt, hJ, rgnorm, vstep

      COMMON / defaultc / ainfty, ubd, mlp, mxf
      COMMON / statsc / dnorm, h, hJt, hJ, ipeq, k, itn, nft, ngt
      COMMON / infoc / rgnorm, vstep, iter, npv, ngr, ninf

      last1 = maxu + 1
      ncx1 = last1 + 2 * maxa
      CALL FUNCTIONS( n, m, x, f, ws( ncx1 ),ws, lws )
      CALL GRADIENTS( n, m, x, ws( last1 ), ws, lws )
      h = 1.0D0
      ubd = 1.0D0
      rgnorm = 1.0D0
      k = n
      itn = 0
      nft = 1
      ngt = 2
      ifail = 5
      RETURN
      END

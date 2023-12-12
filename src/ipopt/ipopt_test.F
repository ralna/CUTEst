! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"

C  Dummy IPOPT for testing ipopt_main interface to CUTEst

C  Nick Gould, 15th January 2013

      INTEGER FUNCTION IPSOLVE( iproblem, X, G, f, LAM, Z_L, Z_U,
     *                          IDAT, DAT )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: iproblem
      REAL ( KIND = rp_ ) :: f
      INTEGER ( KIND = ip_ ) :: IDAT( * )
      REAL ( KIND = rp_ ) :: X( * ), G( * ), LAM( * )
      REAL ( KIND = rp_ ) :: DAT( * ), Z_l( * ), Z_u( * )

      INTEGER ( KIND = ip_ ) :: n, m, nza, nzh, ierr
      COMMON / cutest_ipdims / n, m, nza, nzh

      CALL EV_F( n, X, 0, f, IDAT, DAT, ierr )
      CALL EV_GRAD_F( n, X, 0, DAT, IDAT, DAT, ierr )
      CALL EV_G( n, X, 0, m, G, IDAT, DAT, ierr )
      CALL EV_JAC_G( 0, n, X, 0, m, nza, IDAT(1), IDAT(nza+1), DAT,
     1     IDAT, DAT, ierr )
      CALL EV_JAC_G( 1, n, X, 0, m, nza, IDAT(1), IDAT(nza+1), DAT,
     1     IDAT, DAT, ierr )
      CALL EV_HESS( 0, N, X, 0, 1.0_rp_, m, LAM, 0,
     1     nzh, IDAT(1), IDAT(nzh+1), DAT, IDAT, DAT, ierr)
      CALL EV_HESS( 1, N, X, 0, 1.0_rp_, m, LAM, 0,
     1     nzh, IDAT(1), IDAT(nzh+1), DAT, IDAT, DAT, ierr)
      IPSOLVE = 1
      RETURN
      END

      INTEGER FUNCTION IPCREATE( n, X_L, X_U, m, G_L, G_U, nza,
     *  nzh, idx_style, EV_F, EV_G, EV_GRAD_F, EV_JAC_G, EV_HESS )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n, m, nza, nzh, idx_style
      REAL ( KIND = rp_ ) :: X_l( n ), X_u( n ), G_l( m ), G_u( m )
      EXTERNAL :: EV_F, EV_G, EV_GRAD_F, EV_JAC_G, EV_HESS

      INTEGER ( KIND = ip_ ) :: sn, sm, snza, snzh
      COMMON / cutest_ipdims / sn, sm, snza, snzh
      sn = n
      sm = m
      snza = nza
      snzh = nzh

      IPCREATE = 1
      RETURN
      END

      SUBROUTINE IPFREE( iproblem )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: iproblem
      RETURN
      END

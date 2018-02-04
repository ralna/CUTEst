C     ( Last modified on 15 Jan 2013 at 16:00:00 )

C  Dummy IPOPT for testing ipopt_main interface to CUTEst
C  Nick Gould, 15th January 2013

      INTEGER FUNCTION IPSOLVE( iproblem, X, G, f, LAM, Z_L, Z_U,
     *                          IDAT, DAT )
      INTEGER :: iproblem
      DOUBLE PRECISION :: f
      INTEGER :: IDAT( * )
      DOUBLE PRECISION :: X( * ), G( * ), LAM( * ), Z_l( * ), Z_u( * )
      DOUBLE PRECISION :: DAT( * )

      INTEGER :: n, m, nza, nzh, ierr
      COMMON / cutest_ipdims / n, m, nza, nzh

      CALL EV_F( n, X, 0, f, IDAT, DAT, ierr )
      CALL EV_GRAD_F( n, X, 0, DAT, IDAT, DAT, ierr )
      CALL EV_G( n, X, 0, m, G, IDAT, DAT, ierr )
      CALL EV_JAC_G( 0, n, X, 0, m, nza, IDAT(1), IDAT(nza+1), DAT,
     1     IDAT, DAT, ierr )
      CALL EV_JAC_G( 1, n, X, 0, m, nza, IDAT(1), IDAT(nza+1), DAT,
     1     IDAT, DAT, ierr )
      CALL EV_HESS( 0, N, X, 0, 1.0D+0, m, LAM, 0,
     1     nzh, IDAT(1), IDAT(nzh+1), DAT, IDAT, DAT, ierr)
      CALL EV_HESS( 1, N, X, 0, 1.0D+0, m, LAM, 0,
     1     nzh, IDAT(1), IDAT(nzh+1), DAT, IDAT, DAT, ierr)
      IPSOLVE = 1
      RETURN
      END

      INTEGER FUNCTION IPCREATE( n, X_L, X_U, m, G_L, G_U, nza,
     *  nzh, idx_style, EV_F, EV_G, EV_GRAD_F, EV_JAC_G, EV_HESS )
      INTEGER :: n, m, nza, nzh, idx_style
      DOUBLE PRECISION :: X_l( n ), X_u( n ), G_l( m ), G_u( m )
      EXTERNAL :: EV_F, EV_G, EV_GRAD_F, EV_JAC_G, EV_HESS

      INTEGER :: sn, sm, snza, snzh
      COMMON / cutest_ipdims / sn, sm, snza, snzh
      sn = n
      sm = m
      snza = nza
      snzh = nzh

      IPCREATE = 1
      RETURN
      END

      SUBROUTINE IPFREE( iproblem )
      INTEGER :: iproblem
      RETURN
      END

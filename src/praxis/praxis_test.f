C     ( Last modified on 6 Jan 2013 at 12:10:00 )

C  Dummy PRAXIS etc testing praxis_main interface to CUTEst
C  Nick Gould, 6th January 2013

      SUBROUTINE PRAXIS( EVALF )
      EXTERNAL :: EVALF
      COMMON / CPRAX / V, X, D, Q0, Q1, dmin, epsmch, fx, h, qd0, qd1,
     *                 qf1, small, t, xldt, xm2, xm4, dseed, scbd, n,
     *                 nl, nf, lp, jprint, nmx, illcin, ktm, nfmax,
     *                 jranch
      INTEGER, PARAMETER :: nmax = 1000
      INTEGER :: n, nl, nf, lp, jprint, illcin, ktm, i
      INTEGER :: nfmax, jranch, nmx, status
      DOUBLE PRECISION :: dmin, epsmch, fx, h, qd0, qd1, qf1
      DOUBLE PRECISION :: small, t, xldt, xm2, xm4, dseed, scbd
      DOUBLE PRECISION, DIMENSION( nmax ) :: X, D, Q0, Q1
      DOUBLE PRECISION, DIMENSION( nmax, nmax ) :: V
      DOUBLE PRECISION :: PRAXIS_evalf
      EXTERNAL :: PRAXIS_evalf
      fx = PRAXIS_evalf( X( : n ), n )
      RETURN
      END

      SUBROUTINE RANINI( rvalue )
      DOUBLE PRECISION :: rvalue
      RETURN
      END

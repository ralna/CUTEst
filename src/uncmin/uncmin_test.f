C     ( Last modified on 5 Jan 2013 at 13:40:00 )

C  Dummy OPTIF9 for testing uncmin_main interface to CUTEst
C  Nick Gould, 5th January 2013

      SUBROUTINE OPTIF9( nr, n, X, EVALF, EVALG, EVALH, TYPSIZ,
     *             fscale, method, iexp, msg, ndigit, ilim,
     *             iagflg, iahflg, out, dlt, gradtl, stp,
     *             steptl, XPLS, fpls, GPLS, itrmcd, A, WRK )
      INTEGER :: nr, n, method, iexp, msg, ndigit, ilim, itrmcd
      INTEGER :: out, iagflg, iahflg
      DOUBLE PRECISION :: dlt, gradtl, stp, steptl, fscale, fpls
      DOUBLE PRECISION :: X( n ), TYPSIZ( n ), XPLS( n ), GPLS( n )
      DOUBLE PRECISION :: A( nr, n ), WRK( nr, 8 )
      EXTERNAL :: EVALF, EVALG, EVALH
      INTEGER :: i
      DO 10 i = 1, n
        XPLS( i ) = X( i )
   10 CONTINUE
      CALL EVALF( n, XPLS, fpls )
      CALL EVALG( n, X, GPLS )
      CALL EVALH( nr, n, XPLS, A )
      itrmcd = 4
      RETURN
      END

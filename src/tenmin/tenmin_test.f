C     ( Last modified on 5 Jan 2013 at 14:40:00 )

C  Dummy TENSOR for testing tenmin_main interface to CUTEst
C  Nick Gould, 5th January 2013

      SUBROUTINE TENSOR( nr, n, X, EVALF, EVALG, EVALH, TYPSIZ,
     *             fscale, gradtl, steptl, ilim, stepmx, out,
     *             method, iagflg, iahflg, ndigit, msg, XPLS,
     *             fpls, GPLS, H, itnno, WRK, IWRK  )
      INTEGER :: nr, n, method, msg, ndigit, ilim, itnno, out
      INTEGER ::  iagflg, iahflg
      DOUBLE PRECISION :: gradtl, steptl, fscale, fpls, stepmx
      INTEGER :: IWRK( n )
      DOUBLE PRECISION :: X( n ), TYPSIZ( n ), XPLS( n ), GPLS( n )
      DOUBLE PRECISION :: H( nr, n ), WRK( nr, 8 )
      EXTERNAL :: EVALF, EVALG, EVALH
      INTEGER :: i
      DO 10 i = 1, n
        XPLS( i ) = X( i )
   10 CONTINUE
      CALL EVALF( n, XPLS, fpls )
      CALL EVALG( n, X, GPLS )
      CALL EVALH( nr, n, XPLS, H )
      itnno = 4
      RETURN
      END

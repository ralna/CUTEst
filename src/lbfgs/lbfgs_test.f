C     ( Last modified on 4 Jan 2013 at 13:40:00 )

C  Dummy LBFGS for testing lbfgs_main interface to CUTEst
C  Nick Gould,  4th January 2013

      SUBROUTINE LBFGS( n, m, X, f, G, DIAGCO, DIAG, IPRINT, eps, xtol,
     *                  W, iflag )
      INTEGER n, m, iflag, iprint( 2 )
      DOUBLE PRECISION f, eps, xtol
      LOGICAL diagco
      DOUBLE PRECISION X( n ), G( n ), DIAG( n ), 
     *                 W( n * ( 2 * m + 1 ) + 2 * m )
      IF ( iflag == 0 ) THEN
        iflag = 1
      ELSE
        iflag = 0
      END IF
      RETURN
      END

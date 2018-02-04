C     ( Last modified on 4 Jan 2013 at 14:45:00 )

C  Dummy CGFAM for testing cgplus_main interface to CUTEst
C  Nick Gould,  4th January 2013

      SUBROUTINE CGFAM( n, X, f, G, D, GOLD, IPRINT, eps, W,
     *               iflag, irest, method, finish )
      INTEGER n, iflag, irest, method, IPRINT( 2 )
      DOUBLE PRECISION f, eps
      LOGICAL finish
      DOUBLE PRECISION X( n ), G( n ), D( n ), GOLD( n ), W( n )

      iflag = iflag + 1
      RETURN
      END

C     ( Last modified on 5 Jan 2013 at 11:00:00 )

C  Dummy VF13AD for testing vf13_main interface to CUTEst
C  Nick Gould,  4th January 2013

      SUBROUTINE VF13AD( n, m, meq, X, f, G, C, CN, lcn, maxfun,
     *                  acc, iprint, inf, W, lw, IW )
      INTEGER :: n, m, meq, lcn, maxfun, iprint, inf, lw
      DOUBLE PRECISION :: f, acc
      INTEGER :: IW( n + 1 )
      DOUBLE PRECISION :: X( n ), G( n ), C( m ), CN( lcn, m )
      DOUBLE PRECISION :: W( lw )

      IF ( inf .EQ. - 1 ) THEN
        inf = 0
      ELSE
        inf = 1
      END IF
      RETURN
      END


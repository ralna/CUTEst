C     ( Last modified on 4 Jan 2013 at 14:10:00 )

C  Dummy CG_DECSCENT for testing cg_descent_main interface to CUTEst
C  Nick Gould,  4th January 2013

      SUBROUTINE CG_DESCENT( tol, X, n, EVALF, EVALG, stat, gnorm, f,
     *                 iter, nf, ng, D, G, XTEMP, GTEMP )
      INTEGER          n, iter, nf, ng, stat
      DOUBLE PRECISION f, tol, gnorm 
      EXTERNAL         EVALF, EVALG
      DOUBLE PRECISION :: X( n ), G( n ), D( n ), XTEMP( n ), GTEMP( n )

      WRITE( 6, "( ' Calling CG_DESCENT_dummy with n = ', I0 )" ) n
      CALL EVALF( f, X, n )
      CALL EVALG( G, X, n )
      WRITE( 6, "( ' Leaving CG_DESCENT_dummy ' )" )
      RETURN
      END


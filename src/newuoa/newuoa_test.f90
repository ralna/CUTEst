!     ( Last modified on 29 Jan 2013 at 14:15:00 )

!  Dummy NEWUOA for testing newuoa_main interface to CUTEst
!  Nick Gould, 29th January 2013

      SUBROUTINE NEWUOA( n, npt, X, rhobeg, rhoend, iprint, maxfun, W )
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER :: n, npt, iprint, maxfun
      REAL( KIND = wp ) :: rhobeg, rhoend
      REAL( KIND = wp ) :: X( * ), W( * )

      REAL( KIND = wp ) :: f
      CALL CALFUN( n, X, f )

      RETURN
      END

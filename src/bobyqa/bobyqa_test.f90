!     ( Last modified on 27 Jan 2013 at 17:50:00 )

!  Dummy BOBYQA for testing bobyqa_main interface to CUTEst
!  Nick Gould, 27th January 2013

      SUBROUTINE BOBYQA( n, npt, X, X_l, X_u,                                  &
                         rhobeg, rhoend, iprint, maxfun, W )
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER :: n, npt, iprint, maxfun
      REAL( KIND = wp ) :: rhobeg, rhoend
      REAL( KIND = wp ) :: X( * ), X_l( * ), X_u( * ), W( * )

      REAL( KIND = wp ) :: f
      CALL CALFUN( n, X, f )

      RETURN
      END

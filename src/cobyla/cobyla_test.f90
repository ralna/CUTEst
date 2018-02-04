!     ( Last modified on 7 Jan 2013 at 16:30:00 )

!  Dummy COBYLA for testing cobyla_main interface to CUTEst
!  Nick Gould, 7th January 2013

      SUBROUTINE COBYLA( n, m, X, rhobeg, rhoend, iprint, maxfun, W, IW )
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER :: n, m, iprint, maxfun
      REAL( KIND = wp ) :: rhobeg, rhoend
      INTEGER :: IW( * )
      REAL( KIND = wp ) :: X( * ), W( * )

      REAL( KIND = wp ) :: f
      REAL( KIND = wp ) :: C( m )
      CALL CALCFC( n, m, X, f, C )

      RETURN
      END

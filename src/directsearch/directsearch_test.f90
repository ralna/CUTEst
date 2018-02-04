!     ( Last modified on 16 Feb 2013 at 15:00:00 )

!  Dummy DIRECT SEARCH codes for testing directsearch_main interface to CUTEst
!  Nick Gould, 16th February 2013

      SUBROUTINE pattrn( type, evalf, n, X, stepi, stepf, maxf, istat,         &
                         AUX, iaux )
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER :: type, n, maxf, iaux, istat
      LOGICAL :: right, stddev
      REAL( KIND = wp ) :: stepi, stepf
      REAL( KIND = wp ) :: X( n ), AUX( n + 2 )
      EXTERNAL :: evalf

      AUX( 1 ) = stepi
      CALL DIRECTSEARCH_evalf( X, AUX( 2 ), n )
      AUX( 3 : n + 2 ) = X( : n )
      istat = 1

      RETURN
      END

      SUBROUTINE shh( evalf, n, X, right, SSTEPI, stepf, maxf, stddev, istat,  &
                      AUX, iaux )
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER :: type, n, maxf, iaux, istat
      LOGICAL :: right, stddev
      REAL( KIND = wp ) :: stepf
      REAL( KIND = wp ) :: X( n ), SSTEPI( n ), AUX( n + 2 )
      EXTERNAL :: evalf

      AUX( 1 ) = SSTEPI( 1 )
      CALL DIRECTSEARCH_evalf( X, AUX( 2 ), n )
      AUX( 3 : n + 2 ) = X( : n )
      istat = 1

      RETURN
      END

      SUBROUTINE nm( evalf, n, X, right, sigma, alpha, beta, gamma, SSTEPI,    &
                     stepf, maxf, stddev, istat, AUX, iaux )
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER :: type, n, maxf, iaux, istat
      LOGICAL :: right, stddev
      REAL( KIND = wp ) :: sigma, alpha, beta, gamma, stepf
      REAL( KIND = wp ) :: X( n ), SSTEPI( n ), AUX( n + 2 )
      EXTERNAL :: evalf

      AUX( 1 ) = SSTEPI( 1 )
      CALL DIRECTSEARCH_evalf( X, AUX( 2 ), n )
      AUX( 3 : n + 2 ) = X( : n )
      istat = 1

      RETURN
      END

      SUBROUTINE smd( evalf, n, X, right, SSTEPI, stepf, maxf, stddev, istat,  &
                      AUX, iaux )
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER :: type, n, maxf, iaux, istat
      LOGICAL :: right, stddev
      REAL( KIND = wp ) :: stepf
      REAL( KIND = wp ) :: X( n ), SSTEPI( n ), AUX( n + 2 )
      EXTERNAL :: evalf

      AUX( 1 ) = SSTEPI( 1 )
      CALL DIRECTSEARCH_evalf( X, AUX( 2 ), n )
      AUX( 3 : n + 2 ) = X( : n )
      istat = 1

      RETURN
      END

! THIS VERSION: CUTEST 2.2 - 2023-11-22 AT 12:45 GMT.

#include "cutest_modules.h"

!  Dummy DIRECT SEARCH codes for testing directsearch_main interface to CUTEst
!  Nick Gould, 16th February 2013

      SUBROUTINE pattrn( type, evalf, n, X, stepi, stepf, maxf, istat,         &
                         AUX, iaux )
      USE CUTEST_KINDS_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ) :: type, n, maxf, iaux, istat
      LOGICAL :: right, stddev
      REAL( KIND = rp_ ) :: stepi, stepf
      REAL( KIND = rp_ ) :: X( n ), AUX( n + 2 )
      EXTERNAL :: evalf

      AUX( 1 ) = stepi
      CALL evalf( X, AUX( 2 ), n )
      AUX( 3 : n + 2 ) = X( : n )
      istat = 1

      RETURN
      END

      SUBROUTINE shh( evalf, n, X, right, SSTEPI, stepf, maxf, stddev, istat,  &
                      AUX, iaux )
      USE CUTEST_KINDS_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ) :: type, n, maxf, iaux, istat
      LOGICAL :: right, stddev
      REAL( KIND = rp_ ) :: stepf
      REAL( KIND = rp_ ) :: X( n ), SSTEPI( n ), AUX( n + 2 )
      EXTERNAL :: evalf

      AUX( 1 ) = SSTEPI( 1 )
      CALL evalf( X, AUX( 2 ), n )
      AUX( 3 : n + 2 ) = X( : n )
      istat = 1

      RETURN
      END

      SUBROUTINE nm( evalf, n, X, right, sigma, alpha, beta, gamma, SSTEPI,    &
                     stepf, maxf, stddev, istat, AUX, iaux )
      USE CUTEST_KINDS_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ) :: type, n, maxf, iaux, istat
      LOGICAL :: right, stddev
      REAL( KIND = rp_ ) :: sigma, alpha, beta, gamma, stepf
      REAL( KIND = rp_ ) :: X( n ), SSTEPI( n ), AUX( n + 2 )
      EXTERNAL :: evalf

      AUX( 1 ) = SSTEPI( 1 )
      CALL evalf( X, AUX( 2 ), n )
      AUX( 3 : n + 2 ) = X( : n )
      istat = 1

      RETURN
      END

      SUBROUTINE smd( evalf, n, X, right, SSTEPI, stepf, maxf, stddev, istat,  &
                      AUX, iaux )
      USE CUTEST_KINDS_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ) :: type, n, maxf, iaux, istat
      LOGICAL :: right, stddev
      REAL( KIND = rp_ ) :: stepf
      REAL( KIND = rp_ ) :: X( n ), SSTEPI( n ), AUX( n + 2 )
      EXTERNAL :: evalf

      AUX( 1 ) = SSTEPI( 1 )
      CALL evalf( X, AUX( 2 ), n )
      AUX( 3 : n + 2 ) = X( : n )
      istat = 1

      RETURN
      END

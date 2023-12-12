C  THIS VERSION: CUTEST 2.2 - 2023-11-29 AT 15:00 GMT.

#include "cutest_modules.h"

C  Dummy SETULB for testing lbfgsb_main interface to CUTEst
C  Nick Gould,  4th January 2013

      SUBROUTINE SETULB( n, m, X, XL, XU, NBD, f, G, factr, pgtol, WA,
     *                   IWA, task, iprint, csave, LSAVE, ISAVE, DSAVE )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) n, m, iprint, ISAVE( 44 )
      REAL ( KIND = rp_ ) f, factr, pgtol, DSAVE( 29 )
      CHARACTER ( LEN = 60 ) :: TASK, CSAVE
      LOGICAL LSAVE( 4 )
      INTEGER ( KIND = ip_ ) :: NBD( n ), IWA( 3 * n )
      REAL ( KIND = rp_ ) X( n ), XL( n ), XU( n ), G( n ),
     *  WA( 2 * m * n + 5 * n + 11 * m * m + 8 * m )

      IF ( TASK( 1: 5 ) .EQ. 'START' ) THEN
        TASK( 1: 5 ) = 'FG   '
      ELSE
        TASK( 1: 5 ) = 'CONV '
      END IF
      DSAVE( 13 ) = 0.0_rp_
      RETURN
      END

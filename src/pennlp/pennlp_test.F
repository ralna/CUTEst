! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!  Dummy PENNLPF for testing pennlp_main interface to CUTEst

!  Nick Gould, 28th February 2013

      SUBROUTINE PENNLPF( n, m_lin, m, nnzg, nnzh,
     &                    X_l, X_u, C_l, C_u, X, Y,
     &                    PENNON_evalof, PENNON_evalog, PENNON_evaloh,
     &                    PENNON_evalcf, PENNON_evalcg, PENNON_evalch,
     &                    IOPTIONS, DOPTIONS, IRESULTS, DRESULTS,
     &                    status )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: n, m_lin, m, nnzg, nnzh, status
      INTEGER ( KIND = ip_ ) :: IOPTIONS( 17 ), IRESULTS( 4 )
      REAL ( KIND = rp_ ), DIMENSION( n ) :: X, X_l, X_u
      REAL ( KIND = rp_ ), DIMENSION( m ) :: Y, C_l, C_u
      REAL ( KIND = rp_ ) :: DOPTIONS( 13 ), DRESULTS( 5 )
      EXTERNAL :: PENNON_evalof, PENNON_evalog, PENNON_evaloh
      EXTERNAL :: PENNON_evalcf, PENNON_evalcg, PENNON_evalch
C  automatic arrays
      INTEGER ( KIND = ip_ ) :: G_var( nnzg )
      INTEGER ( KIND = ip_ ) :: H_row( nnzh ), H_col( nnzh )
      REAL ( KIND = rp_ ) :: G_val( nnzg ), H_val( nnzh )
C  trial calls
      CALL PENNON_evalof( X, DRESULTS( 1 ) )
      CALL PENNON_evalog( X, nnzg, G_var, G_val )
      CALL PENNON_evaloh( X, nnzh, H_row, H_col, H_val )
      CALL PENNON_evalcf( 0, X, DRESULTS( 3 ) )
      CALL PENNON_evalcg( 0, X, nnzg, G_var, G_val )
      CALL PENNON_evalch( 0, X, nnzh, H_row, H_col, H_val )
      IRESULTS( 1 ) = 0
      IRESULTS( 2 ) = 0
      IRESULTS( 3 ) = 0
      IRESULTS( 4 ) = 0
      DRESULTS( 2 ) = 1.0_rp_
      DRESULTS( 4 ) = Y( 1 ) * DRESULTS( 3 )
      DRESULTS( 3 ) = ABS( DRESULTS( 3 ) )
      DRESULTS( 5 ) = 1.0_rp_
      status = 5
      RETURN
      END

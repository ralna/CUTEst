!     ( Last modified on 28 Feb 2013 at 15:50:00 )

!  Dummy PENNLPF for testing pennlp_main interface to CUTEst
!  Nick Gould, 28th February 2013

      SUBROUTINE PENNLPF( n, m_lin, m, nnzg, nnzh,
     &                    X_l, X_u, C_l, C_u, X, Y,
     &                    PENNON_evalof, PENNON_evalog, PENNON_evaloh,
     &                    PENNON_evalcf, PENNON_evalcg, PENNON_evalch,
     &                    IOPTIONS, DOPTIONS, IRESULTS, DRESULTS,
     &                    status )
      INTEGER :: n, m_lin, m, nnzg, nnzh, status
      INTEGER :: IOPTIONS( 17 ), IRESULTS( 4 )
      DOUBLE PRECISION, DIMENSION( n ) :: X, X_l, X_u
      DOUBLE PRECISION, DIMENSION( m ) :: Y, C_l, C_u
      DOUBLE PRECISION :: DOPTIONS( 13 ), DRESULTS( 5 )
      EXTERNAL :: PENNON_evalof, PENNON_evalog, PENNON_evaloh
      EXTERNAL :: PENNON_evalcf, PENNON_evalcg, PENNON_evalch
C  local variables
      DOUBLE PRECISION :: c
C  automatic arrays
      INTEGER :: G_var( nnzg ), H_row( nnzh ), H_col( nnzh )
      DOUBLE PRECISION :: G_val( nnzg ), H_val( nnzh )
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
      DRESULTS( 2 ) = 1.0D0
      DRESULTS( 4 ) = Y( 1 ) * DRESULTS( 3 )
      DRESULTS( 3 ) = ABS( DRESULTS( 3 ) )
      DRESULTS( 5 ) = 1.0D0
      status = 5
      RETURN
      END
